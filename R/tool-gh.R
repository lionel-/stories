#' Fetch a GitHub issue or PR and all its discussion
#'
#' @param reference GitHub reference in the format 'owner/repo#number' or a full URL
#' @return A list containing the issue/PR data along with all comments
#' @export
fetch_github_discussion <- function(reference) {
  # Parse the reference to extract owner, repo, and issue/PR number
  parsed_ref <- parse_github_reference(reference)
  owner <- parsed_ref$owner
  repo <- parsed_ref$repo
  number <- parsed_ref$number

  # Determine if it's an issue or PR
  resource_type <- get_resource_type(owner, repo, number)

  # Fetch the issue/PR details
  if (resource_type == "issue") {
    item_data <- fetch_issue(owner, repo, number)
  } else {
    # PR
    item_data <- fetch_pull_request(owner, repo, number)
  }

  # Fetch all comments
  comments <- fetch_all_comments(owner, repo, number, resource_type)

  # Add comments to the result
  item_data$comments <- comments

  # If it's a PR, also fetch review comments
  if (resource_type == "pr") {
    review_comments <- fetch_all_review_comments(owner, repo, number)
    item_data$review_comments <- review_comments
  }

  item_data
}

#' Parse a GitHub reference into its components
#'
#' @param reference GitHub reference in the format 'owner/repo#number' or a full URL
#' @return A list containing owner, repo, and number
#' @keywords internal
parse_github_reference <- function(reference) {
  # Handle full URLs
  if (grepl("^https?://github\\.com/", reference)) {
    # Extract owner/repo/issue or PR number from URL
    pattern <- "github\\.com/([^/]+)/([^/]+)/(issues|pull)/(\\d+)"
    matches <- regexec(pattern, reference)
    result <- regmatches(reference, matches)[[1]]

    if (length(result) == 0) {
      stop("Invalid GitHub URL format")
    }

    list(
      owner = result[2],
      repo = result[3],
      number = as.integer(result[5])
    )
  }

  # Handle owner/repo#number format
  pattern <- "^([^/]+)/([^#]+)#(\\d+)$"
  matches <- regexec(pattern, reference)
  result <- regmatches(reference, matches)[[1]]

  if (length(result) == 0) {
    stop(
      "Invalid reference format. Expected 'owner/repo#number' or GitHub URL"
    )
  }

  list(
    owner = result[2],
    repo = result[3],
    number = as.integer(result[4])
  )
}

#' Determine if a GitHub resource is an issue or PR
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param number Issue or PR number
#' @return String "issue" or "pr"
#' @keywords internal
get_resource_type <- function(owner, repo, number) {
  # Run gh api to check if it's a PR
  cmd <- sprintf('gh api repos/%s/%s/pulls/%d --silent', owner, repo, number)
  result <- tryCatch(
    {
      system(cmd, intern = TRUE)
      "pr"
    },
    error = function(e) {
      "issue"
    }
  )

  result
}

#' Fetch issue data using gh CLI
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param number Issue number
#' @return Issue data as list
#' @keywords internal
fetch_issue <- function(owner, repo, number) {
  cmd <- sprintf('gh api repos/%s/%s/issues/%d', owner, repo, number)
  result <- system(cmd, intern = TRUE)
  jsonlite::fromJSON(paste(result, collapse = ""))
}

#' Fetch pull request data using gh CLI
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param number PR number
#' @return PR data as list
#' @keywords internal
fetch_pull_request <- function(owner, repo, number) {
  cmd <- sprintf('gh api repos/%s/%s/pulls/%d', owner, repo, number)
  result <- system(cmd, intern = TRUE)
  jsonlite::fromJSON(paste(result, collapse = ""))
}

#' Fetch all comments for an issue or PR
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param number Issue or PR number
#' @param resource_type Either "issue" or "pr"
#' @return List of comments
#' @keywords internal
fetch_all_comments <- function(owner, repo, number, resource_type) {
  all_comments <- list()
  page <- 1
  per_page <- 100

  repeat {
    cmd <- sprintf(
      'gh api repos/%s/%s/issues/%d/comments?per_page=%d&page=%d',
      owner,
      repo,
      number,
      per_page,
      page
    )
    result <- system(cmd, intern = TRUE)
    comments <- jsonlite::fromJSON(paste(result, collapse = ""))

    if (length(comments) == 0) {
      break
    }

    all_comments <- c(all_comments, list(comments))

    if (length(comments) < per_page) {
      break
    }

    page <- page + 1
  }

  # Flatten the list of comments
  do.call(rbind, all_comments)
}

#' Fetch all review comments for a PR
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param number PR number
#' @return List of review comments
#' @keywords internal
fetch_all_review_comments <- function(owner, repo, number) {
  all_comments <- list()
  page <- 1
  per_page <- 100

  repeat {
    cmd <- sprintf(
      'gh api repos/%s/%s/pulls/%d/comments?per_page=%d&page=%d',
      owner,
      repo,
      number,
      per_page,
      page
    )
    result <- system(cmd, intern = TRUE)
    comments <- jsonlite::fromJSON(paste(result, collapse = ""))

    if (length(comments) == 0) {
      break
    }

    all_comments <- c(all_comments, list(comments))

    if (length(comments) < per_page) {
      break
    }

    page <- page + 1
  }

  # Flatten the list of comments
  do.call(rbind, all_comments)
}
