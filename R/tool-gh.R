#' Fetch a GitHub issue or PR and all its discussion
#'
#' @param reference GitHub reference in the format 'owner/repo#number' or a full URL
#' @return A list containing the issue/PR data along with all comments
#' @export
#' @importFrom gh gh
fetch_github_discussion <- function(reference) {
  # Parse the reference to extract owner, repo, and issue/PR number
  parsed_ref <- parse_github_reference(reference)
  owner <- parsed_ref$owner
  repo <- parsed_ref$repo
  number <- parsed_ref$number

  # Try to fetch as PR first, if it fails, fetch as issue
  result <- tryCatch(
    {
      list(
        item_data = fetch_pull_request(owner, repo, number),
        resource_type = "pr"
      )
    },
    error = function(e) {
      list(
        item_data = fetch_issue(owner, repo, number),
        resource_type = "issue"
      )
    }
  )
  
  item_data <- result$item_data
  resource_type <- result$resource_type

  # Fetch all comments
  comments <- fetch_all_comments(owner, repo, number, resource_type)

  # Add comments to the result
  item_data$comments <- comments

  # If it's a PR, also fetch review comments
  if (resource_type == "pr") {
    review_comments <- fetch_all_review_comments(owner, repo, number)
    item_data$review_comments <- review_comments
  } else {
    # Ensure review_comments is always a list for issues
    item_data$review_comments <- list()
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

    if (length(result) < 5) {
      stop("Invalid GitHub URL format")
    }

    return(list(
      owner = result[2],
      repo = result[3],
      number = as.integer(result[5])
    ))
  } else {
    # Handle owner/repo#number format
    pattern <- "^([^/]+)/([^#]+)#(\\d+)$"
    matches <- regexec(pattern, reference)
    result <- regmatches(reference, matches)[[1]]

    if (length(result) < 4) {
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
}


#' Fetch issue data using gh package
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param number Issue number
#' @return Issue data as list
#' @keywords internal
fetch_issue <- function(owner, repo, number) {
  gh::gh(
    "GET /repos/{owner}/{repo}/issues/{issue_number}",
    owner = owner,
    repo = repo,
    issue_number = number
  )
}

#' Fetch pull request data using gh package
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param number PR number
#' @return PR data as list
#' @keywords internal
fetch_pull_request <- function(owner, repo, number) {
  gh::gh(
    "GET /repos/{owner}/{repo}/pulls/{pull_number}",
    owner = owner,
    repo = repo,
    pull_number = number
  )
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
  comments <- gh::gh(
    "GET /repos/{owner}/{repo}/issues/{issue_number}/comments",
    owner = owner,
    repo = repo,
    issue_number = number,
    .limit = Inf
  )

  # Ensure we always return a list
  if (is.null(comments)) {
    return(list())
  }
  
  # Return the raw comments list
  comments
}

#' Fetch all review comments for a PR
#'
#' @param owner Repository owner
#' @param repo Repository name
#' @param number PR number
#' @return List of review comments
#' @keywords internal
fetch_all_review_comments <- function(owner, repo, number) {
  comments <- gh::gh(
    "GET /repos/{owner}/{repo}/pulls/{pull_number}/comments",
    owner = owner,
    repo = repo,
    pull_number = number,
    .limit = Inf
  )

  # Ensure we always return a list
  if (is.null(comments)) {
    return(list())
  }
  
  # Return the raw comments list
  comments
}

#' Extract discussion items from a GitHub issue or PR
#'
#' This function takes the result of `fetch_github_discussion` and extracts
#' all discussion items (the original post and all comments) as a list of
#' standardized items containing the body text and user name.
#'
#' @param discussion Result from `fetch_github_discussion`
#' @return A list of discussion items, each with `body` and `user_name` fields
#' @export
extract_discussion_items <- function(discussion) {
  # Start with the original post
  items <- list(
    list(
      body = discussion$body,
      user_name = discussion$user$login
    )
  )
  
  # Add regular comments
  if (length(discussion$comments) > 0) {
    comment_items <- lapply(discussion$comments, function(comment) {
      list(
        body = comment$body,
        user_name = comment$user$login
      )
    })
    items <- c(items, comment_items)
  }
  
  # Add review comments for PRs
  if (!is.null(discussion$review_comments) && length(discussion$review_comments) > 0) {
    review_comment_items <- lapply(discussion$review_comments, function(comment) {
      list(
        body = comment$body,
        user_name = comment$user$login
      )
    })
    items <- c(items, review_comment_items)
  }
  
  items
}

#' Get GitHub discussion items as YAML
#'
#' This function fetches a GitHub issue or PR discussion and returns all
#' discussion items (the original post and all comments) as YAML.
#' It's designed to be used as an AI tool to process GitHub discussions.
#'
#' @param reference GitHub reference in the format 'owner/repo#number' or a full URL
#' @return A character string containing the YAML representation of all discussion items
#' @export
#' @importFrom yaml as.yaml
discussion_items <- function(reference) {
  # Fetch the discussion
  discussion <- fetch_github_discussion(reference)
  
  # Extract the discussion items
  items <- extract_discussion_items(discussion)
  
  # Convert to YAML and return
  yaml::as.yaml(items)
}
