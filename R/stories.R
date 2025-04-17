#' Make stories from your histories
#'
#' @name stories
"_PACKAGE"

#' Generate a story about the development history of a file or function
#'
#' This function uses git log and AI to create a narrative about how a specific
#' file or function evolved over time.
#'
#' @param file Path to the file to analyze, relative to the repo root
#' @param fun Optional name of the function to analyze within the file (default: NULL)
#' @param path Path to the git repository (default: ".")
#' @param model The OpenAI model to use (default: "gpt-4.1")
#' @return A character string containing the AI-generated story
#' @export
stories <- function(file, fun = NULL, path = ".", model = "gpt-4.1") {
  # Save current working directory and restore on exit
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  # Change to the repository directory
  setwd(path)

  # Get GitHub repository information
  repo_info <- get_github_repo_info(path)

  # Get the git log based on whether a function is specified
  if (!is.null(fun)) {
    # Create the git log target for a specific function
    target <- sprintf(":%s:%s", fun, file)
    log <- system2("git", c("log", "-L", target), stdout = TRUE)
  } else {
    # Get the git log for the entire file
    log <- system2("git", c("log", "--follow", "--", file), stdout = TRUE)
  }
  log <- c("```", log, "```")
  log <- paste(log, collapse = "\n")

  # Create the prompt for the AI
  prompt <- sprintf(
    "
You're working with the %s/%s github repository.

I will provide you with the output of %s for %s. Your
task is to analyze the development history %s and summarize it in a
clear and insightful way. Focus on identifying the goals of the authors, any
challenges they faced, changes in their approach, and any other noteworthy
details that would be interesting or useful to engineers or end users. Avoid
making trivial observations or overanalyzing unnecessarily—just provide
thoughtful, concise insights.

If commit messages include references to GitHub pull requests or issues, please
retrieve the associated discussions to gather additional context. You may also
explore further linked discussions if they seem relevant, but avoid going too
far. Be curious, but stop when you have enough context to provide a meaningful
analysis.

Mention relevant contributors with clickable links to their Github profiles
if any.

Please present your response as a markdown document. Do not include any
additional commentary or explanations outside of the requested analysis.
Make sure _every_ github references that you mention are clickable links.
",
    repo_info$owner,
    repo_info$repo,
    if (!is.null(fun)) "`git log -L`" else "`git log --follow`",
    if (!is.null(fun)) sprintf("a specific function (%s)", fun) else sprintf("the file %s", file),
    if (!is.null(fun)) "of the function" else "of the file"
  )

  # Use the AI to generate the story
  chat <- ellmer::chat_openai(
    model = model,
    system_prompt = c(prompt, log)
  )

  chat$register_tool(tool(
    discussion_items,
    "Fetch discussion items from a github reference",
    ref = type_string(
      "A github reference in owner/repo format or as a github URL",
      required = TRUE
    )
  ))

  # Get the AI's response
  writeLines(chat$chat(""))
}

# Helper function to extract GitHub repo info
get_github_repo_info <- function(path) {
  # Save current working directory and restore on exit
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  # Change to the repository directory
  setwd(path)

  # Get the remote URL using git command
  remote_url <- system2(
    "git",
    c("config", "--get", "remote.origin.url"),
    stdout = TRUE
  )

  if (length(remote_url) == 0 || remote_url == "") {
    stop("No remote 'origin' found")
  }

  # Parse the GitHub URL to extract owner and repo
  if (grepl("github.com", remote_url)) {
    # Handle SSH URLs like git@github.com:owner/repo.git
    if (grepl("^git@github.com:", remote_url)) {
      pattern <- "git@github.com:([^/]+)/([^.]+)(\\.git)?$"
    } else if (grepl("^https://github.com/", remote_url)) {
      # Handle HTTPS URLs like https://github.com/owner/repo.git
      pattern <- "github.com/([^/]+)/([^/.]+)(\\.git)?$"
    } else {
      stop("Unrecognized GitHub URL format")
    }

    matches <- regexec(pattern, remote_url)
    result <- regmatches(remote_url, matches)[[1]]

    if (length(result) < 3) {
      stop("Could not parse GitHub repository information from remote URL")
    }

    return(list(
      owner = result[2],
      repo = result[3]
    ))
  } else {
    stop("Remote URL does not appear to be a GitHub repository")
  }
}
