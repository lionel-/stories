#' Make stories from your histories
#'
#' @name stories
#' @import ellmer
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
story <- function(
  file,
  fun = NULL,
  path = ".",
  model = "gpt-4.1",
  echo = TRUE
) {
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

Mention relevant contributors (with clickable links to their Github
profiles if any) for each change you describe.

Please present your response as a markdown document. Do not include any
additional commentary or explanations outside of the requested analysis.
Make sure _every_ github references that you mention are clickable links.
Do not wrap the markdown in ```md ``` fences, the output should be directly
usable in a .md file.

Please add a YAML frontmatter at the start of the document like so (replacing
TITLE by the real title):
---
title: \"TITLE\"
output: html_document
---
",
    repo_info$owner,
    repo_info$repo,
    if (!is.null(fun)) "`git log -L`" else "`git log --follow`",
    if (!is.null(fun)) sprintf("a specific function (%s)", fun) else
      sprintf("the file %s", file),
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
  chat$chat("", echo = echo)
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

#' Generate and view a story about the current file in RStudio
#'
#' This function uses rstudioapi to determine the current file being edited,
#' generates a story about its development history, renders it to HTML,
#' and displays it in the RStudio viewer pane.
#'
#' @param fun Optional name of a function to analyze within the current file (default: NULL)
#' @param model The OpenAI model to use (default: "gpt-4.1")
#' @return Invisibly returns the path to the generated HTML file
#' @export
view_story <- function(fun = NULL, model = "gpt-4.1") {
  # Check if running in RStudio
  if (!rstudioapi::isAvailable()) {
    stop("This function requires RStudio to be running")
  }

  # Get the current document context
  context <- rstudioapi::getActiveDocumentContext()

  # Get the file path of the current document
  file_path <- context$path
  if (file_path == "" || is.null(file_path)) {
    stop("The current document must be saved before generating a story")
  }

  # Get the project root directory using rstudioapi
  project_root <- rstudioapi::getActiveProject()
  if (is.null(project_root)) {
    stop("This function requires an RStudio project")
  }

  # Get the file path relative to the project root
  # First, normalize paths to ensure consistent format
  norm_file_path <- normalizePath(file_path, winslash = "/")
  norm_project_root <- normalizePath(project_root, winslash = "/")

  # Remove the project root from the file path to get the relative path
  relative_path <- sub(paste0("^", norm_project_root, "/?"), "", norm_file_path)

  # Create a temporary file for the markdown output
  md_file <- tempfile(fileext = ".md")

  # Redirect the story output to the markdown file
  md_content <- story(
    file = relative_path,
    fun = fun,
    path = project_root,
    model = model,
    echo = FALSE
  )

  # Write the markdown content to the file
  writeLines(md_content, md_file)

  # Create a temporary file for the HTML output
  html_file <- tempfile(fileext = ".html")

  # Check if rmarkdown is installed
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is needed to render HTML. Please install it.")
  }

  # Render the markdown to HTML
  rmarkdown::render(
    input = md_file,
    output_file = html_file,
    output_format = "html_document",
    quiet = TRUE
  )

  # View the HTML in the RStudio viewer
  rstudioapi::viewer(html_file)

  # Return the path to the HTML file invisibly
  invisible(html_file)
}
