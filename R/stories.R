#' Make stories from your histories
#'
#' @name stories
"_PACKAGE"

#' Generate a story about the development history of a function
#'
#' This function uses git log and AI to create a narrative about how a specific
#' function evolved over time.
#'
#' @param path Path to the git repository
#' @param fun Name of the function to analyze
#' @param file Path to the file containing the function, relative to the repo root
#' @param model The OpenAI model to use (default: "gpt-4.1")
#' @return A character string containing the AI-generated story
#' @export
stories <- function(path, fun, file, model = "gpt-4.1") {
  # Save current working directory and restore on exit
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  # Change to the repository directory
  setwd(path)

  # Create the git log target
  target <- sprintf(":%s:%s", fun, file)

  # Get the git log for the function
  log <- system2("git", c("log", "-L", target), stdout = TRUE)
  log <- c("```", log, "```")
  log <- paste(log, collapse = "\n")

  # Create the prompt for the AI
  prompt <- "
I will provide you with the output of `git log -L` for a specific function. Your
task is to analyze the development history of the function and summarize it in a
clear and insightful way. Focus on identifying the goals of the authors, any
challenges they faced, changes in their approach, and any other noteworthy
details that would be interesting or useful to engineers or end users. Avoid
making trivial observations or overanalyzing unnecessarily—just provide
thoughtful, concise insights.

Please present your response as a markdown document. Do not include any
additional commentary or explanations outside of the requested analysis.
"

  # Use the AI to generate the story
  chat <- ellmer::chat_openai(
    model = model,
    system_prompt = c(prompt, log)
  )

  # Get the AI's response
  chat$chat("")
}
