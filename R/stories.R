#' Make stories from your histories
#'
#' @name stories
"_PACKAGE"
#' Generate a story about the development history of a function
#'
#' This function uses git log and AI to create a narrative about how a specific
#' function evolved over time.
#'
#' @param repo Path to the git repository
#' @param fun Name of the function to analyze
#' @param file Path to the file containing the function, relative to the repo root
#' @param model The OpenAI model to use (default: "gpt-4.1")
#' @return A character string containing the AI-generated story
#' @export
stories <- function(repo, fun, file, model = "gpt-4.1") {
  # Save current working directory and restore on exit
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  # Change to the repository directory
  setwd(repo)
  
  # Create the git log target
  target <- sprintf(":%s:%s", fun, file)
  
  # Get the git log for the function
  log <- system2("git", c("log", "-L", target), stdout = TRUE)
  log <- c("```", log, "```")
  log <- paste(log, collapse = "\n")
  
  # Create the prompt for the AI
  prompt <- "
    I'm going to give you the output of `git log -L` on a given function.
    Your job is to describe the development history of that function, giving
    as many insights that you can find in terms of the objectives of the author(s),
    what they struggled with, what was challenging, what necessitated a review of
    their approach, or anything that might be interesting to know from an end user
    perspective or a curious engineer viewpoint. Try to avoid trivial comments or
    falsely intellectual ones. You're not pretentious, just an astute observer
    eager to share insights but mindful of people's time.

    Do not answer me, just give me the output of your resulting work.
    A markdown document is perfect.
  "
  
  # Use the AI to generate the story
  chat <- ellmer::chat_openai(
    model = model,
    system_prompt = c(prompt, log)
  )
  
  # Get the AI's response
  chat$chat("")
}
