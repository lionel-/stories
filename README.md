# stories

> Make stories from your histories

The `stories` package uses AI to make sense of git histories, generating narrative summaries of how files and functions have evolved over time. It helps developers understand the context, design decisions, and evolution of code by analyzing git commit history and presenting it in a readable format.

## Installation

You can install the development version of stories from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("lionel-/stories")
```

## Usage

The package provides two main functions:

### `story()`

Generate a story about the development history of a file or function:

```r
# Get the story for a file
story("R/important_file.R")

# Get the story for a specific function
story("R/important_file.R", fun = "important_function")

# Specify a different repository path
story("src/main.cpp", path = "~/projects/my-project")

# Use a different model
story("R/important_file.R", model = "gpt-4o")
```

### `view_story()`

In RStudio, generate and view a story about the current file being edited:

```r
# View the story for the current file
view_story()

# View the story for a specific function in the current file
view_story(fun = "important_function")
```

This function will:
1. Determine the current file being edited
2. Generate a story about its development history
3. Render it to HTML
4. Display it in the RStudio viewer pane

## How it works

The package:
1. Uses git commands to extract the commit history for a file or function
2. Retrieves GitHub repository information
3. Sends the git log to an AI model along with a carefully crafted prompt
4. Processes the AI's response into a readable narrative
5. Optionally renders the output as HTML for viewing in RStudio

## Dependencies

- `ellmer`: For AI model interaction
- `gh`: For GitHub API interactions
- `rstudioapi`: For RStudio integration
- `rmarkdown`: For rendering HTML output (suggested)

## Development

This package was vibe-coded with [aider](https://github.com/paul-gauthier/aider), an AI pair programming tool. The development process involved iterative refinement of the code with AI assistance, focusing on creating a clean, user-friendly interface for exploring code history.

## License

MIT
