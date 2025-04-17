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

The git history of either a file or a function is retrieved using `git -p` or `git -L`. This history is summarised by an LLM.

- You need a local git repository.
- If your `origin` remote points to a github repo, you'll get better results. Links to Github issues and PRs are fetched and added as context. The LLM is free to open more links if it finds new ones that seem interesting.

The package provides two main functions:

### `view_story()`

In RStudio or Positron, generate and view a story about the current file being edited:

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
4. Display it in the RStudio or Positron viewer pane


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

## Dependencies

- `ellmer`: For AI model interaction
- `gh`: For GitHub API interactions
- `rstudioapi`: For RStudio and Positron integration
- `rmarkdown`: For rendering HTML output (suggested)

## Development

This package is an experiment and was "vibe-coded" with [aider](https://github.com/paul-gauthier/aider), an AI pair programming tool.

## License

MIT
