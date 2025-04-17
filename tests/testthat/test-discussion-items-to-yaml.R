test_that("discussion_items works with GitHub references", {
  # Check for GitHub token in multiple environment variables
  has_token <- Sys.getenv("GITHUB_PAT") != "" || 
               Sys.getenv("GITHUB_TOKEN") != "" || 
               file.exists("~/.github/token")
  
  skip_if_not(has_token, "No GitHub token available")
  
  # Use a real GitHub issue with a small number of comments
  yaml_output <- discussion_items("r-lib/gh#27")
  
  # Basic validation of the output
  expect_true(is.character(yaml_output))
  expect_true(nchar(yaml_output) > 0)
  
  # Parse the YAML to verify structure
  parsed <- yaml::yaml.load(yaml_output)
  
  # Check that we have at least the original post
  expect_true(is.list(parsed))
  expect_true(length(parsed) >= 1)
  expect_true(!is.null(parsed[[1]]$body))
  expect_true(!is.null(parsed[[1]]$user_name))
})
