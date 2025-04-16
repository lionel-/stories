test_that("fetch_github_discussion handles issues correctly", {
  # Check for GitHub token in multiple environment variables
  has_token <- Sys.getenv("GITHUB_PAT") != "" || 
               Sys.getenv("GITHUB_TOKEN") != "" || 
               file.exists("~/.github/token")
  
  skip_if_not(has_token, "No GitHub token available")

  # Use a real GitHub issue
  result <- fetch_github_discussion("r-lib/gh#27")

  # Basic structure checks
  expect_equal(result$number, 27)
  expect_true(!is.null(result$title))
  expect_true(!is.null(result$body))
  expect_true(is.list(result$comments))
})

test_that("fetch_github_discussion handles PRs correctly", {
  # Check for GitHub token in multiple environment variables
  has_token <- Sys.getenv("GITHUB_PAT") != "" || 
               Sys.getenv("GITHUB_TOKEN") != "" || 
               file.exists("~/.github/token")
  
  skip_if_not(has_token, "No GitHub token available")

  # Use a real GitHub PR
  result <- fetch_github_discussion("r-lib/gh#56")

  # Basic structure checks
  expect_equal(result$number, 56)
  expect_true(!is.null(result$title))
  expect_true(!is.null(result$body))
  expect_true(is.list(result$comments))
  expect_true(is.list(result$review_comments))
})
