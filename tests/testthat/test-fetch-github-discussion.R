test_that("fetch_github_discussion handles issues correctly", {
  # Skip if no GitHub token is available
  skip_if_not(Sys.getenv("GITHUB_PAT") != "", "No GitHub token available")
  
  # Use a real GitHub issue
  result <- fetch_github_discussion("r-lib/gh#27")
  
  # Basic structure checks
  expect_equal(result$number, 27)
  expect_true(!is.null(result$title))
  expect_true(!is.null(result$body))
  expect_true(is.list(result$comments))
})

test_that("fetch_github_discussion handles PRs correctly", {
  # Skip if no GitHub token is available
  skip_if_not(Sys.getenv("GITHUB_PAT") != "", "No GitHub token available")
  
  # Use a real GitHub PR
  result <- fetch_github_discussion("r-lib/gh#56")
  
  # Basic structure checks
  expect_equal(result$number, 56)
  expect_true(!is.null(result$title))
  expect_true(!is.null(result$body))
  expect_true(is.list(result$comments))
  expect_true(is.list(result$review_comments))
})
