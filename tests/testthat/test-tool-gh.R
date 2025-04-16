test_that("parse_github_reference works with owner/repo#number format", {
  ref <- "owner/repo#123"
  result <- parse_github_reference(ref)
  
  expect_equal(result$owner, "owner")
  expect_equal(result$repo, "repo")
  expect_equal(result$number, 123)
})

test_that("parse_github_reference works with GitHub URL format", {
  ref <- "https://github.com/owner/repo/issues/123"
  result <- parse_github_reference(ref)
  
  expect_equal(result$owner, "owner")
  expect_equal(result$repo, "repo")
  expect_equal(result$number, 123)
  
  # Test PR URL
  ref <- "https://github.com/owner/repo/pull/456"
  result <- parse_github_reference(ref)
  
  expect_equal(result$owner, "owner")
  expect_equal(result$repo, "repo")
  expect_equal(result$number, 456)
})

test_that("parse_github_reference throws error for invalid formats", {
  expect_error(parse_github_reference("invalid"), "Invalid reference format")
  expect_error(parse_github_reference("https://github.com/owner/repo/invalid/123"), "Invalid GitHub URL format")
})
