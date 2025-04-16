test_that("fetch_github_discussion handles issues correctly", {
  # Mock the gh::gh function to avoid actual API calls
  mockery::stub(fetch_issue, "gh::gh", list(
    id = 123,
    number = 42,
    title = "Test Issue",
    body = "This is a test issue"
  ))
  
  mockery::stub(fetch_all_comments, "gh::gh", list())
  
  # Mock the parse_github_reference function
  mockery::stub(fetch_github_discussion, "parse_github_reference", 
                list(owner = "owner", repo = "repo", number = 42))
  
  # Mock fetch_pull_request to fail so it falls back to fetch_issue
  mockery::stub(fetch_github_discussion, "fetch_pull_request", 
                function(...) stop("Not a PR"))
  
  # Call the function
  result <- fetch_github_discussion("owner/repo#42")
  
  # Verify the result
  expect_equal(result$number, 42)
  expect_equal(result$title, "Test Issue")
  expect_equal(result$body, "This is a test issue")
  expect_true(is.list(result$comments))
})

test_that("fetch_github_discussion handles PRs correctly", {
  # Mock the gh::gh function to avoid actual API calls
  mockery::stub(fetch_pull_request, "gh::gh", list(
    id = 456,
    number = 99,
    title = "Test PR",
    body = "This is a test PR"
  ))
  
  mockery::stub(fetch_all_comments, "gh::gh", list())
  mockery::stub(fetch_all_review_comments, "gh::gh", list())
  
  # Mock the parse_github_reference function
  mockery::stub(fetch_github_discussion, "parse_github_reference", 
                list(owner = "owner", repo = "repo", number = 99))
  
  # Call the function
  result <- fetch_github_discussion("owner/repo#99")
  
  # Verify the result
  expect_equal(result$number, 99)
  expect_equal(result$title, "Test PR")
  expect_equal(result$body, "This is a test PR")
  expect_true(is.list(result$comments))
  expect_true(is.list(result$review_comments))
})
