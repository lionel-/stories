test_that("extract_discussion_items works with issues", {
  # Create a mock issue discussion
  mock_discussion <- list(
    body = "Issue description",
    user = list(login = "user1"),
    created_at = "2023-01-01T12:00:00Z",
    comments = list(
      list(body = "Comment 1", user = list(login = "user2"), created_at = "2023-01-02T12:00:00Z"),
      list(body = "Comment 2", user = list(login = "user3"), created_at = "2023-01-03T12:00:00Z")
    ),
    review_comments = list()
  )
  
  # Extract items
  items <- extract_discussion_items(mock_discussion)
  
  # Check results
  expect_equal(length(items), 3)
  expect_equal(items[[1]]$body, "Issue description")
  expect_equal(items[[1]]$user_name, "user1")
  expect_equal(items[[1]]$created_at, "2023-01-01T12:00:00Z")
  expect_equal(items[[2]]$body, "Comment 1")
  expect_equal(items[[2]]$user_name, "user2")
  expect_equal(items[[2]]$created_at, "2023-01-02T12:00:00Z")
  expect_equal(items[[3]]$body, "Comment 2")
  expect_equal(items[[3]]$user_name, "user3")
  expect_equal(items[[3]]$created_at, "2023-01-03T12:00:00Z")
})

test_that("extract_discussion_items works with PRs", {
  # Create a mock PR discussion
  mock_discussion <- list(
    body = "PR description",
    user = list(login = "user1"),
    created_at = "2023-01-01T12:00:00Z",
    comments = list(
      list(body = "Comment 1", user = list(login = "user2"), created_at = "2023-01-02T12:00:00Z")
    ),
    review_comments = list(
      list(body = "Review comment", user = list(login = "user3"), created_at = "2023-01-03T12:00:00Z")
    )
  )
  
  # Extract items
  items <- extract_discussion_items(mock_discussion)
  
  # Check results
  expect_equal(length(items), 3)
  expect_equal(items[[1]]$body, "PR description")
  expect_equal(items[[1]]$user_name, "user1")
  expect_equal(items[[1]]$created_at, "2023-01-01T12:00:00Z")
  expect_equal(items[[2]]$body, "Comment 1")
  expect_equal(items[[2]]$user_name, "user2")
  expect_equal(items[[2]]$created_at, "2023-01-02T12:00:00Z")
  expect_equal(items[[3]]$body, "Review comment")
  expect_equal(items[[3]]$user_name, "user3")
  expect_equal(items[[3]]$created_at, "2023-01-03T12:00:00Z")
})

test_that("extract_discussion_items handles empty comments", {
  # Create a mock discussion with no comments
  mock_discussion <- list(
    body = "Description",
    user = list(login = "user1"),
    created_at = "2023-01-01T12:00:00Z",
    comments = list(),
    review_comments = list()
  )
  
  # Extract items
  items <- extract_discussion_items(mock_discussion)
  
  # Check results
  expect_equal(length(items), 1)
  expect_equal(items[[1]]$body, "Description")
  expect_equal(items[[1]]$user_name, "user1")
  expect_equal(items[[1]]$created_at, "2023-01-01T12:00:00Z")
})
