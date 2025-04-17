test_that("discussion_items_to_yaml works with basic items", {
  # Create sample discussion items
  items <- list(
    list(body = "Main post", user_name = "user1"),
    list(body = "First comment", user_name = "user2"),
    list(body = "Second comment", user_name = "user3")
  )
  
  # Convert to YAML
  yaml_output <- discussion_items_to_yaml(items)
  
  # Parse back to verify structure
  parsed <- yaml::yaml.load(yaml_output)
  
  # Check structure
  expect_true(is.list(parsed))
  expect_equal(length(parsed), 3)
  expect_equal(parsed[[1]]$body, "Main post")
  expect_equal(parsed[[1]]$user_name, "user1")
  expect_equal(parsed[[2]]$body, "First comment")
  expect_equal(parsed[[3]]$user_name, "user3")
})
