tweets <- data.frame(status_id = c(1234, 5678),
                     text = c("I tweet about one thing! #onething #things",
                              "I tweet about another thing!?%$ #another thing #things"),
                     stringsAsFactors = FALSE) %>%
  feature_orthographic(status_id, text)

boring_tweet <- data.frame(status_id = 123,
                           text = "this is a boring tweet",
                           stringsAsFactors = FALSE) %>%
  feature_orthographic(status_id, text)


test_that("orthographic dimensions are expected", {
  expect_equal(dim(tweets), c(2L, 5L))
  expect_equal(dim(boring_tweet), c(1L, 1L))
})

test_that("orthographic function doesn't return NAs", {
  expect_equal(sum(is.na(tweets)),0)
  expect_equal(sum(is.na(boring_tweet)), 0)
})
