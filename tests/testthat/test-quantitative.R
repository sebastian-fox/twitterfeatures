tweet <- data.frame(status_id = c(1234, 5678),
                    text = c("This is a tweet. This is the second sentence.",
                             "This is another tweet"),
                    stringsAsFactors = FALSE)

expected_dimensions <- c(2L, 6L)
test_that("quantitative dimensions are expected", {
  expect_equal(dim(feature_sentiment(plain_tweet, status_id, text)),
               expected_dimensions)
  expect_equal(dim(feature_sentiment(excited_tweet, status_id, text)),
               expected_dimensions)
})

test_that("quantitative function doesn't return NAs", {
  expect_equal(sum(is.na(feature_sentiment(plain_tweet, status_id, text))),
               0)
  expect_equal(sum(is.na(feature_sentiment(excited_tweet, status_id, text))),
               0)
})
