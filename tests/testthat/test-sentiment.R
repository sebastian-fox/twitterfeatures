plain_tweet <- data.frame(status_id = c(1234, 5678),
                          text = c("This is a tweet",
                                   "This is another tweet"),
                          stringsAsFactors = FALSE)

excited_tweet <- data.frame(status_id = c(1234, 5678),
                            text = c("Everything is great, happy and fantastic",
                                     "Bored, sad, tired, grumpy"),
                            stringsAsFactors = FALSE)

mixed_tweets <- data.frame(status_id = c(1234, 5678),
                           text = c("This is a tweet",
                                    "This appreciate a fantastic brilliant tweet"),
                           stringsAsFactors = FALSE)


expected_dimensions <- c(2L, 7L)
test_that("sentiment dimensions are expected", {
  expect_equal(dim(feature_sentiment(plain_tweet, status_id, text)),
               expected_dimensions)
  expect_equal(dim(feature_sentiment(excited_tweet, status_id, text)),
               expected_dimensions)
  expect_equal(dim(feature_sentiment(mixed_tweets, status_id, text)),
               expected_dimensions)
})

test_that("sentiment function doesn't return NAs", {
  expect_equal(sum(is.na(feature_sentiment(plain_tweet, status_id, text))),
               0)
  expect_equal(sum(is.na(feature_sentiment(excited_tweet, status_id, text))),
               0)
  expect_equal(sum(is.na(feature_sentiment(mixed_tweets, status_id, text))),
               0)
})


test_that("sentiment errors work", {
  expect_error(feature_sentiment(plain_tweet, status_id, text, sentiments = "nonsense"),
               "sentiments must be at least one of nrc, Bing-Liu or MPQA")
})
