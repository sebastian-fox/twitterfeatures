plain_tweet <- data.frame(status_id = c(1234, 5678),
                          text = c("This is a tweet",
                                   "This is another tweet"),
                          stringsAsFactors = FALSE)

excited_tweet <- data.frame(status_id = c(1234, 5678),
                            text = c("Everything is great, happy and fantastic",
                                     "Bored, sad, tired, grumpy"),
                            stringsAsFactors = FALSE)




test_that("hashtag dimensions are expected", {
  expect_equal(dim(feature_sentiment(plain_tweet, status_id, text)),
               c(2L, 7L))
  expect_equal(dim(feature_sentiment(excited_tweet, status_id, text)),
               c(2L, 7L))
})

test_that("hashtag function doesn't return NAs", {
  expect_equal(sum(is.na(feature_sentiment(plain_tweet, status_id, text))),
               0)
  expect_equal(sum(is.na(feature_sentiment(excited_tweet, status_id, text))),
               0)
})
