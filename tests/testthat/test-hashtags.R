tweets <- data.frame(status_id = c(1234, 5678),
                     text = c("I tweet about one thing #happy #things",
                              "I tweet about another #terrible #things"),
                     stringsAsFactors = FALSE)

tweets_one_neg_hash <- data.frame(status_id = c(1234, 5678),
                                  text = c("I tweet about one thing #things",
                                           "I tweet about another #terrible #things"),
                                  stringsAsFactors = FALSE)

tweets_no_hash <- data.frame(status_id = c(1234, 5678),
                             text = c("I tweet about one thing",
                                      "I tweet about another"),
                             stringsAsFactors = FALSE)




expected_dimensions <- c(2L, 6L)
test_that("hashtag dimensions are expected", {
  expect_equal(dim(feature_hashtags(tweets, status_id, text)),
               expected_dimensions)
  expect_equal(dim(feature_hashtags(tweets_one_neg_hash, status_id, text)),
               expected_dimensions)
  expect_equal(dim(feature_hashtags(tweets_no_hash, status_id, text)),
               expected_dimensions)
})

test_that("hashtag function doesn't return NAs", {
  expect_equal(sum(is.na(feature_hashtags(tweets, status_id, text))),
               0)
  expect_equal(sum(is.na(feature_hashtags(tweets_one_neg_hash, status_id, text))),
               0)
  expect_equal(sum(is.na(feature_hashtags(tweets_no_hash, status_id, text))),
               0)
})
