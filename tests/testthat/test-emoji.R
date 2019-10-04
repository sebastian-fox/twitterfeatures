library(dplyr)
tweets_emo <- data.frame(status_id = c(1234, 5678, 999),
                         text = c("I tweet about one thing 😃 #onething #things 😋",
                                  "I tweet about another thing 😃 #another thing #things",
                                  "\U0001f609"),
                         stringsAsFactors = FALSE) %>%
  feature_emoji(status_id, text)

tweets_no_emo <- data.frame(status_id = c(1234, 5678, 999),
                            text = c("I tweet about one thing #onething #things",
                                     "I tweet about another thing #another thing #things",
                                     "I love unit tests"),
                            stringsAsFactors = FALSE) %>%
  feature_emoji(status_id, text)

tweet_emo <- data.frame(status_id = 123,
                        text = paste("\U0001f600, \U0001f601, \U0001f602",
                                     "\U0001f923, \U0001f603, \U0001f604",
                                     "\U0001f605, \U0001f606, \U0001f609",
                                     "\U0001f60a, \U0001f60b, \U0001f60e",
                                     "\U0001f60d, \U0001f618, \U0001f617",
                                     "\U0001f619, \U0001f61a, \U0001f642",
                                     "\U0001f917, \U0001f929, \U0001f914",
                                     "\U0001f928, \U0001f610, \U0001f611",
                                     "\U0001f636, \U0001f644, \U0001f60f",
                                     "\U0001f623, \U0001f625, \U0001f62e",
                                     "\U0001f910, \U0001f62f, \U0001f62a",
                                     "\U0001f62b, \U0001f634, \U0001f60c",
                                     "\U0001f61b, \U0001f61c, \U0001f61d, \U0001f61d",
                                     collapse = ", "),
                        stringsAsFactors = FALSE)
tweet_emo1 <- feature_emoji(tweet_emo, status_id, text, top_num = 1)
tweet_emo <- feature_emoji(tweet_emo, status_id, text)

test_that("emoji dimensions are expected", {
  expect_equal(dim(tweets_emo), c(3L, 5L))
  expect_equal(dim(tweets_no_emo), c(3L, 2L))
  expect_equal(dim(tweet_emo), c(1L, 41L))
  expect_equal(dim(tweet_emo1), c(1L, 3L))
})



test_that("sentiment function doesn't return NAs", {
  expect_equal(sum(is.na(tweets_emo)), 0)
  expect_equal(sum(is.na(tweets_no_emo)), 0)
  expect_equal(sum(is.na(tweet_emo)), 0)
})
