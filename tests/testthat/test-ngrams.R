tweets <- data.frame(status_id = c(1234, 5678),
                     text = c("I tweet about one thing #onething #things",
                              "I tweet about another #anotherthing"),
                     stringsAsFactors = FALSE)
unusual_tweets <- data.frame(status_id = c(1234, 345, 5435, 6543),
                             text = c("Onewordonly",
                                      "Oneword http://t.co.asjkre/34hjl",
                                      "Many words thank you",
                                      "t.co/we23f.jx9km @username"),
                             stringsAsFactors = FALSE)

ngram_df_n1 <- feature_ngrams(tweets, status_id, text, n_ngram = 1, top_num = 3)
ngram_df_n3 <- feature_ngrams(tweets, status_id, text, n_ngram = 3, top_num = 1)
ngram_df_default <- feature_ngrams(tweets, status_id, text)

unusual_tweet_df <- feature_ngrams(unusual_tweets, status_id, text, n_ngram = 2)
unusual_tweet_df_nofeatures <- feature_ngrams(unusual_tweets[unusual_tweets$status_id != 5435, ],
                                   status_id, text, n_ngram = 2)

test_that("ngram dimensions are expected", {
  expect_equal(dim(ngram_df_n1), c(2L, 4L))
  expect_equal(dim(ngram_df_n3), c(2L, 2L))
  expect_equal(dim(ngram_df_default), c(2L, 10L))
  expect_equal(dim(unusual_tweet_df), c(4L, 4L))
  expect_equal(dim(unusual_tweet_df_nofeatures), c(3L, 1L))
})

test_that("gnram function doesn't return NAs", {
  expect_equal(sum(is.na(ngram_df_n1)), 0)
  expect_equal(sum(is.na(ngram_df_n3)), 0)
  expect_equal(sum(is.na(ngram_df_default)), 0)
  expect_equal(sum(is.na(unusual_tweet_df)), 0)
  expect_equal(sum(is.na(unusual_tweet_df_nofeatures)), 0)
})
