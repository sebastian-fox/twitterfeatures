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

skipngram_df_n2_n3 <- feature_skip_ngrams(tweets,
                                          status_id,
                                          text,
                                          n_ngrams_min = 2,
                                          n_ngrams_max = 3,
                                          top_num = 3)
skipngram_df_sw1 <- feature_skip_ngrams(tweets,
                                          status_id,
                                          text,
                                          top_num = 3,
                                          n_skip_words = 1)
skipunusual_tweet_df <- feature_skip_ngrams(unusual_tweets, status_id, text)


test_that("skip ngram dimensions are expected", {
  expect_equal(dim(skipngram_df_n2_n3), c(2L, 46L))
  expect_equal(dim(skipngram_df_sw1), c(2L, 52L))
  expect_equal(dim(skipunusual_tweet_df), c(4L, 28L))
})

test_that("skip ngram function doesn't return NAs", {
  expect_equal(sum(is.na(skipngram_df_n2_n3)), 0)
  expect_equal(sum(is.na(skipngram_df_sw1)), 0)
  expect_equal(sum(is.na(skipunusual_tweet_df)), 0)
})
