tweet <- data.frame(status_id = c(1234, 5678),
                    text = c("This is a tweet with an exceptionally long word #longwordszz",
                             "This tweet is another tweeeeeeeeeeet"),
                    stringsAsFactors = FALSE)

no_stop_wrd_tweets <- data.frame(status_id = c(1234, 5678),
                               text = c("exceptionally delicious",
                                        "beautifully delightful"),
                               stringsAsFactors = FALSE)

plain_tweet_n1 <- feature_stopwords(tweet, status_id, text, top_num = 1)
plain_tweet <- feature_stopwords(tweet, status_id, text)
no_stp_wrd_tweet <- feature_stopwords(no_stop_wrd_tweets, status_id, text)

test_that("wtopword dimensions are expected", {
  expect_equal(dim(plain_tweet_n1), c(2L, 4L))
  expect_equal(dim(plain_tweet), c(2L, 7L))
  expect_equal(dim(no_stp_wrd_tweet), c(2L, 2L))

})

test_that("stopword function doesn't return NAs", {
  expect_equal(sum(is.na(plain_tweet_n1)), 0)
  expect_equal(sum(is.na(plain_tweet)), 0)
  expect_equal(sum(is.na(no_stp_wrd_tweet)), 0)
})
