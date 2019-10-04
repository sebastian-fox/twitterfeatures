tweet <- data.frame(status_id = c(1234, 5678),
                          text = c("Thiiiis is a tweet with an exceptionally long word #longwordszz",
                                   "This tweeeeeeeeeeet is another tweeeeeeeeeeet"),
                          stringsAsFactors = FALSE)

short_wrd_tweets <- data.frame(status_id = c(1234, 5678),
                               text = c("I tweet about one thing #onething #things",
                                        "I tweet about other things #otherthings
                                         #things"),
                               stringsAsFactors = FALSE)

plain_tweet <- feature_longwords(tweet, status_id, text)
plain_tweet1 <- feature_longwords(tweet, status_id, text, top_num = 1)

shrt_tweet <- feature_longwords(short_wrd_tweets, status_id, text)

test_that("sentiment dimensions are expected", {
  expect_equal(dim(plain_tweet), c(2L, 4L))
  expect_equal(dim(plain_tweet1), c(2L, 3L))
  expect_equal(dim(shrt_tweet), c(2L, 2L))

})

test_that("sentiment function doesn't return NAs", {
  expect_equal(sum(is.na(plain_tweet)), 0)
  expect_equal(sum(is.na(plain_tweet1)), 0)
  expect_equal(sum(is.na(shrt_tweet)), 0)
})
