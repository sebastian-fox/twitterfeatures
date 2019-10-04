slang_tweet <- data.frame(status_id = c(1234, 5678),
                          text = c("Omg! #brb! wtf this is a tweet",
                                   "rofl! omg! This is another tweet"),
                          stringsAsFactors = FALSE)

plain_tweet <- data.frame(status_id = c(1234, 5678),
                          text = c("Everything is great, happy and fantastic",
                                   "Bored, sad, tired, grumpy"),
                          stringsAsFactors = FALSE)

slang_df_n1 <- feature_slang(slang_tweet, status_id, text, top_num = 1)
slang_df <- feature_slang(slang_tweet, status_id, text)
plain_df <- feature_slang(plain_tweet, status_id, text)



test_that("slang dimensions are expected", {
  expect_equal(dim(slang_df_n1), c(2L, 3L))
  expect_equal(dim(slang_df), c(2L, 4L))
  expect_equal(dim(plain_df), c(2L, 1L))
})

test_that("slang function doesn't return NAs", {
  expect_equal(sum(is.na(slang_df_n1)), 0)
  expect_equal(sum(is.na(slang_df)), 0)
  expect_equal(sum(is.na(plain_df)), 0)
})
