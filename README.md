
<!-- README.md is generated from README.Rmd. Please edit that file -->

# twitterfeatures

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/sebastian-fox/twitterfeatures.svg?branch=master)](https://travis-ci.org/sebastian-fox/twitterfeatures)
[![Coveralls test
coverage](https://coveralls.io/repos/github/sebastian-fox/twitterfeatures/badge.svg)](https://coveralls.io/r/sebastian-fox/twitterfeatures?branch=master)
<!-- badges: end -->

The goal of twitterfeatures is to make it easy to engineer simple
features out of Twitter Content.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("sebastian-fox/twitterfeatures")
```

## Example

This is a basic example. It shows how to create sentiment features from
the Tweets using 3 different sentiment dictionaries.

``` r
library(twitterfeatures)
tweet <- data.frame(status_id = c(1234, 5678),
                    text = c("Everything is great, happy and fantastic",
                             "Bored, sad, tired, grumpy"),
                    stringsAsFactors = FALSE)

feature_sentiment(tweet, status_id, text)
#>   status_id nrc_pos_word_group nrc_neg_word_group bingliu_pos_word
#> 1      1234          0.6666667               0.00              0.5
#> 2      5678          0.0000000               1.25              0.0
#>   bingliu_neg_word mpqa_pos_word_group mpqa_neg_word_group
#> 1                0                 0.5                 0.0
#> 2                1                 0.0                 0.5
```
