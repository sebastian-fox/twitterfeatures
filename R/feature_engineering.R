#' Hashtag features
#'
#' Engineers features related to hashtags in Tweets.
#'
#' @return A data frame of document id fields and their associated hashtag
#'   features
#' @details Hashtags are first converted to lowercase before the features are calculated.
#'
#' The hashtag features are calculated as a proportion of the total number of words in the Tweet:
#' \itemize{
#'  \item{the count of positive/negative sentiment based on the NRC lexicon}
#'  \item{the count of positive/negative sentiment once all NRC lexicon sentiments are grouped}
#'  \item{the count of the top n hashtags used in the training data}
#' }
#'
#' @param data a dataframe or tibble containing the text data and document id
#' @param doc_id_field unquoted field name identifying the field within the data
#'   that represents the unique document id
#' @param text_field unquoted field name identifying the field name in data that
#'   contains the text of the Tweet
#' @param top_n integer, the top n hashtags to create features from
#' @param min_n integer, the minimum number of occurrences a hashtag must appear in the data
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I tweet about one thing #onething #things",
#'                               "I tweet about another thing #another thing #things"))
#' feature_hashtag(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom textdata lexicon_nrc
#' @importFrom tidyr pivot_wider
#' @export
feature_hashtags <- function(data, doc_id_field, text_field, top_n = 100L, min_n = 2L) {
  out <- data %>%
    unnest_tokens(word, {{ text_field }},
                  to_lower = FALSE, token = "tweets") %>%
    group_by({{ doc_id_field }}) %>%
    mutate(total_words_in_tweet = n(),
           word = tolower(word)) %>%
    filter(grepl("^#", word))

  # nrc lexicon - pos/neg sentiments for hashtags
  nrc_pos_neg_hashtags <- out %>%
    mutate(word = gsub("^#", "", word)) %>%
    inner_join(lexicon_nrc(), by = "word") %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    count({{ doc_id_field }}, total_words_in_tweet, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n,
                values_fill = 0) %>%
    mutate(nrc_pos_hash = positive / total_words_in_tweet,
           nrc_neg_hash = negative / total_words_in_tweet) %>%
    select({{ doc_id_field }}, nrc_pos_hash, nrc_neg_hash)

  # nrc lexicon - pos/neg groupings for hashtags
  nrc_groupings <- data.frame(sentiment = c("anger", "disgust", "fear", "negative", "sadness",
                                            "anticipation", "joy", "positive", "surprise", "trust"),
                              grouping = c(rep("neg_group", 5), rep("pos_group", 5)),
                              stringsAsFactors = FALSE)
  nrc_pos_neg_group <- out %>%
    mutate(word = gsub("^#", "", word)) %>%
    inner_join(lexicon_nrc(), by = "word") %>%
    left_join(nrc_groupings, by = "sentiment") %>%
    count({{ doc_id_field }}, total_words_in_tweet, grouping) %>%
    pivot_wider(names_from = grouping, values_from = n,
                values_fill = 0) %>%
    mutate(nrc_pos_hash_group = pos_group / total_words_in_tweet,
           nrc_neg_hash_group = neg_group / total_words_in_tweet) %>%
    select({{ doc_id_field }}, nrc_pos_hash_group, nrc_neg_hash_group)

  # top x tweets
  out <- out %>%
    group_by(word) %>%
    mutate(n = n()) %>%
    filter(n >= min_n) %>%
    ungroup()
  top_hashtags <- out %>%
    select(word, n) %>%
    unique() %>%
    top_n(top_n, wt = n) %>%
    select(word)
  out <- out %>%
    inner_join(top_hashtags, by = "word") %>%
    count({{ doc_id_field }}, total_words_in_tweet, word) %>%
    mutate(frequency_of_hashtag = n / total_words_in_tweet) %>%
    select(-c(n, total_words_in_tweet)) %>%
    pivot_wider(names_from = word, values_from = frequency_of_hashtag,
                values_fill = 0)
  data <- data %>%
    select({{ doc_id_field }}) %>%
    left_join(out, by = rlang::quo_text(enquo(doc_id_field))) %>%
    left_join(nrc_pos_neg_hashtags, by = rlang::quo_text(enquo(doc_id_field))) %>%
    left_join(nrc_pos_neg_group, by = rlang::quo_text(enquo(doc_id_field)))
  data[is.na(data)] <- 0

  return(data)
}

feature_sentiment <- function(data, doc_id_field, text_field, sentiments = c("nrc", "Bing-Liu", "MPQA")) {
  if (any(!(sentiments %in% c("nrc", "Bing-Liu", "MPQA")))) stop("sentiments must be at least one of nrc, Bing-Liu or MPQA")

  out <- data %>%
    unnest_tokens(word, {{ text_field }},
                  to_lower = FALSE, token = "tweets",
                  strip_punct = TRUE) %>%
    group_by({{ doc_id_field }}) %>%
    mutate(total_words_in_tweet = n(),
           word = tolower(word))

  doc_ids <- data %>%
    select({{ doc_id_field }})

  if ("nrc" %in% sentiments) {
    nrc_groupings <- data.frame(sentiment = c("anger", "disgust", "fear", "negative", "sadness",
                                              "anticipation", "joy", "positive", "surprise", "trust"),
                                grouping = c(rep("neg_group", 5), rep("pos_group", 5)),
                                stringsAsFactors = FALSE)
    nrc_pos_neg_group <- out %>%
      inner_join(lexicon_nrc(), by = "word") %>%
      left_join(nrc_groupings, by = "sentiment") %>%
      count({{ doc_id_field }}, total_words_in_tweet, grouping) %>%
      spread(grouping, n, fill = 0) %>%
      mutate(nrc_pos_word_group = pos_group / total_words_in_tweet,
             nrc_neg_word_group = neg_group / total_words_in_tweet) %>%
      select({{ doc_id_field }}, nrc_pos_word_group, nrc_neg_word_group)
    doc_ids <- doc_ids %>%
      left_join(nrc_pos_neg_group, by = rlang::quo_text(enquo(doc_id_field)))
  }

  if ("Bing-Liu" %in% sentiments) {
    bing_pos_neg_group <- out %>%
      inner_join(lexicon_bing(), by = "word") %>%
      count({{ doc_id_field }}, total_words_in_tweet, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(bingliu_pos_word = positive / total_words_in_tweet,
             bingliu_neg_word = negative / total_words_in_tweet) %>%
      select({{ doc_id_field }}, bingliu_pos_word, bingliu_neg_word)
    doc_ids <- doc_ids %>%
      left_join(bing_pos_neg_group, by = rlang::quo_text(enquo(doc_id_field)))
  }

  if ("MPQA" %in% sentiments) {
    # downloaded from https://mpqa.cs.pitt.edu/lexicons/
    mpqa_groupings <- read.csv("./ppt/mpqa_lexicon.csv", stringsAsFactors = FALSE)
    mpqa_pos_neg_group <- out %>%
      inner_join(mpqa_groupings, by = "word") %>%
      count({{ doc_id_field }}, total_words_in_tweet, grouping) %>%
      spread(grouping, n, fill = 0) %>%
      mutate(mpqa_pos_word_group = positive / total_words_in_tweet,
             mpqa_neg_word_group = negative / total_words_in_tweet) %>%
      select({{ doc_id_field }}, mpqa_pos_word_group, mpqa_neg_word_group)
    doc_ids <- doc_ids %>%
      left_join(mpqa_pos_neg_group, by = rlang::quo_text(enquo(doc_id_field)))
  }

  doc_ids[is.na(doc_ids)] <- 0
  return(doc_ids)
}

library(stringr)
feature_quantitative <- function(data, doc_id_field, text_field) {
  characters <- data %>%
    mutate(num_characters = nchar({{ text_field }})) %>%
    select({{ doc_id_field }}, num_characters)
  words <- data %>%
    unnest_tokens(word, {{ text_field }},
                  to_lower = FALSE, token = "tweets",
                  strip_punct = TRUE) %>%
    mutate(word_length = nchar(word),
           word = tolower(word)) %>%
    group_by({{ doc_id_field }}) %>%
    summarise(num_words = n(),
              ave_word_length = mean(word_length))
  sentences <- data %>%
    unnest_tokens(sentence, {{ text_field }},
                  token = "sentences") %>%
    mutate(num_words = str_count(sentence, "\\w+")) %>%
    group_by({{ doc_id_field }}) %>%
    summarise(num_sentences = n(),
              ave_words_in_sentence = mean(num_words))
  out <- characters %>%
    left_join(words, by = rlang::quo_text(enquo(doc_id_field))) %>%
    left_join(sentences, by = rlang::quo_text(enquo(doc_id_field))) %>%
    return()
}

library(stringi)
feature_emoji <- function(data, doc_id_field, text_field, top_n = 20) {
  emojis <- data.frame(emoji = readLines("https://unicode.org/Public/emoji/12.0/emoji-test.txt"), stringsAsFactors = FALSE) %>%
    filter(!grepl("^#", emoji),
           emoji != "") %>%
    mutate(emoji_new = str_squish(str_replace(emoji, ";.*$", "")),
           unicode = gsub(" ", ".{1,10}", emoji_new),
           unicode = tolower(unicode)) %>%
    pull(unicode) %>%
    unique()
  text_field <- enquo(text_field)
  emoji_count <- data %>%
    mutate(!!quo_name(text_field) := stri_escape_unicode(.data[[!!quo_name(text_field)]]),
           emoji = str_extract_all(!!text_field, paste0(emojis, collapse = "|"))) %>%
    unnest(emoji) %>%  # seperates the different charges for each name
    count({{ doc_id_field }}, emoji)

  top_n_emojis <- emoji_count %>%
    group_by(emoji) %>%
    summarise(total_used = sum(n)) %>%
    top_n(top_n, wt = total_used)

  word_count <- data %>%
    unnest_tokens(word, !!text_field,
                  to_lower = FALSE, token = "tweets",
                  strip_punct = TRUE) %>%
    count({{ doc_id_field }}, name = "total_words_in_tweet")

  emoji_count <- emoji_count %>%
    inner_join(word_count, by = rlang::quo_text(enquo(doc_id_field))) %>%
    filter(emoji %in% top_n_emojis$emoji) %>%
    mutate(emoji_per_word = n / total_words_in_tweet) %>%
    dplyr::select({{ doc_id_field }}, emoji, emoji_per_word) %>%
    mutate(emoji = paste0("u", emoji)) %>%
    group_by({{ doc_id_field }}, emoji) %>%
    summarise(emoji_per_word = sum(emoji_per_word)) %>%
    spread(emoji, emoji_per_word, fill = 0)

  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(emoji_count, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)
}

feature_orthographic <- function(data, doc_id_field, text_field) {
  out <- data %>%
    mutate(num_characters = nchar({{ text_field }}),
           punctuation = gsub("@\\w+ *", "", {{ text_field }}),
           punctuation = gsub("#\\w+ *", "", punctuation),
           punctuation = gsub("\\|", "", punctuation),
           punctuation = gsub("[^[:punct:]+]", "", punctuation)) %>%
    select(- {{ text_field }}) %>%
    unnest_tokens(character, punctuation,
                  strip_punct = FALSE) %>%
    count({{ doc_id_field }}, character, num_characters) %>%
    mutate(punct_per_character = n / num_characters,
           character = paste("punc", character, sep = "_")) %>%
    select(-n, -num_characters) %>%
    spread(character, punct_per_character)

  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(out, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)

}

feature_longwords <- function(data, doc_id_field, text_field, remove_urls = TRUE) {
  if (remove_urls) {
    data <- data %>%
      mutate_at(.vars = quo_name(enquo(text_field)),
                .funs = qdapRegex::rm_twitter_url) %>%

      mutate_at(.vars = quo_name(enquo(text_field)),
                .funs = str_replace_all,
                pattern = "/",
                replacement = "")
  }
  data_temp <- data %>%
    unnest_tokens(word, {{ text_field }},
                  to_lower = FALSE, token = "tweets",
                  strip_punct = TRUE) %>%
    mutate(word_length = nchar(word),
           word = tolower(word)) %>%
    group_by(status_id) %>%
    mutate(total_words_in_tweet = n(),
           elongated = grepl("([[:alpha:]])\\1{2,}", word), # search for words with 3 or more repeated letters
           over10 = nchar(word) > 10) %>%
    group_by(word) %>%
    mutate(number_repeats = n()) %>%
    ungroup() %>%
    filter(!grepl("^#|^@|^http", word))

  elongate <- data_temp %>%
    filter(elongated == TRUE) %>%
    count(status_id, total_words_in_tweet) %>%
    mutate(elongated_words = n / total_words_in_tweet) %>%
    select(-c(total_words_in_tweet:n))

  top10 <- data_temp %>%
    filter(over10 == TRUE) %>%
    select(word, number_repeats) %>%
    unique() %>%
    top_n(10, number_repeats)

  long_words <- data_temp %>%
    filter(word %in% top10$word) %>%
    count(status_id, total_words_in_tweet, word) %>%
    mutate(prop_word_in_tweet = n / total_words_in_tweet,
           word = paste0("long_", word)) %>%
    select(-c("total_words_in_tweet", "n")) %>%
    spread(word, prop_word_in_tweet, fill = 0)

  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(elongate, by = rlang::quo_text(enquo(doc_id_field))) %>%
    left_join(long_words, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)
}

library(quanteda)
feature_stopwords <- function(data, doc_id_field, text_field) {
  data_temp <- data %>%
    unnest_tokens(word, {{ text_field }}, token = "tweets",
                  to_lower = FALSE, strip_punct = TRUE) %>%
    group_by({{ doc_id_field }}) %>%
    mutate(total_words_in_tweet = n(),
           word = tolower(word)) %>%
    filter(word %in% stopwords::stopwords("en")) %>%
    mutate(num_stopwords = n()) %>%
    ungroup()
  top10 <- data_temp %>%
    count(word) %>%
    top_n(10, n) %>%
    pull(word)
  data_temp <- data_temp %>%
    filter(word %in% top10) %>%
    count({{ doc_id_field }}, total_words_in_tweet, num_stopwords, word) %>%
    mutate(stopwords_proportion = num_stopwords / total_words_in_tweet,
           stopword_proportion = n / total_words_in_tweet,
           word = paste0("stopword_", word)) %>%
    select(-c("total_words_in_tweet", "num_stopwords", "n")) %>%
    spread(word, stopword_proportion)

  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(data_temp, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)

}

feature_onomatopoeia <- function(data, doc_id_field, text_field) {
  #http://www.writtensound.com/index.php?term=c
}

library(lexicon)
feature_slang <- function(data, doc_id_field, text_field) {
  data_temp <- data %>%
    unnest_tokens(word, {{ text_field }}, token = "tweets",
                  to_lower = FALSE, strip_punct = TRUE) %>%
    group_by({{ doc_id_field }}) %>%
    mutate(total_words_in_tweet = n(),
           word = tolower(word)) %>%
    filter(word %in% tolower(lexicon::hash_internet_slang$x)) %>%
    mutate(num_slang = n()) %>%
    ungroup()
  top10 <- data_temp %>%
    count(word) %>%
    top_n(10, n) %>%
    pull(word)
  data_temp <- data_temp %>%
    filter(word %in% top10) %>%
    count({{ doc_id_field }}, total_words_in_tweet, num_slang, word) %>%
    mutate(slangwords_proportion = num_slang / total_words_in_tweet,
           slang_proportion = n / total_words_in_tweet,
           word = paste0("slang_", word)) %>%
    select(-c("total_words_in_tweet", "num_slang", "n")) %>%
    spread(word, slang_proportion)

  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(data_temp, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)

}

feature_PoS <- function(data, doc_id_field, text_field) {

}

feature_ngrams <- function(data, doc_id_field, text_field, type = "ngrams", n_ngrams = 1, top_num = 1000) {
  if (!type %in% c("ngrams", "character_shingles")) stop("type must be either ngram or character_shingles")
  data_temp <- data %>%
    unnest_tokens(ngram, text, token = type, n = n_ngrams) %>%
    group_by({{ doc_id_field }}) %>%
    mutate(ngrams_in_tweet = n()) %>%
    ungroup()
  topnum <- data_temp %>%
    count(ngram) %>%
    top_n(top_num, n) %>%
    pull(ngram)
  data_temp <- data_temp %>%
    filter(ngram %in% topnum) %>%
    count({{ doc_id_field }}, ngrams_in_tweet, ngram) %>%
    mutate(ngram_proportion = n / ngrams_in_tweet,
           ngram = paste0(type, "_", n_ngrams, "_", ngram)) %>%
    select(-c("ngrams_in_tweet", "n"))

  if (type == "character_shingles") {
    data_temp <- data_temp %>%
      mutate(ngram = stri_escape_unicode(ngram),
             ngram = gsub("\\\\", "\\", ngram))
  }
  data_temp <- data_temp %>%
    spread(ngram, ngram_proportion)

  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(data_temp, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)
}

feature_skip_ngrams <- function(data, doc_id_field, text_field,
                                n_ngrams_min = 3, n_ngrams_max = 7,
                                n_skip_words = 4, top_num = 1000) {
  data_temp <- data %>%
    unnest_tokens(skipgrams, text, token = "skip_ngrams",
                  n_min = n_ngrams_min,
                  n = n_ngrams_max,
                  k = n_skip_words) %>%
    mutate(num_words = str_count(skipgrams, "[\\w]+[[:punct:]]*[\\w]*")) %>%
    group_by({{ doc_id_field }}, num_words) %>%
    mutate(skipgrams_in_tweet = n()) %>%
    ungroup()
  topnum <- data_temp %>%
    count(skipgrams, num_words) %>%
    group_by(num_words) %>%
    top_n(top_num, n) %>%
    pull(skipgrams)

  data_temp <- data_temp %>%
    filter(skipgrams %in% topnum) %>%
    count({{ doc_id_field }}, skipgrams_in_tweet, skipgrams, num_words) %>%
    mutate(skipgram_proportion = n / skipgrams_in_tweet,
           skipgrams = paste0("skipgram_", num_words, "_", skipgrams)) %>%
    select(-c("skipgrams_in_tweet", "n", "num_words")) %>%
    spread(skipgrams, skipgram_proportion)

  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(data_temp, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)
}

feature_wordcors <- function(data, doc_id_field, text_field, top_num = 1000) {
  data_temp <- data %>%
    unnest_tokens(word, {{ text_field }},
                  token = "tweets", to_lower = FALSE,
                  strip_punct = TRUE) %>%
    mutate(word = tolower(word)) %>%
    filter(!(word %in% stopwords::stopwords("en")))

  top50percent <- data_temp %>%
    count(word, sort = TRUE) %>%
    filter(n > mean(n)) %>%
    pull(word)

  data_temp <- data_temp %>%
    filter(word %in% top50percent) %>%
    pairwise_cor(word, {{ doc_id_field }}, sort = TRUE) %>%
    top_n(top_num, correlation) %>%
    filter(row_number() %% 2 == 1) %>%
    transmute(search_pairs = paste(item1, item2, collapse = "|"))
  data <- data %>%
    mutate(count = sum(str_count({{ text_field }}, data_temp$search_pairs)))
  return(data)

}
