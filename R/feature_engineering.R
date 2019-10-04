#' Hashtag features
#'
#' Engineers features related to hashtags in Tweets.
#'
#' @return A data frame of document ids their associated hashtag features
#' @details Hashtags are first converted to lowercase before the features are
#'   calculated.
#'
#'   The hashtag features are calculated as a proportion of the total number of
#'   words in the Tweet:
#'
#'   \itemize{
#'     \item{the count of positive/negative sentiment based on the NRC lexicon}
#'     \item{the count of positive/negative sentiment once all NRC lexicon
#'     sentiments are grouped}
#'     \item{the count of the top n hashtags used in the training data}
#'   }
#'
#' @param data a dataframe or tibble containing the text data and document id
#' @param doc_id_field unquoted field name identifying the field within the data
#'   that represents the unique document id
#' @param text_field unquoted field name identifying the field name in data that
#'   contains the text of the Tweet
#' @param top_num integer, the top n hashtags to create features from
#' @param min_n integer, the minimum number of occurrences a hashtag must appear
#'   in the data
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I tweet about one thing #onething #things",
#'                               "I tweet about another thing #another
#'                               thing #things"),
#'                      stringsAsFactors = FALSE)
#' feature_hashtags(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom textdata lexicon_nrc
#' @importFrom tidyr pivot_wider
#' @export
feature_hashtags <- function(data, doc_id_field, text_field, top_num = 100L, min_n = 2L) {
  out <- data %>%
    unnest_tokens(word, {{ text_field }},
                  to_lower = FALSE, token = "tweets",
                  strip_punct = TRUE) %>%
    group_by({{ doc_id_field }}) %>%
    mutate(total_words_in_tweet = n(),
           word = tolower(word)) %>%
    filter(grepl("^#", word))

  # nrc lexicon - pos/neg sentiments for hashtags
  nrc_pos_neg_hashtags <- out %>%
    mutate(word = gsub("^#", "", word)) %>%
    inner_join(lexicon_nrc(), by = "word") %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    count({{ doc_id_field }}, total_words_in_tweet, sentiment)
  if (nrow(nrc_pos_neg_hashtags) > 0) {
    nrc_pos_neg_hashtags <- nrc_pos_neg_hashtags %>%
      pivot_wider(names_from = sentiment, values_from = n)
      missing_fields <- setdiff(c("positive", "negative"),
                                names(nrc_pos_neg_hashtags)) # check if either positive of negative are missing
      if (length(missing_fields) > 0) nrc_pos_neg_hashtags[missing_fields] <- 0 # Add them, filled with '0's
    nrc_pos_neg_hashtags <- nrc_pos_neg_hashtags %>%
      mutate(nrc_pos_hash = positive / total_words_in_tweet,
             nrc_neg_hash = negative / total_words_in_tweet) %>%
      select({{ doc_id_field }}, nrc_pos_hash, nrc_neg_hash)
  } else {
    nrc_pos_neg_hashtags <- data %>%
      select({{ doc_id_field }}) %>%
      filter(is.null({{ doc_id_field }})) %>%
      mutate(nrc_pos_hash = numeric(),
             nrc_neg_hash = numeric())
  }


  # nrc lexicon - pos/neg groupings for hashtags
  nrc_groupings <- data.frame(sentiment = c("anger", "disgust", "fear", "negative", "sadness",
                                            "anticipation", "joy", "positive", "surprise", "trust"),
                              grouping = c(rep("neg_group", 5), rep("pos_group", 5)),
                              stringsAsFactors = FALSE)
  nrc_pos_neg_group <- out %>%
    mutate(word = gsub("^#", "", word)) %>%
    inner_join(lexicon_nrc(), by = "word") %>%
    left_join(nrc_groupings, by = "sentiment") %>%
    count({{ doc_id_field }}, total_words_in_tweet, grouping)

  if (nrow(nrc_pos_neg_group) > 0) {

    nrc_pos_neg_group <- nrc_pos_neg_group %>%
      pivot_wider(names_from = grouping, values_from = n)
    missing_fields <- setdiff(c("pos_group", "neg_group"),
                              names(nrc_pos_neg_group)) # check if either positive of negative are missing
    if (length(missing_fields) > 0) nrc_pos_neg_group[missing_fields] <- 0 # Add them, filled with '0's
    nrc_pos_neg_group <- nrc_pos_neg_group %>%
      mutate(nrc_pos_hash_group = pos_group / total_words_in_tweet,
             nrc_neg_hash_group = neg_group / total_words_in_tweet) %>%
      select({{ doc_id_field }}, nrc_pos_hash_group, nrc_neg_hash_group)

  } else {
    nrc_pos_neg_group <- data %>%
      select({{ doc_id_field }}) %>%
      filter(is.null({{ doc_id_field }})) %>%
      mutate(nrc_pos_hash_group = numeric(),
             nrc_neg_hash_group = numeric())
  }


  # top x tweets
  out <- out %>%
    group_by(word) %>%
    mutate(n = n()) %>%
    filter(n >= min_n) %>%
    ungroup()
  top_hashtags <- out %>%
    select(word, n) %>%
    unique() %>%
    top_n(top_num, wt = n) %>%
    select(word)
  out <- out %>%
    inner_join(top_hashtags, by = "word") %>%
    count({{ doc_id_field }}, total_words_in_tweet, word) %>%
    mutate(frequency_of_hashtag = n / total_words_in_tweet) %>%
    select(-c(n, total_words_in_tweet)) %>%
    pivot_wider(names_from = word, values_from = frequency_of_hashtag)
  data <- data %>%
    select({{ doc_id_field }}) %>%
    left_join(out, by = rlang::quo_text(enquo(doc_id_field))) %>%
    left_join(nrc_pos_neg_hashtags, by = rlang::quo_text(enquo(doc_id_field))) %>%
    left_join(nrc_pos_neg_group, by = rlang::quo_text(enquo(doc_id_field)))
  data[is.na(data)] <- 0

  return(data)
}


#' Sentiment features
#'
#' Engineers features related to sentiment in Tweets.
#'
#' @return A data frame of document ids and their associated sentiment
#'   features
#' @details Tweets are first converted to lowercase before the features are
#'   calculated.
#'
#'   The sentiment features are calculated as a proportion of the total number of
#'   words in the Tweet:
#'
#'   \itemize{
#'     \item{the count of sentiment grouping by sentiment lexicon}
#'   }
#'
#' @inheritParams feature_hashtags
#' @param sentiments character, can be one or multiple of nrc, Bing-Liu or MPQA
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I love to tweet about one thing #onething #things",
#'                               "I have doubts about tweeting about another thing
#'                               #another thing #things"),
#'                      stringsAsFactors = FALSE)
#' feature_sentiment(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom textdata lexicon_nrc lexicon_bing
#' @importFrom tidyr pivot_wider
#' @importFrom rlang quo_text
#' @export
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
      count({{ doc_id_field }}, total_words_in_tweet, grouping)

    if (nrow(nrc_pos_neg_group) > 0) {

      nrc_pos_neg_group <- nrc_pos_neg_group %>%
        pivot_wider(names_from = grouping, values_from = n)
      missing_fields <- setdiff(c("pos_group", "neg_group"),
                                names(nrc_pos_neg_group)) # check if either positive of negative are missing
      if (length(missing_fields) > 0) nrc_pos_neg_group[missing_fields] <- 0 # Add them, filled with '0's
      nrc_pos_neg_group <- nrc_pos_neg_group %>%
        mutate(nrc_pos_word_group = pos_group / total_words_in_tweet,
               nrc_neg_word_group = neg_group / total_words_in_tweet) %>%
        select({{ doc_id_field }}, nrc_pos_word_group, nrc_neg_word_group)

    } else {
      nrc_pos_neg_group <- data %>%
        select({{ doc_id_field }}) %>%
        filter(is.null({{ doc_id_field }})) %>%
        mutate(nrc_pos_word_group = numeric(),
               nrc_neg_word_group = numeric())
    }
    doc_ids <- doc_ids %>%
      left_join(nrc_pos_neg_group, by = rlang::quo_text(enquo(doc_id_field)))
  }

  if ("Bing-Liu" %in% sentiments) {
    bing_pos_neg_group <- out %>%
      inner_join(lexicon_bing(), by = "word") %>%
      count({{ doc_id_field }}, total_words_in_tweet, sentiment)

    if (nrow(bing_pos_neg_group) > 0) {

      bing_pos_neg_group <- bing_pos_neg_group %>%
        pivot_wider(names_from = sentiment, values_from = n)
      missing_fields <- setdiff(c("positive", "negative"),
                                names(bing_pos_neg_group)) # check if either positive of negative are missing
      if (length(missing_fields) > 0) bing_pos_neg_group[missing_fields] <- 0 # Add them, filled with '0's
      bing_pos_neg_group <- bing_pos_neg_group %>%
        mutate(bingliu_pos_word = positive / total_words_in_tweet,
               bingliu_neg_word = negative / total_words_in_tweet) %>%
        select({{ doc_id_field }}, bingliu_pos_word, bingliu_neg_word)

    } else {
      bing_pos_neg_group <- data %>%
        select({{ doc_id_field }}) %>%
        filter(is.null({{ doc_id_field }})) %>%
        mutate(bingliu_pos_word = numeric(),
               bingliu_neg_word = numeric())
    }
    doc_ids <- doc_ids %>%
      left_join(bing_pos_neg_group, by = rlang::quo_text(enquo(doc_id_field)))

  }

  if ("MPQA" %in% sentiments) {
    # downloaded from https://mpqa.cs.pitt.edu/lexicons/
    mpqa_pos_neg_group <- out %>%
      inner_join(mpqa_groupings, by = "word") %>%
      count({{ doc_id_field }}, total_words_in_tweet, grouping)
    if (nrow(mpqa_pos_neg_group) > 0) {

      mpqa_pos_neg_group <- mpqa_pos_neg_group %>%
        pivot_wider(names_from = grouping, values_from = n)
      missing_fields <- setdiff(c("positive", "negative"),
                                names(mpqa_pos_neg_group)) # check if either positive of negative are missing
      if (length(missing_fields) > 0) mpqa_pos_neg_group[missing_fields] <- 0 # Add them, filled with '0's
      mpqa_pos_neg_group <- mpqa_pos_neg_group %>%
        mutate(mpqa_pos_word_group = positive / total_words_in_tweet,
               mpqa_neg_word_group = negative / total_words_in_tweet) %>%
        select({{ doc_id_field }}, mpqa_pos_word_group, mpqa_neg_word_group)

    } else {
      mpqa_pos_neg_group <- data %>%
        select({{ doc_id_field }}) %>%
        filter(is.null({{ doc_id_field }})) %>%
        mutate(mpqa_pos_word_group = numeric(),
               mpqa_neg_word_group = numeric())
    }
    doc_ids <- doc_ids %>%
      left_join(mpqa_pos_neg_group, by = rlang::quo_text(enquo(doc_id_field)))
  }

  doc_ids[is.na(doc_ids)] <- 0
  return(doc_ids)
}

#' Quantitative features
#'
#' Engineers quantitative features in Tweets.
#'
#' @return A data frame of document ids and their associated quantitative
#'   features
#' @details The quantitative features are
#'
#'   \itemize{
#'     \item{the count of characters}
#'     \item{the number of words}
#'     \item{the average word length}
#'     \item{the number of sentences}
#'     \item{the average number of words in a sentence}
#'   }
#'
#' @inheritParams feature_hashtags
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I love to tweet about one thing #onething #things",
#'                               "I have doubts about tweeting about another thing
#'                               #another thing #things"),
#'                      stringsAsFactors = FALSE)
#' feature_quantitative(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom rlang quo_text
#' @importFrom stringr str_count
#' @importFrom stringi stri_escape_unicode
#' @export
feature_quantitative <- function(data, doc_id_field, text_field) {
  characters <- data %>%
    select({{ doc_id_field }}, {{ text_field }}) %>%
    mutate(dummy = stri_escape_unicode({{ text_field }}),
           dummy = gsub("\\\\U.\\w+", "X", dummy),
           num_characters = nchar(dummy)) %>%
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
    left_join(sentences, by = rlang::quo_text(enquo(doc_id_field)))
  return(out)
}

#' Emoji features
#'
#' Engineers features related to emojis in Tweets.
#'
#' @return A data frame of document ids their associated emoji features
#' @details The full list of emojis are initially downloaded from
#'   https://unicode.org/Public/emoji/12.0/emoji-test.txt so an internet connection
#'   is required.
#'
#'   The emoji features are calculated as a proportion of the total number of
#'   words in the Tweet:
#'
#'   \itemize{
#'     \item{the count of the top n emojis in the data}
#'   }
#'
#' @inheritParams feature_hashtags
#' @param top_num integer, the top n emojis to create features from
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I tweet about one thing \U0001f602 #onething #things \U0001f600",
#'                               "I tweet about another thing \U0001f602 #another thing #things"),
#'                      stringsAsFactors = FALSE)
#' feature_emoji(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom stringr str_squish str_replace str_extract_all
#' @importFrom stringi stri_escape_unicode
#' @importFrom tidyr pivot_wider unnest
#' @importFrom rlang quo_name
#' @export
feature_emoji <- function(data, doc_id_field, text_field, top_num = 20) {
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
    top_n(top_num, wt = total_used)

  word_count <- data %>%
    unnest_tokens(word, !!text_field,
                  to_lower = FALSE, token = "tweets",
                  strip_punct = TRUE) %>%
    count({{ doc_id_field }}, name = "total_words_in_tweet")

  # count the number of letters in a tweet
  letter_count <- data %>%
    mutate(!!quo_name(text_field) := stri_escape_unicode(.data[[!!quo_name(text_field)]]),
           !!quo_name(text_field) := gsub("\\\\U.\\w+", "X", .data[[!!quo_name(text_field)]]),
           total_characters = nchar(.data[[!!quo_name(text_field)]])) %>%
    select({{ doc_id_field }}, total_characters)

  # calculate the number of emojis per letter
  emoji_per_character <- emoji_count %>%
    group_by({{ doc_id_field }}) %>%
    summarise(total_emojis = sum(n)) %>%
    left_join(letter_count, by = rlang::quo_text(enquo(doc_id_field))) %>%
    mutate(emoji_per_character = total_emojis / total_characters) %>%
    select({{ doc_id_field }}, emoji_per_character)

  emoji_count <- emoji_count %>%
    inner_join(word_count, by = rlang::quo_text(enquo(doc_id_field))) %>%
    filter(emoji %in% top_n_emojis$emoji) %>%
    mutate(emoji_per_word = n / total_words_in_tweet) %>%
    dplyr::select({{ doc_id_field }}, emoji, emoji_per_word) %>%
    mutate(emoji = paste0("u", emoji)) %>%
    group_by({{ doc_id_field }}, emoji) %>%
    summarise(emoji_per_word = sum(emoji_per_word))
  if (nrow(emoji_count) > 0) {
    emoji_count <- emoji_count %>%
      pivot_wider(names_from = emoji, values_from = emoji_per_word)
  } else {
    emoji_count <- data %>%
      select({{ doc_id_field }})
  }


  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(emoji_count, by = rlang::quo_text(enquo(doc_id_field))) %>%
    left_join(emoji_per_character, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)
}

#' Orthographic features
#'
#' Engineers orthographic features in Tweets.
#'
#' @return A data frame of document ids their associated orthographic features
#' @details As part of the pre-processing to calculate these features, usernames
#'   containing @@ are removed, as are hashtags.
#'
#'   The orthographic features are calculated as a proportion of the total number of
#'   characters in the Tweet prior to the pre-processing steps above:
#'
#'   \itemize{
#'     \item{the count of each of the punctuation characters}
#'   }
#'
#' @inheritParams feature_hashtags
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I tweet about one thing! #onething #things",
#'                               "I tweet about another thing!?%$ #another thing #things"),
#'                      stringsAsFactors = FALSE)
#' feature_orthographic(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr pivot_wider unnest
#' @importFrom rlang quo_text
#' @export
feature_orthographic <- function(data, doc_id_field, text_field) {
  out <- data %>%
    select({{ doc_id_field }}, {{ text_field }}) %>%
    mutate(dummy = stri_escape_unicode({{ text_field }}),
           dummy = gsub("\\\\U.\\w+", "X", dummy),
           num_characters = nchar(dummy)) %>%
    select({{ doc_id_field }}, {{ text_field }}, num_characters) %>%
    mutate(punctuation = gsub("@\\w+ *", "", {{ text_field }}),
           punctuation = gsub("#\\w+ *", "", punctuation),
           punctuation = gsub("\\|", "", punctuation),
           punctuation = gsub("[^[:punct:]+]", "", punctuation)) %>%
    select(- {{ text_field }}) %>%
    unnest_tokens(character, punctuation,
                  strip_punct = FALSE) %>%
    count({{ doc_id_field }}, character, num_characters) %>%
    mutate(punct_per_character = n / num_characters,
           character = paste("punc", character, sep = "_")) %>%
    select(-n, -num_characters)

  if (nrow(out) > 0) {
    out <- out %>%
      pivot_wider(names_from = character, values_from = punct_per_character)
  } else {
    out <- data %>%
      select({{ doc_id_field }})
  }


  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(out, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)

}

#' Long word features
#'
#' Engineers features related to long words in Tweets.
#'
#' @return A data frame of document ids their associated long word features
#' @details Tweets are first converted to lowercase before the features are
#'   calculated. Twitter URLs (if remove_urls = TRUE), hashtags and
#'   @@usernames are also removed.
#'
#'   The long word features are calculated as a proportion of the total number of
#'   words in the Tweet:
#'
#'   \itemize{
#'     \item{the count of words that have 3 or more identical consecutive characters}
#'     \item{the count of the top_num words that are over 10 letters long}
#'   }
#'
#' @inheritParams feature_hashtags
#' @param top_num integer, the top n long words to create features from
#' @param remove_urls logical; should urls be removed prior to identifying long words
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I tweeet about one thing #onething #things",
#'                               "I tweeet about otherthings #otherthings
#'                                #things"),
#'                      stringsAsFactors = FALSE)
#' feature_longwords(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr pivot_wider
#' @importFrom qdapRegex rm_twitter_url
#' @importFrom rlang quo_name
#' @importFrom stringr str_replace_all
#' @export
feature_longwords <- function(data, doc_id_field, text_field, top_num = 10, remove_urls = TRUE) {
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
    top_n(top_num, number_repeats)

  long_words <- data_temp %>%
    filter(word %in% top10$word) %>%
    count(status_id, total_words_in_tweet, word) %>%
    mutate(prop_word_in_tweet = n / total_words_in_tweet,
           word = paste0("long_", word)) %>%
    select(-c("total_words_in_tweet", "n"))

  if (nrow(long_words) > 0) {
    long_words <- long_words %>%
      pivot_wider(names_from = word, values_from = prop_word_in_tweet)
  } else {
    long_words <- data %>%
      dplyr::select({{ doc_id_field }})
  }


  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(elongate, by = rlang::quo_text(enquo(doc_id_field))) %>%
    left_join(long_words, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)
}

#' Stop word features
#'
#' Engineers features related to stop words in Tweets.
#'
#' @return A data frame of document ids their associated stop word features
#' @details Tweets are first converted to lowercase before the features are
#'   calculated.
#'
#'   The stop word features are calculated as a proportion of the total number of
#'   words in the Tweet:
#'
#'   \itemize{
#'     \item{the count of the top_num stop words in all Tweets}
#'   }
#'
#' @inheritParams feature_hashtags
#' @param top_num integer, the top n stop words to create features from
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I tweet about one thing #onething #things",
#'                               "I tweet about another #anotherthing
#'                                #things"),
#'                      stringsAsFactors = FALSE)
#' feature_stopwords(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr pivot_wider
#' @importFrom rlang quo_text
#' @importFrom stopwords stopwords
#' @export
feature_stopwords <- function(data, doc_id_field, text_field, top_num = 10) {
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
    top_n(top_num, n) %>%
    pull(word)
  data_temp <- data_temp %>%
    filter(word %in% top10) %>%
    count({{ doc_id_field }}, total_words_in_tweet, num_stopwords, word) %>%
    mutate(stopwords_proportion = num_stopwords / total_words_in_tweet,
           stopword_proportion = n / total_words_in_tweet,
           word = paste0("stopword_", word)) %>%
    select(-c("total_words_in_tweet", "num_stopwords", "n")) %>%
    pivot_wider(names_from = word, values_from = stopword_proportion,
                values_fill = 0)

  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(data_temp, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)

}

#' Onomatopoeiaic features
#'
#' Engineers features related to onomatopoeiaic words in Tweets.
#'
#' @return A data frame of document ids their associated onomatopoeiaic word features
#' @details Tweets are first converted to lowercase before the features are
#'   calculated.
#'
#'   The onomatopoeiaic word features are calculated as a proportion of the total number of
#'   words in the Tweet:
#'
#'   \itemize{
#'     \item{the count of the top_num onomatopoeiaic words in all Tweets}
#'   }
#'
#' @inheritParams feature_hashtags
#' @param top_num integer, the top n onomatopoeiaic words to create features from
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I tweet about one thing #onething #things",
#'                               "I tweet about another #anotherthing
#'                                #things"),
#'                      stringsAsFactors = FALSE)
#' feature_onomatopoeia(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr pivot_wider
#' @importFrom rlang quo_text
#' @export
feature_onomatopoeia <- function(data, doc_id_field, text_field, top_num = 10) {
  #http://www.writtensound.com/index.php?term=c
}

#' Slang features
#'
#' Engineers features related to slang words in Tweets.
#'
#' @return A data frame of document ids their associated slang word features
#' @details Tweets are first converted to lowercase before the features are
#'   calculated. Slang words are taken from the \code{hash_internet_slang} dataset
#'   from the \code{lexicon} package.
#'
#'   The slang word features are calculated as a proportion of the total number of
#'   words in the Tweet:
#'
#'   \itemize{
#'     \item{the total number of slang words in a Tweet}
#'     \item{the count of each of the top_num slang words in all Tweets}
#'   }
#'
#' @inheritParams feature_hashtags
#' @param top_num integer, the top n onomatopoeiaic words to create features from
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I tweet about one thing #onething #things",
#'                               "I tweet about another #anotherthing
#'                                #things"),
#'                      stringsAsFactors = FALSE)
#' feature_slang(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr pivot_wider
#' @importFrom rlang quo_text
#' @export
feature_slang <- function(data, doc_id_field, text_field, top_num = 10) {
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
    top_n(top_num, n) %>%
    pull(word)
  data_temp <- data_temp %>%
    filter(word %in% top10) %>%
    count({{ doc_id_field }}, total_words_in_tweet, num_slang, word) %>%
    mutate(slangwords_proportion = num_slang / total_words_in_tweet,
           slang_proportion = n / total_words_in_tweet,
           word = paste0("slang_", word)) %>%
    select(-c("total_words_in_tweet", "num_slang", "n")) %>%
    pivot_wider(names_from = word, values_from = slang_proportion,
                values_fill = 0)

  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(data_temp, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)

}

#' Part-of-Speech features
#'
#' Engineers features related to Part-of-Speech words in Tweets.
#'
#' @return A data frame of document ids their associated Part-of-Speech word features
#' @details Tweets are first converted to lowercase before the features are
#'   calculated.
#'
#'   The Part-of-Speech word features are calculated as a proportion of the total number of
#'   words in the Tweet:
#'
#'   \itemize{
#'     \item{the number of words in each Part-of-Speech grouping}
#'   }
#'
#' @inheritParams feature_hashtags
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I tweet about one thing #onething #things",
#'                               "I tweet about another #anotherthing
#'                                #things"),
#'                      stringsAsFactors = FALSE)
#' feature_PoS(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr pivot_wider
#' @importFrom rlang quo_text
#' @export
feature_PoS <- function(data, doc_id_field, text_field) {

}

#' ngram features
#'
#' Engineers features related to ngrams (both word or character) in Tweets.
#'
#' @return A data frame of document ids their associated ngrams features
#' @details Tweets are first converted to lowercase before the features are
#'   calculated.
#'
#'   The ngram features are calculated as a proportion of the total number of
#'   ngrams in the Tweet:
#'
#'   \itemize{
#'     \item{the number of the top_num ngrams in all Tweets}
#'   }
#'
#' @inheritParams feature_hashtags
#' @param type string; either ngrams of character_shingles
#' @param n_ngrams ingeger; the number in the ngram
#' @param top_num integer, the top n ngrams to create features from
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I tweet about one thing #onething #things",
#'                               "I tweet about another #anotherthing
#'                                #things"),
#'                      stringsAsFactors = FALSE)
#' feature_ngrams(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr pivot_wider
#' @importFrom rlang quo_text
#' @importFrom stringi stri_escape_unicode
#' @export
feature_ngrams <- function(data, doc_id_field, text_field, type = "ngrams", n_ngrams = 1L, top_num = 1000L) {
  if (!type %in% c("ngrams", "character_shingles")) stop("type must be either ngram or character_shingles")
  data_temp <- data %>%
    unnest_tokens(ngram, {{ text_field }}, token = type, n = n_ngrams) %>%
    group_by({{ doc_id_field }}) %>%
    mutate(ngrams_in_tweet = n(),
           ngram = tolower(ngram)) %>%
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
    pivot_wider(names_from = ngram, values_from = ngram_proportion,
                values_fill = 0)

  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(data_temp, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)
}

#' Skip ngram features
#'
#' Engineers features related to skip word ngrams in Tweets.
#'
#' @return A data frame of document ids their associated skip ngrams features
#' @details Tweets are first converted to lowercase before the features are
#'   calculated.
#'
#'   The ngram features are calculated as a proportion of the total number of
#'   skip ngrams in the Tweet that contain the same number of words:
#'
#'   \itemize{
#'     \item{the number of the top_num skip ngrams of all lengths in all Tweets}
#'   }
#'
#' @inheritParams feature_hashtags
#' @param n_ngrams_min integer; the minimum number in the ngram
#' @param n_ngrams_max integer; the maximum number in the ngram
#' @param n_skip_words integer; the maximum number of words to skip when calculating
#'   ngrams
#' @param top_num integer, the top n skip ngrams to create features from
#' @examples
#' tweets <- data.frame(status_id = c(1234, 5678),
#'                      text = c("I tweet about one thing #onething #things",
#'                               "I tweet about another #anotherthing
#'                                #things"),
#'                      stringsAsFactors = FALSE)
#' feature_skip_ngrams(tweets, status_id, text)
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr pivot_wider
#' @importFrom rlang quo_text
#' @export
feature_skip_ngrams <- function(data, doc_id_field, text_field,
                                n_ngrams_min = 3L, n_ngrams_max = 7L,
                                n_skip_words = 4L, top_num = 1000L) {
  data_temp <- data %>%
    unnest_tokens(skipgrams, text, token = "skip_ngrams",
                  n_min = n_ngrams_min,
                  n = n_ngrams_max,
                  k = n_skip_words) %>%
    mutate(skipgrams = tolower(skipgrams),
           num_words = str_count(skipgrams, "[\\w]+[[:punct:]]*[\\w]*")) %>%
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
    pivot_wider(names_from = skipgrams, values_from = skipgram_proportion,
                values_fill = 0)

  data <- data %>%
    dplyr::select({{ doc_id_field }}) %>%
    left_join(data_temp, by = rlang::quo_text(enquo(doc_id_field)))

  data[is.na(data)] <- 0
  return(data)
}

#' #' Word correlation features
#' #'
#' #' Engineers features related to word correlations in Tweets.
#' #'
#' #' @return A data frame of document ids their associated word correlation features
#' #' @details Tweets are first converted to lowercase before the features are
#' #'   calculated. The top 50% of words used in all
#' #'   Tweets are then retained.
#' #'
#' #'   The word correlation features are calculated as a proportion of the total number of
#' #'   bigrams in the Tweet:
#' #'
#' #'   \itemize{
#' #'     \item{the number of the top_num word pairs occurrences}
#' #'   }
#' #'
#' #' @inheritParams feature_hashtags
#' #' @param top_num integer, the top n word correlations to create features from
#' #' @examples
#' #' tweets <- data.frame(status_id = c(1234, 5678),
#' #'                      text = c("I tweet about one thing #onething #things",
#' #'                               "I tweet about another #anotherthing
#' #'                                #things"),
#' #'                      stringsAsFactors = FALSE)
#' #' feature_wordcors(tweets, status_id, text)
#' #' @import dplyr
#' #' @importFrom tidytext unnest_tokens
#' #' @importFrom tidyr pivot_wider
#' #' @importFrom rlang quo_text
#' #' @importFrom widyr pairwise_cor
#' #' @export
#' feature_wordcors <- function(data, doc_id_field, text_field, top_num = 1000) {
#'   tweet_ngrams <- data %>%
#'     unnest_tokens(ngram, {{ text_field }}, token = "ngrams", n = 2) %>%
#'     group_by({{ doc_id_field }}) %>%
#'     summarise(ngrams_in_tweet = n()) %>%
#'     select({{ doc_id_field }}, ngrams_in_tweet)
#'
#'   data_temp <- data %>%
#'     unnest_tokens(word, {{ text_field }},
#'                   token = "tweets", to_lower = FALSE,
#'                   strip_punct = TRUE) %>%
#'     mutate(word = tolower(word)) #%>%
#'     # filter(!(word %in% stopwords::stopwords("en")))
#'
#'
#'
#'   top50percent <- data_temp %>%
#'     count(word, sort = TRUE) %>%
#'     filter(n > mean(n)) %>%
#'     pull(word)
#'
#'   data_temp <- data_temp %>%
#'     filter(word %in% top50percent) %>%
#'     pairwise_cor(word, {{ doc_id_field }}, sort = TRUE) %>%
#'     top_n(top_num, correlation) %>%
#'     filter(row_number() %% 2 == 1) %>%
#'     transmute(search_pairs = paste(item1, item2, collapse = "|"))
#'   data <- data %>%
#'     mutate(count = sum(str_count({{ text_field }}, data_temp$search_pairs)))
#'   return(data)
#'
#' }
