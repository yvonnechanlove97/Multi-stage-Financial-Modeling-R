clean_text <- function(text_reviews) {
  text_reviews <- gsub(text_reviews, pattern = "\\.{2,}|!{1,}",
                       replacement = ". ")
  text_reviews <- gsub(text_reviews, pattern = "<[/\ ]*.*[/\ ]*>",
                       replacement = " ")
  text_reviews <- gsub(text_reviews, pattern = "[/;:\\[\\]+]|\\-{1,}|\\(|\\)",
                       replacement = " ")
  text_reviews <- gsub(text_reviews, pattern = "\\\\", replacement = "")
  text_reviews <- gsub(text_reviews, pattern = "`", replacement = "'")
  text_reviews <- gsub(text_reviews, pattern = "([a-z])\\.([a-z])",
                       replacement = "\\1. \\2")
  return(text_reviews)
}

parse_amod_nsubj_neg <- function(dep_row, tokens) {
  t1_cond <- tokens$id == dep_row[1] & tokens$sid == dep_row[2] &
    tokens$tid == dep_row[4]
  t2_cond <- tokens$id == dep_row[1] & tokens$sid == dep_row[2] &
    tokens$tid == dep_row[3]
  t1 <- tokens$lemma[t1_cond]
  t2 <- tokens$lemma[t2_cond]
  pos1 <- tokens$upos[t1_cond]
  pos2 <- tokens$upos[t2_cond]
  return(
    c(dep_row[1], dep_row[2], t1, t2, dep_row[5], pos1, pos2, tokens$sid[t1_cond],
      tokens$sid[t2_cond], tokens$tid[t1_cond], tokens$tid[t2_cond]))
}

parse_neg_acomp <- function(dep_row, tokens) {
  t1_cond <- tokens$id == dep_row[1, 1] & tokens$sid == dep_row[1, 2] &
    tokens$tid == dep_row[1, 4]
  t2_cond <- tokens$id == dep_row[1, 1] & tokens$sid == dep_row[1, 2] &
    tokens$tid == dep_row[1, 7]
  t1 <- tokens$lemma[t1_cond]
  t2 <- tokens$lemma[t2_cond]
  pos1 <- tokens$upos[t1_cond]
  pos2 <- tokens$upos[t2_cond]
  return(c(as.character(dep_row[1, 1]), as.character(dep_row[1, 2]), t1, t2,
           as.character(dep_row[1, 5]), pos1, pos2,
           tokens$sid[t1_cond], tokens$sid[t2_cond],
           tokens$tid[t1_cond], tokens$tid[t2_cond]))
}

remove_amod_neg <- function(reqd_tokens, neg_deps) {
  removes_amod <- c()
  reqd_tokens <- data.frame(reqd_tokens, stringsAsFactors = F)
  neg_deps$id <- gsub(as.character(neg_deps$id), pattern = "^ ",
                      replacement = "")
  neg_deps$sid <- gsub(as.character(neg_deps$sid), pattern = "^ ",
                       replacement = "")
  neg_deps$tid <- gsub(as.character(neg_deps$tid), pattern = "^ ",
                       replacement = "")
  for(i in 1:nrow(neg_deps)) {
    idx <- which(reqd_tokens$id == neg_deps$id[i] &
                   reqd_tokens$t2_sid == neg_deps$sid[i] &
                   reqd_tokens$t2_tid == neg_deps$tid[i])
    removes_amod <- c(removes_amod, idx)
  }
  if(length(removes_amod) > 0) {
    reqd_tokens <- reqd_tokens[-removes_amod, ]
  }
  return(reqd_tokens = reqd_tokens)
}

reqd_upos <- c("ADJ")
dep_type <- c("amod", "neg", "acomp", "advmod")

extract_phrases <- function(dependencies, tokens) {
  tokens$id <- gsub(as.character(tokens$id), pattern = "^ ", replacement = "")
  tokens$sid <- gsub(as.character(tokens$sid), pattern = "^ ", replacement = "")
  tokens$tid <- gsub(as.character(tokens$tid), pattern = "^ ", replacement = "")
  dep_subset <- dependencies[dependencies$relation %in% dep_type, ]
  if(nrow(dep_subset) > 0) {
    dep_subset <- split(dep_subset, dep_subset$relation)
    reqd_tokens <- data.frame()
    reqd_tokens2 <- data.frame()
    amod_cond <- "amod" %in% names(dep_subset)
    nsubj_cond <- "nsubj" %in% names(dep_subset)
    advmod_cond <- "advmod" %in% names(dep_subset)
    amod_nsubj_cond <- amod_cond | nsubj_cond | advmod_cond
    neg_cond <- "neg" %in% names(dep_subset)
    if(amod_nsubj_cond) {
      df <- rbind(dep_subset[["amod"]], dep_subset[["nsubj"]], dep_subset[["advmod"]])
      reqd_tokens <- do.call(rbind, lapply(1:nrow(df), function(i) {
        dep_row <- df[i,]
        toks <- tokens[tokens$id == dep_row[1, 1] &
                         tokens$sid == dep_row[1, 2], ]
        dep_row <- gsub(as.character(dep_row), pattern = "^ ", replacement = "")
        res <- parse_amod_nsubj_neg(dep_row, tokens)
        return(res)
      }))
      colnames(reqd_tokens) <-
        c("id", "sid", "t1", "t2", "relation", "t1_pos", "t2_pos",
          "t1_sid", "t2_sid", "t1_tid", "t2_tid")
      if(neg_cond)
        reqd_tokens <- remove_amod_neg(reqd_tokens, dep_subset[["neg"]])
      reqd_tokens <- data.frame(reqd_tokens)
      # reqd_tokens <- reqd_tokens[sapply(reqd_tokens$t2_pos,
      #                                   function(upos) upos %in% reqd_upos), ]
    }
    if(neg_cond) {
      reqd_tokens2 <-
        do.call(rbind,lapply(1:nrow(dep_subset[["neg"]]), function(i) {
        dep_row <- gsub(dep_subset[["neg"]][i, ], pattern = "^ ",
                        replacement = "")
        dep_row <- matrix(dep_row, nrow = 1)
        colnames(dep_row) <- colnames(dep_subset[["neg"]])
        if("acomp" %in% names(dep_subset)) {
          dep_row1 <- merge(dep_row, dep_subset[["acomp"]],
                            by = c("id", "sid", "tid"))
          if(nrow(dep_row1) > 0) {
            res <- parse_neg_acomp(dep_row1, tokens)
          } else {
            res <- parse_amod_nsubj_neg(dep_row, tokens)
          }
        } else {
          res <- parse_amod_nsubj_neg(dep_row, tokens)
        }
        return(res)
      }))
      colnames(reqd_tokens2) <-
        c("id", "sid", "t1", "t2", "relation", "t1_pos", "t2_pos",
          "t1_sid", "t2_sid", "t1_tid", "t2_tid")
      reqd_tokens2 <- data.frame(reqd_tokens2)
      # if(nrow(reqd_tokens2) > 0)
        # reqd_tokens2 <- reqd_tokens2[
        #   sapply(reqd_tokens2$t2_pos, function(upos) upos %in% reqd_upos), ]
    }
    reqd_tokens <- rbind(reqd_tokens, reqd_tokens2)
    reqd_tokens <- data.frame(reqd_tokens, stringsAsFactors = F)
    return(reqd_tokens)
  } else {
    return(data.frame(stringsAsFactors = F))
  }
}

merge_vecs <- function(vec1, vec2) {
  common <- intersect(names(vec1), names(vec2))
  if(length(common) > 0) {
    vec1[common] <- vec1[common] + vec2[common]
  }
  vec <- c(vec1[common], vec1[setdiff(names(vec1), common)],
           vec2[setdiff(names(vec2), common)])
  return(vec)
}

get_corpus <- function(text) {
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

get_tfidf <- function(text, thr = 20, norm = F) {
  corpus <- get_corpus(text)
  summary_tfidf <- TermDocumentMatrix(corpus, control = list(
    weighting = function(x) weightTfIdf(x, normalize = norm),
    stopwords = T, bounds = list(global = c(thr, Inf))))
  summary_tfidf <- as.matrix(summary_tfidf)
  return(t(summary_tfidf))
}

#' @title Document Term Matrix
#' @description Returns the document term matrix of given text input preprocessed using `FinancialModelingR`
#'
#' @param full_df Path to a CSV file with export sales report
#' @return Data frame: Document term matrix
#' @examples
#' price_df <- read_exports("raw_data/ExportSalesDataByCommodity(Soybeans).csv", skip_lines = 4)
#' @export
get_dtm <- function(text, thr = 20, norm = F) {
  corpus <- get_corpus(text)
  summary_tfidf <- TermDocumentMatrix(corpus, control = list(
    weighting = function(x) weightTf(x),
    stopwords = T, bounds = list(global = c(thr, Inf))))
  summary_tfidf <- as.matrix(summary_tfidf)
  return(data.frame(t(summary_tfidf)))
}

# get_dtm <- function(full_df) {
#   splits <- split(full_df, full_df$id)
#   ret <- do.call(rbind.fill, lapply(splits, function(split) get_dtm_row(split)))
#   rownames(ret) <- names(splits)
#   ret <- data.frame(as.matrix(ret))
#   return(ret)
# }

get_dtm_row <- function(full_df) {
  amod_vec <- c()
  neg_vec <- c()
  other_vec <- c()
  nsubj_vec <- c()
  df <- split(full_df, full_df$relation)
  if("amod" %in% names(df)) {
    amod_vec <- table(c(df[["amod"]]$t1, df[["amod"]]$t2))
  }
  if("neg" %in% names(df)) {
    neg_vec <- -table(df[["neg"]]$t2)
  }
  if("other" %in% names(df)) {
    other_vec <- table(df[["other"]]$t1)
  }
  if("nsubj" %in% names(df)) {
    nsubj_vec <- table(c(df[["nsubj"]]$t1, df[["nsubj"]]$t2))
  }
  vec <- merge_vecs(amod_vec, neg_vec)
  vec <- merge_vecs(vec, other_vec)
  vec <- merge_vecs(vec, nsubj_vec)
  df <- data.frame(matrix(vec, nrow = 1))
  colnames(df) <- names(vec)
  return(df)
}

get_polarity_df <- function(text_reviews, save_file = "review_polarity.Rds") {
  reqd_pos <- c("JJ", "JJS", "JJR")
  indices <- 1:length(text_reviews)

  t1 <- Sys.time()
  reqd <- do.call(rbind, lapply(indices, function(i) {
    review <- text_reviews[i]
    annotated <- cnlp_annotate(input = review)
    dependencies <- annotated$dependency
    tokens <- annotated$token
    lemma_repl <- grep(tokens$lemma, pattern = "^-")
    tokens$lemma[lemma_repl] <- gsub(tokens$word[lemma_repl], pattern = "^-",
                                     replacement = "")
    phrase_words <- data.frame()
    phrases <- extract_phrases(dependencies, tokens)
    factors <- sapply(phrases, class) == "factor"
    if(sum(factors) > 0) {
      phrases[, factors] <- sapply(phrases[, factors], as.character)
    }
    if(nrow(phrases) > 0) {
      phrase_words <- phrases[, c("id", "t1", "t2", "relation")]
      phrase_words <- phrase_words[nchar(phrase_words$t1) > 2 &
                                     nchar(phrase_words$t2) > 2, ]
    }
    other_words_df <- data.frame()
    tokens$sid <- as.character(tokens$sid)
    tokens$tid <- as.character(tokens$tid)
    tokens$word <- as.character(tokens$word)
    tokens$lemma <- as.character(tokens$lemma)
    tokens$upos <- as.character(tokens$upos)
    tokens$pos <- as.character(tokens$pos)
    other_words <- tokens[sapply(tokens$pos, function(pos)
      pos %in% reqd_pos) & sapply(tokens$lemma, function(lemma)
        !(lemma %in% c(phrase_words$t1, phrase_words$t2))),
      c("id", "lemma")]
    if(nrow(other_words) > 0) {
      other_words_df <- data.frame(id = other_words$id, t1 = other_words$lemma,
                                   t2 = "", relation = "other",
                                   stringsAsFactors = F)
      factors <- sapply(other_words_df, class) == "factor"
      if(sum(factors) > 0) {
        other_words_df[, factors] <- sapply(other_words_df[, factors],
                                            as.character)
      }
      other_words_df <- other_words_df[nchar(other_words_df$t1) > 2, ]
    }
    full_df <- rbind(phrase_words, other_words_df)
    if(nrow(full_df) > 0) {
      pols <- polarity(paste(full_df$t1, full_df$t2))
      full_df$polarity <- pols$all$polarity
      doc_polarity <- data.frame(
        full_df %>% dplyr::group_by(id) %>%
          dplyr::summarize(avg_polarity = mean(polarity),
                           max_polarity = max(polarity),
                           min_polarity = min(polarity),
                           sd_polarity = sd(polarity),
                           useful_text = paste0(paste(t1, t2),
                                                collapse = " ")))
      rownames(doc_polarity) <- doc_polarity$id
      doc_polarity$id <- NULL
      doc_polarity <- doc_polarity[paste0("doc", 1:length(i)), ]
    } else {
      doc_polarity <- data.frame(matrix(nrow = 0, ncol = 6))
      colnames(doc_polarity) <- c("id", "avg_polarity", "max_polarity",
                                  "min_polarity", "sd_polarity", "useful_text")
    }
    return(doc_polarity)
  }))
  t2 <- Sys.time()
  reqd$sd_polarity[is.na(reqd$sd_polarity)] <- 0
  saveRDS(reqd, save_file)
  return(reqd)
}

#' @title Preprocess Tweets
#' @description Read and preprocess standard Tweet input CSV file
#'
#' @param file Location of Tweet CSV file
#' @param processed Parameter in c("processed", "unprocessed"). Performs cleaning based on part-of-speech and dependencies using spaCy and extracts sentiments if this is set to "processed". Otherwise it directly extracts sentiments on the whole Tweet
#' @param stopwords_file Text file containing stopwords - one in each line
#' @return Data frame with sentiment summary of each Tweet in text file
#' @examples
#' df = process_text(path="raw_data/Tweets/China tweets @realDonaldTrump.csv")
#' @export
process_text <- function(file, processed = "processed", stopwords_file = NULL) {
  files <- list.files()
  if(!(paste0(processed, "_", file, ".Rds") %in% files)) {
    reviews <- data.frame(fread(file), stringsAsFactors = F)
    text_reviews <- as.character(reviews$text)

    text_reviews <- tolower(text_reviews)

    text_reviews <- clean_text(text_reviews)
    if(processed != "processed") {
      review_polarity <- sapply(1:length(text_reviews), function(i) {
        review <- text_reviews[i]
        review <- clean_text(review)
        corpus <- Corpus(VectorSource(review))
        corpus <- tm_map(corpus, tolower)
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        if(!is.null(stopwords_file)) {
          corpus <- tm_map(corpus, removeWords, readLines(stopwords_file))
        }
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, stemDocument, language="english")
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        review <- corpus[[1]]$content
        # annotated <- cnlp_annotate(input = review)
        # sentences <- c()
        # for(sid1 in unique(annotated$token$sid)) {
        #   annotated1 <- annotated$token[annotated$token$sid == sid1, ]
        #   annotated1$word[annotated1$lemma != "-PRON-"] <-
        #     annotated1$lemma[annotated1$lemma != "-PRON-"]
        #   annotated1 <- annotated1[!is.na(annotated1$cid), ]
        #   sentences <- c(sentences, paste0(annotated1$word, collapse = " "))
        # }
        sentences <- sent_detect(review)
        pols <- sapply(sentences, function(sentence) polarity(sentence))
        min_max_polarity <- range(sapply(pols, function(polar) polar$polarity))
        pol <- polarity(sentences)
        df <- data.frame(avg_polarity = pol$group$ave.polarity,
                         max_polarity = min_max_polarity[2],
                         min_polarity = min_max_polarity[1],
                         sd_polarity = pol$group$sd.polarity,
                         useful_text = paste0(sentences, collapse = ". "),
                         stringsAsFactors = F)
        return(df)
      })
      review_polarity <- data.frame(
        avg_polarity = unlist(review_polarity["avg_polarity", ]),
        max_polarity = unlist(review_polarity["max_polarity", ]),
        min_polarity = unlist(review_polarity["min_polarity", ]),
        sd_polarity = unlist(review_polarity["sd_polarity", ]),
        useful_text = unlist(review_polarity["useful_text", ]),
        stringsAsFactors = F)
      saveRDS(review_polarity, paste0(processed, "_", file, ".Rds"))
    } else {
      review_polarity <- get_polarity_df(
        text_reviews, save_file = paste0(processed, "_", file, ".Rds"))
    }
  }
  else {
    review_polarity <- readRDS(paste0(processed, "_", file, ".Rds"))
  }
  rm(list = setdiff(ls(), lsf.str()))
  gc()
}


# Topic modeling code is not working right now. Gotta fix this!
# t1 <- Sys.time()
# corpus <- Corpus(VectorSource(reqd$useful_text[!is.na(reqd$useful_text)]))
# tfidf <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf),
#   removeNumbers = F, stemming = F, stopwords = F, bounds = list(global = c(1, Inf))))
# # rowSums(tfidf)
# lda_model <- LDA(tfidf, k = 20, control = list(seed = 1))
# probs <- posterior(lda_model, tfidf)$topics
# t2 <- Sys.time()
# print(t2 - t1)

# remove_cols <- colnames(reqd) == "not" | colnames(reqd) == "nocolumn"
# if(sum(remove_cols) > 0)
#   reqd <- reqd[, -which(remove_cols)]
# writeLines(colnames(reqd), "all_cols.txt")
# totals <- colSums(reqd, na.rm = T)
# reqd[is.na(reqd)] <- 0
#
# # Not yet implemented
# # This section is for correcting spellings of columns
# spell_mistakes <- colnames(reqd)[!hunspell::hunspell_check(colnames(reqd))]
# library(fuzzywuzzyR)
# init <- FuzzMatcher$new()
# init$Ratio(string1 = "", string2 = "")
# init$Partial_ratio(string1 = "", string2 = "")
# init$Partial_token_set_ratio(string1 = "", string2 = "")
# init$Partial_token_sort_ratio(string1 = "", string2 = "")
#
# bool <- sapply(reqd, function(col) any(col < 0, na.rm = T))
# colnames(reqd)[bool][1]
# which(reqd[, bool][, 1005] < 0)
# colnames(reqd)[bool][1005]
#
# # all_deps <- unique(dependencies$relation)


# ratio <- length(text_reviews)/100
# max_len <- 100
# if(length(text_reviews) > 500) {
#   if(ratio <= max_len) {
#     indices <- split(idxs, cut(idxs, breaks = 100))
#   } else {
#     tots <- ceiling(ratio)
#     vec <- do.call(c, lapply(1:tots, function(i) rep(i, max_len)))
#     vec <- vec[idxs]
#     indices <- split(idxs, vec)
#   }
# } else {
#   indices <- list()
#   indices[[1]] <- idxs
# }

