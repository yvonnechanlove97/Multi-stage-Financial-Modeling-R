#' @title Sentiment Wordcloud
#' @description Plots wordcloud colored by sentiment of the preprocessed data obtained using `FinancialModelingR`
#'
#' @param df1 Data frame with
#' @return Wordcloud object based on word occurrence frequency colored by average sentiment of word
#' @examples
#' df1 <- readRDS("raw_data/Tweets/unprocessed_China tweets @readDonaldTrump.csv.Rds")
#' plot_wordcloud(df1)
#' @export

plot_wordcloud <- function(df1) {
  corpus <- Corpus(VectorSource(df1$useful_text))
  splits <- lapply(strsplit(df1$useful_text, " "),
                   function(split) split[split != ""])
  splits <- table(unlist(lapply(1:nrow(df1), function(i) {
    word <- "negative"
    if(df1$avg_polarity[i] > 0) {
      word <- "positive"
    }
    return(paste0(splits[[i]], "__", word))
  })))
  dtm <- TermDocumentMatrix(corpus)
  name_splits <- strsplit(names(splits), "__")
  wordcounts <- data.frame(count = as.vector(rollup(dtm, 2, na.rm = T, FUN = sum)))
  wordcounts$positive <- wordcounts$negative <- 0
  rownames(wordcounts) <- dtm$dimnames$Terms
  for(i in 1:length(splits)) {
    split <- name_splits[[i]]
    wordcounts[split[1], split[2]] <- wordcounts[split[1], split[2]] + 1
  }
  wordcounts <- na.omit(wordcounts)
  wordcounts$positive_perc <- wordcounts$positive/
    (wordcounts$positive + wordcounts$negative)
  wordcounts$color <- ifelse(wordcounts$positive_perc > 0.7, "green",
                             ifelse(wordcounts$positive_perc > 0.3,
                                    "orange", "red"))
  # set.seed(1)
  # png("wordcloud.png", width = 1744, height = 1088)
  wordcloud(words = rownames(wordcounts), freq = wordcounts$count, min.freq = 30,
            max.words = 200, random.order = F, rot.per = 0.35,
            colors = wordcounts$color)
  # dev.off()
}
