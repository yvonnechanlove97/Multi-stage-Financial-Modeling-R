#' @title Read Price from Excel File
#' @description Reads an Excel file with Date, Open, High, Low and Close prices of a stock or future contract
#'
#' @param file Path to a Excel file with Date, Open, High, Low and Close prices
#' @param delta_price Flag indicating whether change in price should be calculated
#' @param subset Flag indicating whether only a subset of the data should be used
#' @param subset_min_date Character string indicating the minimum date to be included in subset after reading the price file
#' @param skip_lines Number of lines above the header line in the Excel file
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' price_df <- read_price(in_file, delta_price = F, subset = F, skip_lines = 3)
#' delta_price_df <- read_price(in_file, delta_price = T, subset = F, skip_lines = 3)
#' @export
read_price <- function(in_file, delta_price = F, add_delta = F, subset = F,
                       subset_min_date = "2017-01-01", skip_lines = 3,
                       rename_price_columns = F, rename_prefix = NULL) {
  prices <- data.frame(read_excel(in_file, skip = skip_lines))
  prices$Date <- as.Date(prices$Date, format = "%Y-%m-%d")
  prices <- prices[order(prices$Date), ]
  if(delta_price) {
    if(add_delta) {
      prices$delta_Open <- c(NA, diff(prices$Open))
      prices$delta_Close <- c(NA, diff(prices$Close))
      prices$delta_High <- c(NA, diff(prices$High))
      prices$delta_Low <- c(NA, diff(prices$Low))

      prices$delta_Open_sign <- sign(prices$delta_Open)
      prices$delta_Close_sign <- sign(prices$delta_Close)
      prices$delta_High_sign <- sign(prices$delta_High)
      prices$delta_Low_sign <- sign(prices$delta_Low)
    } else {
      prices$Open <- c(NA, diff(prices$Open))
      prices$Close <- c(NA, diff(prices$Close))
      prices$High <- c(NA, diff(prices$High))
      prices$Low <- c(NA, diff(prices$Low))
    }
  }
  if(subset) {
    prices <- prices[prices$Date >= as.Date(subset_min_date, format = "%Y-%m-%d"), ]
  }
  if(rename_price_columns) {
    colnames(prices)[2:ncol(prices)] <-
      paste0(rename_prefix, colnames(prices)[2:ncol(prices)])
  }
  rownames(prices) <- NULL
  return(prices)
}

# get_features <- function(df, prices) {
#   df <- merge(df, prices, by.x = "created_at", by.y = "Date")
#   df <- df[, sapply(df, sd) != 0]
#   return(df)
# }
#
# back_fill_na <- function(series) {
#   na_idx <- which(is.na(series))
#   while(length(na_idx) > 0) {
#     series[na_idx] <- series[na_idx + 1]
#     na_idx <- which(is.na(series))
#   }
#   return(series)
# }
#
# get_residues <- function(df, prices, model) {
#   pred_df <- data.frame(Date = df$created_at,
#                         pred_Close = predict(model, df),
#                         stringsAsFactors = F)
#   prices$Date_cut <- as.character(cut(x = prices$Date,
#                                       breaks = pred_df$Date))
#   prices$Date_cut[nrow(prices)] <-
#     as.character(pred_df$Date[nrow(pred_df)])
#   prices$Date_cut[is.na(prices$Date_cut)] <-
#     prices$Date_cut[!is.na(prices$Date_cut)][1]
#   prices$Date1 <- as.Date(prices$Date_cut, format = "%Y-%m-%d")
#   prices <- merge(prices, pred_df, on.x = "Date1",
#                   on.y = "Date", all.x = T)
#   prices$Date1 <- prices$Date_cut <- NULL
#
#   prices$pred_Close <- backfill_na(prices$pred_Close)
#
#   prices$Close <- prices$Close - prices$pred_Close
#   prices$pred_Close <- NULL
#   return(prices)
# }

forward_fill_na <- function(series) {
  for(i in 2:length(series)) {
    if(is.na(series[i])) {
      series[i] <- series[i - 1]
    }
  }
  return(series)
}
