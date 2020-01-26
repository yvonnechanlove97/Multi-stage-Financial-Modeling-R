#' @title Read Price from Excel File
#' @description Reads an Excel file with Date, Open, High, Low and Close prices of a stock or future contract
#'
#' @param file Path to a Excel file with Date, Open, High, Low and Close prices
#' @param delta_price Flag indicating whether change in price should be calculated
#' @param add_delta Flag indicating whether the change in price should be added as new column. Setting to FALSE replaces the existing price columns
#' @param subset Flag indicating whether only a subset of the data should be used
#' @param subset_min_date Character string indicating the minimum date to be included in subset after reading the price file
#' @param skip_lines Number of lines above the header line in the Excel file
#' @param rename_price_columns Flag indicating whether price columns should be renamed
#' @param rename_prefix Prefix use while renaming price columns
#' @return Data frame with Date column and price columns with given prefix
#' @examples
#' price_df <- read_price(in_file, delta_price = F, subset = F, skip_lines = 3)
#' delta_price_df <- read_price(in_file, delta_price = T, subset = F, skip_lines = 3)
#' @export

read_price <- function(in_file, delta_price = F, add_delta = F, subset = F,
                       subset_min_date = "2017-01-01", skip_lines = 3,
                       rename_price_columns = F, rename_prefix = NULL) {
  library(readxl)
  prices <- data.frame(readxl::read_excel(in_file, skip = skip_lines))
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

#' @title Read Export from CSV File
#' @description Reads an CSV file with Date and export/sales to different countries
#'
#' @param file Path to a CSV file with export sales report
#' @param skip_lines Number of lines to skip on top of the file
#' @return Data frame with Date column and export/sales columns
#' @examples
#' price_df <- read_exports("raw_data/ExportSalesDataByCommodity(Soybeans).csv", skip_lines = 4)
#' @export

read_exports <- function(file, skip_lines) {
  df_export = read.csv(file,
                       na.string = "",
                       skip = skip_lines,
                       stringsAsFactors = FALSE,
                       header = FALSE)

  df_export = remove_empty(df_export, which = c("rows"))
  df_export = remove_empty(df_export, which = c("cols"))
  df_export$V3 = NULL
  colnames(df_export)[c(1:3,12)] = c(as.matrix(df_export[3,c(1:3,12)]))
  colnames(df_export)[4:5] = apply(as.matrix(df_export[2:3,4:5]), 2, paste,
                                   collapse = "_")
  colnames(df_export)[6:11] = apply(as.matrix(df_export[1:3,6:11]), 2, paste,
                                    collapse = "_")
  df_export = df_export[-c(1:3),]
  colnames(df_export) = gsub(" ", "_",colnames(df_export))
  df_export$`Unit Desc` = NULL
  df_export = cbind(df_export[1:3], lapply(df_export[4:11], FUN = function(x)
    as.numeric(gsub(",","",x))))
  rm_unk = which(df_export$Country == "UNKNOWN")
  rm_knw = which(df_export$Country == "KNOWN")
  rm_gt = which(df_export$Country == "GRAND TOTAL")
  df_export = df_export[-c(rm_unk, rm_knw, rm_gt),]
  return(df_export)
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


#' @title Merge Different Granularities
#' @description Merges data frames of different time granularities and fills missing values with previous non-missing value
#'
#' @param daily_price_df Alternate data frame for modeling: daily price data frame containing date column and price
#' @param daily_price_df_date_col Name of date column in `daily_price_df`
#' @param other_granularity_df Alternate data frame for modeling: independent variable data frame containing date column and independent variables
#' @param other_granularity_df_date_col Name of date column in `other_granularity_df`
#' @param lag Number of days to lag the series for modeling. For example, if lag = 1 yesterday's independent variable values are used for predicting today's price
#' @return Data frame with merged output of daily price data and other data frame of different granularity. The missing values in other data frame are filled with previous known value
#' @examples
#' merge_price_other_df(daily_price_df = contractsForJuly2020,
#' other_granularity_df = soybeanWASDE_clean,
#' lag = 1)
#' @export
merge_price_other_df <- function(daily_price_df,
                                 daily_price_df_date_col,
                                 other_granularity_df,
                                 other_granularity_df_date_col,
                                 lag = 1) {
  other_granularity_df[, other_granularity_df_date_col] <-
    as.Date(other_granularity_df[, other_granularity_df_date_col]) - lag
  daily_price_df[, daily_price_df_date_col] <-
    as.Date(daily_price_df[, daily_price_df_date_col])
  date_col_idx <- which(colnames(daily_price_df) == daily_price_df_date_col)
  colnames(daily_price_df)[date_col_idx] <- "Date"
  date_col_idx <- which(colnames(other_granularity_df) == other_granularity_df_date_col)
  colnames(other_granularity_df)[date_col_idx] <- "Date"
  daily_price_df <- daily_price_df[order(daily_price_df$Date), ]
  other_granularity_df <- other_granularity_df[order(other_granularity_df$Date), ]
  merged_df <- merge(daily_price_df, other_granularity_df, by = "Date", all.x = T)
  merged_df <- data.frame(sapply(merged_df, forward_fill_na))
  return(merged_df)
}
