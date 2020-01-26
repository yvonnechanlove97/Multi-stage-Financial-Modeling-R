#' @title Machine Learning Modeling
#' @description Builds machine learning model for a given granularity of data by merging with the daily price data. The inputs can be provided either as x, y, model_type, test_x where the data is considered as merged (independent variables may have missing values) or as
#'
#' @param x Data frame containing date column and independent variables with or without missing values
#' @param y Series of outcome variable values
#' @param test_x Data frame containing test set independent variables
#' @param test_y Series of test outputs
#' @param daily_price_df Alternate data frame for modeling: daily price data frame containing date column and price
#' @param other_granularity_df Alternate data frame for modeling: independent variable data frame containing date column and independent variables
#' @param daily_price_df_date_col Name of date column in `daily_price_df`
#' @param other_granularity_df_date_col Name of date column in `other_granularity_df`
#' @param independent_variables Names of independent variables in `other_granularity_df`. All columns in `other_granularity_df` are considered if this is left as NULL. This parameter is not considered if x, y, model_type, test_x are provided
#' @param dependent_variable Name of dependent variable in `daily_price_df`
#' @param fill_missing_with_0 Flag indicating whether missing values in merged data frame should be replaced with 0
#' @param lag Number of days to lag the series for modeling. For example, if lag = 1 yesterday's independent variable values are used for predicting today's price
#' @param model_type Type of machine learning model to be built. Currently `rpart` and `lm` are supported
#' @return List containing the model, predictions for training set and predictions for test set along with residues
#' @examples
#'
#' @export

build_model <- function(x = NULL, y = NULL, test_x = NULL, test_y = NULL,
                        daily_price_df, other_granularity_df,
                        daily_price_df_date_col,
                        other_granularity_df_date_col,
                        independent_variables,
                        dependent_variable,
                        fill_missing_with_0 = FALSE,
                        lag = 1, model_type = "rpart") {
  keep_pattern <- "[^a-zA-Z0-9]"
  if(is.null(x)) {
    merged_df <- merge_price_other_df(
      daily_price_df = daily_price_df,
      daily_price_df_date_col = daily_price_df_date_col,
      other_granularity_df = other_granularity_df,
      other_granularity_df_date_col = other_granularity_df_date_col,
      lag = 1)
    reqd_col <- which(colnames(daily_price_df) == daily_price_df_date_col)
    colnames(daily_price_df)[reqd_col] <- "Date"
    rownames(daily_price_df) <- NULL
    if(fill_missing_with_0) {
      merged_df[is.na(merged_df)] <- 0
    }
    test_seq <- seq(from = nrow(merged_df) - 29, to = nrow(merged_df), by = 1)
    test_x <- merged_df[test_seq, ]
    merged_df <- merged_df[-test_seq, ]
    colnames(merged_df) <- gsub(x = colnames(merged_df),
                                pattern = keep_pattern, replacement = ".")
    colnames(other_granularity_df) <-
      gsub(x = colnames(other_granularity_df),
           pattern = keep_pattern, replacement = ".")
    if(is.null(independent_variables)) {
      independent_variables <- setdiff(colnames(other_granularity_df), "Date")
    }
  } else {
    x <- data.frame(sapply(x, forward_fill_na))
    test_x <- data.frame(sapply(test_x, forward_fill_na))
    colnames(x) <- colnames(test_x) <-
      gsub(x = colnames(x), pattern = "[^a-zA-Z0-9]", replacement = ".")
    merged_df <- cbind(x, y)
    colnames(merged_df)[ncol(merged_df)] <- "y"
    if(is.null(independent_variables)) {
    independent_variables <- setdiff(colnames(x), "Date")
    } else {
      independent_variables <- setdiff(independent_variables, "Date")
    }
    dependent_variable <- "y"
  }
  independent_variables <- gsub(x = independent_variables, pattern = keep_pattern,
                                replacement = ".")
  independent_variables <- paste0(independent_variables, collapse = " + ")
  dependent_variable <- gsub(x = dependent_variable, pattern = keep_pattern,
                             replacement = ".")
  form <- as.formula(paste0(dependent_variable, " ~ ", independent_variables))
  if(model_type == "rpart") {
    library(rpart)
    library(rpart.plot)
    model <- rpart(form, merged_df)
  } else if(model_type == "lm") {
    model <- lm(form, merged_df)
  }
  train_pred <- predict(model, merged_df)
  test_pred <- predict(model, test_x)
  train_pred_df <- data.frame(Date = merged_df$Date, prediction = train_pred)
  test_pred_df <- data.frame(Date = test_x$Date, prediction = test_pred)
  if(!is.null(x)) {
    train_pred_df <- merge(merged_df, train_pred_df, by = "Date", all.x = T)
    test_pred_df <- merge(merged_df, test_pred_df, by = "Date", all.x = T)
  } else {
    reqd_cols <- c("Date", "prediction")
    train_pred_df <- merge(daily_price_df[-test_seq, ],
                           train_pred_df,
                           by = "Date", all.x = T)
    test_pred_df <- merge(daily_price_df[test_seq, ],
                          test_pred_df,
                          by = "Date", all.x = T)
    colnames(train_pred_df) <- gsub(colnames(train_pred_df),
                                    pattern = keep_pattern,
                                    replacement = ".")
    colnames(test_pred_df) <- gsub(colnames(test_pred_df),
                                   pattern = keep_pattern,
                                   replacement = ".")
  }
  train_pred_df$prediction <- forward_fill_na(train_pred_df$prediction)
  train_pred_df$residue <-
    train_pred_df[, dependent_variable] - train_pred_df$prediction
  test_pred_df$prediction <- forward_fill_na(test_pred_df$prediction)
  test_pred_df$residue <-
    test_pred_df[, dependent_variable] - test_pred_df$prediction
  return(list(model = model, train_pred_df = train_pred_df,
              test_pred_df = test_pred_df))
}
