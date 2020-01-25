#' @title Machine Learning Modeling
#' @description Builds machine learning model for a given granularity of data by merging with the daily price data. The inputs can be provided either as x, y, model_type, test_x where the data is considered as merged (independent variables may have missing values) or as
#'
#' @param x Data frame containing date column and independent variables with or without missing values
#' @param y Series of outcome variable values
#' @param model_type Type of machine learning model to be built. Currently `rpart` and `lm` are supported
#' @param n Number of days to lead and lag around week ending. Prices are compared between `week_end + n` and `week_end - n`
#' @param daily_price_df Alternate data frame for modeling: daily price data frame containing date column and price
#' @param other_granularity_df Alternate data frame for modeling: independent variable data frame containing date column and independent variables
#' @param daily_price_df_date_col Name of date column in `daily_price_df`
#' @param other_granularity_df_date_col Name of date column in `other_granularity_df`
#' @param independent_variables Names of independent variables in `other_granularity_df`. All columns in `other_granularity_df` are considered if this is left as NULL
#' @param dependent_variable Name of dependent variable in `daily_price_df`
#' @param lag Number of days to lag the series for modeling. For example, if lag = 1 yesterday's independent variable values are used for predicting today's price
#' @return List containing the model, predictions for training set and predictions for test set along with residues
#' @examples
#'
#' @export

build_model <- function(x = NULL, y = NULL, model_type = "rpart",
                        test_x,
                        daily_price_df, other_granularity_df,
                        daily_price_df_date_col,
                        other_granularity_df_date_col,
                        independent_variables,
                        dependent_variable,
                        lag = 1) {
  if(is.null(x)) {
    other_granularity_df[, other_granularity_df_date_col] <-
      as.Date(other_granularity_df[, other_granularity_df_date_col]) - lag
    daily_price_df[, daily_price_df_date_col] <-
      as.Date(daily_price_df[, daily_price_df_date_col])
    date_col_idx <- which(colnames(daily_price_df) == daily_price_df_date_col)
    colnames(daily_price_df)[date_col_idx] <- "Date"
    date_col_idx <- which(colnames(other_granularity_df) == other_granularity_df_date_col)
    colnames(other_granularity_df)[date_col_idx] <- "Date"
    merged_df <- merge(daily_price_df, other_granularity_df, by = "Date")
    test_seq <- seq(from = nrow(merged_df) - 29, to = nrow(merged_df), by = 1)
    test_x <- merged_df[test_seq, ]
    merged_df <- merged_df[-test_seq, ]
    if(is.null(independent_variables)) {
      independent_variables <- setdiff(colnames(other_granularity_df), "Date")
    }
  } else {
    x <- data.frame(sapply(x, forward_fill_na))
    merged_df <- cbind(x, y)
    colnames(merged_df)[ncol(merged_df)] <- "y"
    independent_variables <- setdiff(colnames(x), "Date")
    dependent_variable <- "y"
  }
  independent_variables <- paste0(independent_variables, collapse = " + ")
  form <- as.formula(paste0(dependent_variable, " ~ ", independent_variables))
  if(model_type == "rpart") {
    model <- rpart(form, merged_df)
  } else if(model_type == "lm") {
    model <- lm(form, merged_df)
  }
  train_pred <- predict(model, merged_df)
  test_pred <- predict(model, test_x)
  train_pred_df <- data.frame(Date = merged_df$Date, prediction = train_pred)
  test_pred_df <- data.frame(Date = test_x$Date, prediction = test_pred)
  if(is.null(x)) {
    train_pred_df <- merge(merged_df, train_pred_df, by = "Date", all.x = T)
    test_pred_df <- merge(merged_df, test_pred_df, by = "Date", all.x = T)
  } else {
    train_pred_df <- merge(daily_price_df[-test_seq, ], train_pred_df,
                           by = "Date", all.x = T)
    test_pred_df <- merge(daily_price_df[test_seq, ], test_pred_df,
                          by = "Date", all.x = T)
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
