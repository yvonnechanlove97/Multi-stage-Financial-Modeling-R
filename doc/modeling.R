## -----------------------------------------------------------------------------
library(FinancialModelingR)

## -----------------------------------------------------------------------------
contractsForJuly2020 <- readRDS("preprocessed_data/contractsForJuly2020.Rds")

## -----------------------------------------------------------------------------
tweet_df <- readRDS("preprocessed_data/tweet_df.Rds")
tweet_df$china.jobs <- apply(tweet_df[, 2:ncol(tweet_df)], 1, function(row) {
  return(as.integer((row["china"] * row["jobs"]) > 0))
})
tweet_df <- data.frame(tweet_df)
print(head(tweet_df))

## ----warning=F, message=F-----------------------------------------------------
library(dplyr)
data("soybeanExports", package = "FinancialModelingR")
competitors <- c("ARGENTINA", "BRAZIL")
df_total_export <- soybeanExports %>% group_by(Country) %>%
  summarize(Total_Export = sum(Weekly_Exports, na.rm = T))
top_countries <- head(x = df_total_export$Country[
  order(df_total_export$Total_Export, decreasing = TRUE)], n = 5)
selected_countries <- c(competitors, top_countries)
df_top_export <- soybeanExports[sapply(
  soybeanExports$Country, function(country) country %in% selected_countries), ]

## -----------------------------------------------------------------------------
data("soybeanCombinedWASDE")
soybeanWASDE_clean <- clean_wasde(combined_data = soybeanCombinedWASDE)

## -----------------------------------------------------------------------------
pred_model_list <- build_model(daily_price_df = contractsForJuly2020,
                               other_granularity_df = soybeanWASDE_clean,
                               daily_price_df_date_col = "Date",
                               other_granularity_df_date_col = "Date",
                               independent_variables = "Area Planted",
                               dependent_variable = "july_2020_Close",
                               lag = 1, model_type = "lm")
print(pred_model_list$model)
print(head(pred_model_list$train_pred_df))
print(head(pred_model_list$test_pred_df))

## -----------------------------------------------------------------------------
pred_model_list1 <- build_model(daily_price_df = contractsForJuly2020,
                                other_granularity_df = tweet_df,
                                daily_price_df_date_col = "Date",
                                other_granularity_df_date_col = "Date",
                                independent_variables = c("china", "trade",
                                                          "money", "deal",
                                                          "tariffs", "economy",
                                                          "currency",
                                                          "china.jobs"),
                                fill_missing_with_0 = TRUE,
                                dependent_variable = "july_2020_Close",
                                lag = 1, model_type = "lm")
print(pred_model_list1$model)
train_pred_df <- pred_model_list1$train_pred_df
test_pred_df <- pred_model_list1$test_pred_df

## -----------------------------------------------------------------------------
y <- train_pred_df$residue
x <- merge(contractsForJuly2020[, c("Date", "july_2020_Close")],
           soybeanWASDE_clean, all.x = T)
fill_cols <- setdiff(colnames(soybeanWASDE_clean), "Date")
# soybeanWASDE_clean[, fill_cols] <- sapply(soybeanWASDE_clean[, fill_cols],
#                                           forward_fill_na)
test_x <- x[642:671, ]
x <- x[1:641, ]
test_y <- test_pred_df$residue

pred_model_list2 <- build_model(x = x,
                                y = y,
                                test_x = test_x,
                                test_y = test_y,
                                independent_variables = c("Area Planted"),
                                fill_missing_with_0 = F,
                                dependent_variable = "july_2020_Close",
                                lag = 1, model_type = "lm")

