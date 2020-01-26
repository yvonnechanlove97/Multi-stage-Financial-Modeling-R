## -----------------------------------------------------------------------------
library(FinancialModelingR)

## -----------------------------------------------------------------------------
contractsForJuly2020 <- read_price(
  in_file = "private_data/ActiveSoybeanContractsforJuly2020.xlsx", delta_price = T,
  add_delta = T, subset = T, subset_min_date = "2017-01-01",
  rename_price_columns = T, rename_prefix = "july_2020_", skip_lines = 3)
contractsForJuly2020$Date <- as.Date(contractsForJuly2020$Date, "%Y-%m-%d")

## -----------------------------------------------------------------------------
tweet_df <- readRDS("private_data/text_features.Rds")
colnames(tweet_df)[1] <- "Date"
sds <- sapply(tweet_df[, 2:ncol(tweet_df)], sd)
remove_cols <- 1 + which(sds == 0)
if(length(remove_cols) > 0) {
  tweet_df <- tweet_df[, -remove_cols]
}
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

