## ----warning=F, message=F-----------------------------------------------------
library(FinancialModelingR)
soybeanCropProgress2019 <- readRDS("preprocessed_data/soybeanCropProgress2019.Rds")

## ----fig.width = 7, fig.height = 4--------------------------------------------
contractsForJuly2020 <- readRDS("preprocessed_data/contractsForJuly2020.Rds")
print(head(contractsForJuly2020))

## -----------------------------------------------------------------------------
# DTM = document term matrix
tweet_dtm_df <- readRDS("preprocessed_data/tweet_df.Rds")
print(head(tweet_dtm_df))
price_tweet_dtm_df <- merge(tweet_dtm_df,
                            contractsForJuly2020[, c("Date", "july_2020_Close")],
                            by = "Date")

## ----fig.width = 7, fig.height = 4--------------------------------------------
create_corr_plot(independent_var_names = NULL,
                 dependent_var_names = "july_2020_Close",
                 df = price_tweet_dtm_df,
                 remove_cols = c("amp", "get", "want", "many", "will", "just",
                                 "want", "can", "realdonaldtrump", "created_at",
                                 "made", "going", "must", "make", "Date"))

price_tweet_dtm_df$china_jobs <- apply(price_tweet_dtm_df[, -1], 1, function(row) {
  as.integer((row["china"] * row["jobs"]) != 0)
})
create_corr_plot(independent_var_names = c("china", "trade", "money",
                                           "deal", "tariffs",
                                           "economy", "currency", "china_jobs"),
                 dependent_var_names = "july_2020_Close",
                 df = price_tweet_dtm_df)

## ----fig.width = 7, fig.height = 4, warning = F, message=F--------------------
plot_price_vs_weekly_series(df1_progress = soybeanCropProgress2019,
                            df2_contracts = contractsForJuly2020) +
  labs(title = "Crop_progress_19 vs March Contract price")

## ----fig.width = 7, fig.height = 4--------------------------------------------
df_top_export <- readRDS("preprocessed_data/top_10_export_countries.Rds")
df_top_export <- merge(contractsForJuly2020, df_top_export, by = "Date")
ind_col <- c("Weekly_Exports", "CMY_Outstanding_Sales", "CMY_Gross_New_Sales",
             "CMY_Net_Sales" ,"CMY_Total_Commitment")
corr_mat_weekly <- create_corr_plot(
  independent_var_names = ind_col,
  dependent_var_names = colnames(df_top_export)[
    grep("Close",colnames(df_top_export))],
  df = df_top_export,
  selected_countries = unique(df_top_export$Country),
  country_var = "Country",
  remove_countries = c("GRAND TOTAL", "KNOWN"))

## ----fig.width=7, fig.height=4------------------------------------------------
soybeanWASDE_clean <- readRDS("preprocessed_data/soybeanWASDE_clean.Rds")
plot_monthly_data(soybeanWASDE_clean)

