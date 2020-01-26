## ----fig.width=7, fig.height=4, warning=F, message=F--------------------------
library(FinancialModelingR)
library(png)
library(grid)
img <- readPNG("private_data/july2020.PNG")
grid.raster(img)

## -----------------------------------------------------------------------------
contractsForJuly2020 <- FinancialModelingR::read_price(
  in_file = "private_data/ActiveSoybeanContractsforJuly2020.xlsx", delta_price = T,
  add_delta = T, subset = T, subset_min_date = "2017-01-01",
  rename_price_columns = T, rename_prefix = "july_2020_", skip_lines = 3)
contractsForJuly2020$Date <- as.Date(contractsForJuly2020$Date, "%Y-%m-%d")
saveRDS(contractsForJuly2020, "preprocessed_data/contractsForJuly2020.Rds")

## ----fig.width=7, fig.height=4, warning=F, message=F--------------------------
img <- readPNG("private_data/realdonaldtrump.PNG")
grid.raster(img)

## -----------------------------------------------------------------------------
tweet_df <- readRDS("private_data/text_features.Rds")
colnames(tweet_df)[1] <- "Date"
sds <- sapply(tweet_df[, 2:ncol(tweet_df)], sd)
remove_cols <- 1 + which(sds == 0)
if(length(remove_cols) > 0) {
  tweet_df <- tweet_df[, -remove_cols]
}
saveRDS(tweet_df, "preprocessed_data/tweet_df.Rds")

## ----fig.width=7, fig.height=4------------------------------------------------
library(png)
library(grid)
img <- readPNG("private_data/exports.PNG")
grid.raster(img)

## -----------------------------------------------------------------------------
library(janitor)
soybeanExports = read_exports(
  file = "raw_data/ExportSalesDataByCommodity(Soybeans).csv", skip_lines = 4)

## -----------------------------------------------------------------------------
library(dplyr)
data("soybeanExports", package = "FinancialModelingR")
competitors <- c("ARGENTINA", "BRAZIL")
df_total_export <- soybeanExports %>% group_by(Country) %>%
  summarize(Total_Export = sum(Weekly_Exports, na.rm = T))
top_countries <- head(x = df_total_export$Country[
  order(df_total_export$Total_Export, decreasing = TRUE)], n = 10)
selected_countries <- c(competitors, top_countries)
df_top_export <- soybeanExports[sapply(
  soybeanExports$Country, function(country) country %in% selected_countries), ]
saveRDS(df_top_export, "preprocessed_data/top_10_export_countries.Rds")

## -----------------------------------------------------------------------------
data("soybeanCropProgressUSA2019", package = "FinancialModelingR")
soybeanCropProgress2019$WEEK.ENDING <-
  as.Date(soybeanCropProgress2019$WEEK.ENDING, "%Y-%m-%d")
saveRDS(soybeanCropProgress2019, "preprocessed_data/soybeanCropProgress2019.Rds")

## -----------------------------------------------------------------------------
data("soybeanCombinedWASDE")
soybeanWASDE_clean <- clean_wasde(combined_data = soybeanCombinedWASDE)
saveRDS(soybeanWASDE_clean, "preprocessed_data/soybeanWASDE_clean.Rds")

