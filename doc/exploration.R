## -----------------------------------------------------------------------------
library(FinancialModelingR)
library(readxl)
library(ggplot2)
data("soybeanCropProgressUSA2017")
soybeanCropProgress2017$WEEK.ENDING <-
  as.Date(soybeanCropProgress2017$WEEK.ENDING, "%Y-%m-%d")

## -----------------------------------------------------------------------------
contractsForJuly2020 <- read_price(
  in_file = "private_data/ActiveSoybeanContractsforJuly2020.xlsx", delta_price = T,
  add_delta = T, subset = T, subset_min_date = "2017-01-01",
  rename_price_columns = T, rename_prefix = "july_2020_", skip_lines = 3)
contractsForJuly2020$Date <- as.Date(contractsForJuly2020$Date, "%Y-%m-%d")
print(head(contractsForJuly2020))

## -----------------------------------------------------------------------------
plt_pretty(df1_progress = soybeanCropProgress2017,
           df2_contracts = contractsForJuly2020) +
  labs(title = "Crop_progress_17 vs March Contract price")

