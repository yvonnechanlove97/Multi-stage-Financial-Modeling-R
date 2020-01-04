#' @title Progress - Price Plot
#' @description Plots the weekly progress (in percentage) vs price series. Prices are considered 2 days before and 2 days after the end date of each week
#'
#' @param df1_progress Data frame containing progress
#' @param df2_contracts Data frame containing close prices
#' @return ggplot object with plots comparing price against progress over current year, previous year and 5 year average
#' @examples
#' contractsForJuly2020 <- read_price(
#' in_file = "private_data/ActiveSoybeanContractsforJuly2020.xlsx", delta_price = T,
#' add_delta = T, subset = T, subset_min_date = "2017-01-01",
#' rename_price_columns = T, rename_prefix = "july_2020_", skip_lines = 3)
#' plt_pretty(df1_progress = soybeanCropProgress2017, df2_contracts = contractsForJuly2020)
#' @export
plt_pretty <- function(df1_progress, df2_contracts) {
  # Close prices of last business day and the following business day
  date_1 <- df1_progress$WEEK.ENDING - 2
  date_2 <- df1_progress$WEEK.ENDING + 2
  close_col <- grep(colnames(df2_contracts), pattern = "_Close$")
  close_col <- setdiff(
    close_col, grep(colnames(df2_contracts), pattern = "_delta_Close$"))
  colnames(df2_contracts)[close_col] <- 'Close'
  price_1 <- df2_contracts[
    sapply(df2_contracts$Date, function(date) date %in% date_1), 'Close']
  price_2 <- df2_contracts[
    sapply(df2_contracts$Date, function(date) date %in% date_2), 'Close']
  tmp1 <- data.frame(cbind(Date = date_1, Close = price_1))
  tmp2 <- data.frame(cbind(Date = df2_contracts[df2_contracts$Date %in% date_2, "Date"],
                           Close = price_2)) # pair the date in records
  gap1 <- max(tmp1$Close) - min(tmp1$Close)
  gap2 <- max(tmp2$Close) - min(tmp2$Close)
  tmp1$Date <- as.Date(tmp1$Date, origin = "1970-01-01")
  tmp2$Date <- as.Date(tmp2$Date, origin = "1970-01-01")

  ggplot(data = df1_progress) +
    geom_point(data = tmp1,aes(x = Date, y = (Close - min(tmp1$Close)) * 100/gap1, alpha = 'Before')) +
    geom_line(data = tmp1,aes(x = Date, y = (Close - min(tmp1$Close)) * 100/gap1, alpha = 'Before')) +
    geom_point(data = tmp2,aes(x = Date, y = (Close - min(tmp2$Close)) * 100/gap2, alpha = 'After')) +
    geom_line(data = tmp2,aes(x = Date, y = (Close - min(tmp2$Close)) * 100/gap2, alpha = 'After')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.PLANTED, colour = "Planted", linetype = 'Current Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.PLANTED, colour = "Planted", linetype = 'Current Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.PLANTED, colour = "Planted", linetype = '5 Years Average')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.PLANTED, colour = "Planted", linetype = 'Previous Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.EMERGED, colour = "Emerged", linetype = 'Current Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.EMERGED, colour = "Emerged", linetype = '5 Years Average')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.EMERGED, colour = "Emerged", linetype = 'Previous Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.BLOOMING, colour = "Blooming", linetype = 'Current Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.BLOOMING, colour = "Blooming", linetype = '5 Years Average')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.BLOOMING, colour = "Blooming", linetype = 'Previous Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.SETTING.PODS, colour = "Setting Pods", linetype = 'Current Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.SETTING.PODS, colour = "Setting Pods", linetype = '5 Years Average')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.SETTING.PODS, colour = "Setting Pods", linetype = 'Previous Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.DROPPING.LEAVES, colour = "Dropping leaves", linetype = 'Current Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.DROPPING.LEAVES, colour = "Dropping leaves", linetype = '5 Years Average')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.DROPPING.LEAVES, colour = "Dropping leaves", linetype = 'Previous Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS.in.PCT.HARVESTED, colour = "Harvested", linetype = 'Current Year')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..5.YEAR.AVG.in.PCT.HARVESTED, colour = "Harvested", linetype = '5 Years Average')) +
    geom_line(aes(x = WEEK.ENDING,y = PROGRESS..PREVIOUS.YEAR.in.PCT.HARVESTED, colour = "Harvested", linetype = 'Previous Year')) +
    scale_x_date(name = "Time", date_breaks = "2 weeks", date_labels = "%m-%d") +
    scale_y_continuous("Crop Progress in percentage", sec.axis = sec_axis(~ (. * gap1 /100) + min(tmp1$Close) ,breaks = function(x) pretty(x, n=10),  name = "Price")) +
    scale_linetype_manual("Variabler",values=c("Current Year"=1, "5 Years Average" = 2, "Previous Year" = 3)) +
    scale_color_manual("Stage",values = c("brown","darkgreen","green","gold","red","purple")) +
    scale_alpha_manual("Price before/after the report", values = c(1, 1/5)) +
    theme_bw() + coord_fixed()
}
