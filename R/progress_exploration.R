#' @title Progress - Price Plot
#' @description Plots the multiple weekly data series vs daily price series. Prices are considered n days before and n days after the end date of each week
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
plot_price_vs_weekly_series <- function(df1_progress, df2_contracts,
                                        week_ending_col = "WEEK.ENDING", n = 2,
                                        line_columns = c(
                                          "PROGRESS.in.PCT.PLANTED",
                                          "PROGRESS..5.YEAR.AVG.in.PCT.PLANTED",
                                          "PROGRESS..PREVIOUS.YEAR.in.PCT.PLANTED",
                                          "PROGRESS.in.PCT.EMERGED",
                                          "PROGRESS..5.YEAR.AVG.in.PCT.EMERGED",
                                          "PROGRESS..PREVIOUS.YEAR.in.PCT.EMERGED",
                                          "PROGRESS.in.PCT.BLOOMING",
                                          "PROGRESS..5.YEAR.AVG.in.PCT.BLOOMING",
                                          "PROGRESS..PREVIOUS.YEAR.in.PCT.BLOOMING",
                                          "PROGRESS.in.PCT.SETTING.PODS",
                                          "PROGRESS..5.YEAR.AVG.in.PCT.SETTING.PODS",
                                          "PROGRESS..PREVIOUS.YEAR.in.PCT.SETTING.PODS",
                                          "PROGRESS.in.PCT.DROPPING.LEAVES",
                                          "PROGRESS..5.YEAR.AVG.in.PCT.DROPPING.LEAVES",
                                          "PROGRESS..PREVIOUS.YEAR.in.PCT.DROPPING.LEAVES",
                                          "PROGRESS.in.PCT.HARVESTED",
                                          "PROGRESS..5.YEAR.AVG.in.PCT.HARVESTED",
                                          "PROGRESS..PREVIOUS.YEAR.in.PCT.HARVESTED"),
                                        line_colors = c(
                                          "Planted", "Planted", "Planted", "Emerged",
                                          "Emerged", "Emerged", "Blooming",
                                          "Blooming", "Blooming", "Setting Pods",
                                          "Setting Pods", "Setting Pods",
                                          "Dropping Leaves", "Dropping Leaves",
                                          "Dropping Leaves", "Harvested", "Harvested",
                                          "Harvested"),
                                        line_type = c("Current Year", "5 Years Average",
                                                      "Previous Year", "Current Year",
                                                      "5 Years Average", "Previous Year",
                                                      "Current Year", "5 Years Average",
                                                      "Previous Year", "Current Year",
                                                      "5 Years Average", "Previous Year",
                                                      "Current Year", "5 Years Average",
                                                      "Previous Year", "Current Year",
                                                      "5 Years Average", "Previous Year"),
                                        color_variable_name = "Stage",
                                        color_mapping = c(
                                          "Planted" = "brown", "Emerged" = "darkgreen",
                                          "Blooming" = "green", "Setting Pods" = "gold",
                                          "Dropping Leaves" = "red", "Harvested" = "purple"),
                                        variable_name = "Variable",
                                        variable_mapping = c("Current Year" = 1,
                                                             "5 Years Average" = 2,
                                                             "Previous Year" = 3)) {
  # Close prices of last business day and the following business day
  date_1 <- df1_progress[, week_ending_col] - n
  date_2 <- df1_progress[, week_ending_col] + n
  uniq_line_colors <- unique(line_colors)
  uniq_colors <- sapply(uniq_line_colors, function(colr) color_mapping[colr])
  names(uniq_colors) <- NULL
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

  plt <- ggplot(data = df1_progress) +
    geom_point(data = tmp1,aes(
      x = Date, y = (Close - min(tmp1$Close)) * 100/gap1, alpha = 'Before')) +
    geom_line(data = tmp1,aes(
      x = Date, y = (Close - min(tmp1$Close)) * 100/gap1, alpha = 'Before')) +
    geom_point(data = tmp2,aes(
      x = Date, y = (Close - min(tmp2$Close)) * 100/gap2, alpha = 'After')) +
    geom_line(data = tmp2,aes(
      x = Date, y = (Close - min(tmp2$Close)) * 100/gap2, alpha = 'After')) +
    scale_x_date(name = "Time", date_breaks = "2 weeks", date_labels = "%m-%d") +
    scale_y_continuous("Crop Progress in percentage",
                       sec.axis = sec_axis(~ (. * gap1 /100) + min(tmp1$Close),
                                           breaks = function(x) pretty(x, n=10),
                                           name = "Price")) +
    scale_alpha_manual("Price before/after the report", values = c(1, 1/5)) +
    theme_bw() + coord_fixed()
  for(i in 1:length(line_columns)) {
    plt <- plt + geom_line(data = df1_progress,
                           aes_string(x = week_ending_col,
                                      y = line_columns[i],
                                      colour = shQuote(line_colors[i]),
                                      linetype = shQuote(line_type[i])
                           ))
  }
  plt <- plt +
    scale_color_manual(color_variable_name, values = uniq_colors) +
    scale_linetype_manual(variable_name, values = variable_mapping)
  return(plt)
}
