#' @title Weekly Series vs Price Plot
#' @description Plots the multiple weekly data series vs daily price series. Prices are considered n days before and n days after the end date of each week. Line color corresponds to the type of weekly series and line type corresponds to the type of variable.
#'
#' @param df1_progress Data frame containing weekly series
#' @param df2_contracts Data frame containing close prices. Leave as NULL if the weekly data has already been merged with daily prices
#' @param week_ending_col Column name in `df1_progress` that contains the end of week for each data point
#' @param n Number of days to lead and lag around week ending. Prices are compared between `week_end + n` and `week_end - n`
#' @param series_type Label for type of weekly series - appears as y-axis label in plot
#' @param line_columns Columns in `df1_progress` containing required weekly series to plot, corresponds to the exhaustive list of different types of series and variables among each series
#' @param line_colors Color sequence (strings) corresponding to the line_columns
#' @param color_mapping Mapping from string sequence of line colors (series type) to string sequence of predefined colors in R
#' @param line_type Line type sequence (strings) corresponding to each line added using line_columns
#' @param color_variable_name Proxy for each type of weekly series used in plotting - appears in legend
#' @param variable_name Proxy for each type of variable user in plotting - appears in legend
#' @param variable_mapping Line type mapping for different types of variables
#' @return ggplot object with plots comparing price against progress over current year, previous year and 5 year average
#' @examples
#' contractsForJuly2020 <- read_price(
#' in_file = "private_data/ActiveSoybeanContractsforJuly2020.xlsx", delta_price = T,
#' add_delta = T, subset = T, subset_min_date = "2017-01-01",
#' rename_price_columns = T, rename_prefix = "july_2020_", skip_lines = 3)
#' plt_pretty(df1_progress = soybeanCropProgress2017, df2_contracts = contractsForJuly2020)
#' @export
plot_price_vs_weekly_series <- function(
  df1_progress, df2_contracts = NULL, week_ending_col = "WEEK.ENDING", n = 2,
  series_type = "Crop Progress in percentage", line_columns = c(
    "PROGRESS.in.PCT.PLANTED", "PROGRESS..5.YEAR.AVG.in.PCT.PLANTED",
    "PROGRESS..PREVIOUS.YEAR.in.PCT.PLANTED", "PROGRESS.in.PCT.EMERGED",
    "PROGRESS..5.YEAR.AVG.in.PCT.EMERGED",
    "PROGRESS..PREVIOUS.YEAR.in.PCT.EMERGED",
    "PROGRESS.in.PCT.BLOOMING", "PROGRESS..5.YEAR.AVG.in.PCT.BLOOMING",
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
  line_colors = c("Planted", "Planted", "Planted", "Emerged",
                  "Emerged", "Emerged", "Blooming",
                  "Blooming", "Blooming", "Setting Pods",
                  "Setting Pods", "Setting Pods",
                  "Dropping Leaves", "Dropping Leaves",
                  "Dropping Leaves", "Harvested", "Harvested",
                  "Harvested"),
  color_mapping = c("Planted" = "brown", "Emerged" = "darkgreen",
                    "Blooming" = "green", "Setting Pods" = "gold",
                    "Dropping Leaves" = "red", "Harvested" = "purple"),
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
  variable_name = "Variable",
  variable_mapping = c("Current Year" = 1, "5 Years Average" = 2,
                       "Previous Year" = 3)) {
  if(is.null(df2_contracts)) {
    df2_contracts <- df1_progress
  }
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
    scale_y_continuous(series_type,
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


#' @title Correlation Plot
#' @description Creates correlation plot between dependent variables and independent variables for selected countries
#'
#' @param independent_var_names Character vector containing names of independent variables
#' @param dependent_var_names Character vector containing names of dependent variables
#' @param df Data frame containing price merged with exports
#' @return Correlation matrix between set of independent variables and set of dependent variables
#' @examples
#' ### Example 1: Weekly exports ###
#' data("soybeanExports")
#' competitors <- c("ARGENTINA", "BRAZIL")
#' df_total_export <- soybeanExports %>% group_by(Country) %>%
#'   summarize(Total_Export = sum(Weekly_Exports, na.rm = T))
#' top_countries <- head(x = df_total_export$Country[
#'   order(df_total_export$Total_Export, decreasing = TRUE)], n = 10)
#' selected_countries <- c(competitors, top_countries)
#' df_top_export <- soybeanExports[sapply(
#'   soybeanExports$Country, function(country) country %in% selected_countries), ]
#' df_top_export = inner_join(contractsForJuly2020, df_top_export, by = "Date")
#' ind_col <- c("Weekly_Exports", "CMY_Outstanding_Sales", "CMY_Gross_New_Sales",
#'              "CMY_Net_Sales" ,"CMY_Total_Commitment")
#' corr_mat_weekly = corr_mat_weekly <- create_corr_plot(
#' independent_var_names = ind_col,
#' dependent_var_names = colnames(df_top_export)[
#'   grep("Close",colnames(df_top_export))],
#' df = df_top_export,
#' selected_countries = selected_countries,
#' country_var = "Country",
#' remove_countries = c("GRAND TOTAL", "KNOWN"))
#'
#' ### Example 2 ###
#' tweet_dtm_df <- readRDS("private_data/text_features.Rds")
#' print(head(tweet_dtm_df))
#' price_tweet_dtm_df <- merge(tweet_dtm_df,
#'                             contractsForJuly2020[, c("Date", "july_2020_Close")],
#'                             by.x = "created_at", by.y = "Date")
#' create_corr_plot(independent_var_names = NULL,
#'                  dependent_var_names = "july_2020_Close",
#'                  df = price_tweet_dtm_df,
#'                  remove_cols = c("amp", "get", "want", "many", "will", "just",
#'                                  "want", "can", "realdonaldtrump", "created_at",
#'                                  "made", "going", "must", "make"))
#'
#' ### Example 3 ###
#' price_tweet_dtm_df$china_jobs <- apply(price_tweet_dtm_df[, -1], 1, function(row) {
#'   as.integer((row["china"] * row["jobs"]) != 0)
#' })
#' create_corr_plot(independent_var_names = c("china", "trade", "money",
#'                                            "deal", "tariffs",
#'                                            "economy", "currency", "china_jobs"),
#'                  dependent_var_names = "july_2020_Close",
#'                  df = price_tweet_dtm_df)
#' @export
create_corr_plot <- function(independent_var_names, dependent_var_names, df, ...) {
  params <- list(...)
  if("selected_countries" %in% names(params)) {
    selected_countries <- params[["selected_countries"]]
    remove_countries <- params[["remove_countries"]]
    country_var <- params[["country_var"]]
    corr_plot_list <- list()
    # list_top_corr <- list()
    # list_corr_mat <- list()
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

    independent_var = independent_var_names
    dependent_var = dependent_var_names
    if(!is.null(remove_countries)) {
      selected_countries <- setdiff(selected_countries, remove_countries)
    }
    for (j in selected_countries) {
      corr_mat = cor(x = df[df[, country_var] == j, independent_var],
                     y = df[df[, country_var] == j, dependent_var],
                     method = "pearson")
      df_corr = as.data.frame(as.table(corr_mat))
      df_ab_corr = df_corr[order(df_corr$Freq, decreasing = TRUE),]

      # list_top_corr[[paste0("top_corr_",j)]] = head(df_ab_corr, top_corr)
      # list_corr_mat[[paste0("corr_mat_",j)]] = corr_mat
      corr_plot_list <- append(corr_plot_list,
                               corrplot(corr_mat,
                                        tl.cex = .6,
                                        method = "color",
                                        addCoef.col = "black",
                                        title = j,
                                        mar = c(0,0,1,0)))
    }
    # return_list = append(list_top_corr, list_corr_mat)
    # return (return_list)
    return(corr_plot_list)
  } else if("remove_cols" %in% names(params)) {
    remove_cols <- params[["remove_cols"]]
  } else {
    remove_cols <- NULL
  }
  if(is.null(independent_var_names)) {
    independent_var_names <- setdiff(colnames(df), c(dependent_var_names))
  }
  independent_var_names <- setdiff(independent_var_names, remove_cols)
  dependent_var_names <- setdiff(dependent_var_names, remove_cols)
  remove_cols <- colnames(df)[sapply(df, sd) == 0]
  independent_var_names <- setdiff(independent_var_names, remove_cols)
  dependent_var_names <- setdiff(dependent_var_names, remove_cols)
  df <- df[, sapply(df, sd) != 0]
  corr_mat = cor(x = df[, independent_var_names],
                 y = df[, dependent_var_names],
                 method = "pearson")

  corrplot(corr_mat,
           tl.cex = .6,
           method = "color", hclust.method = "ward")
}
