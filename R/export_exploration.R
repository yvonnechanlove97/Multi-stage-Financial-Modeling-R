#' @title Correlation Plot
#' @description Creates correlation plot between dependent variables and independent variables for selected countries
#'
#' @param independent_var_names Character vector containing names of independent variables
#' @param dependent_var_names Character vector containing names of dependent variables
#' @param selected_countries Character vector containing names of selected countries
#' @param df Data frame containing price merged with exports
#' @param country_var Name of the column with Country
#' @param top_corr Specifies 'k' in the top-k correlations for each country
#' @param remove_countries Country names to be removed
#' @return The sum of \code{x} and \code{y}.
#' @examples
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
#' corr_mat_weekly = create_export_corr_plot(independent_var_names = ind_col,
#'                                    dependent_var_names = colnames(df_top_export)[
#'                                    grep("Close",colnames(df_top_export))],
#'                                    selected_countries = selected_countries,
#'                                    df = df_top_export, top_corr = 4)
#' @export
create_export_corr_plot <- function(independent_var_names, dependent_var_names,
                                    selected_countries, df, country_var = "Country",
                                    top_corr = 4,
                                    remove_countries = c("GRAND TOTAL", "KNOWN")) {
  list_top_corr <- list()
  list_corr_mat <- list()
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

    list_top_corr[[paste0("top_corr_",j)]] = head(df_ab_corr, top_corr)
    list_corr_mat[[paste0("corr_mat_",j)]] = corr_mat
    corrplot(list_corr_mat[[paste0("corr_mat_",j)]],
             tl.cex = .6,
             method = "color",
             addCoef.col = "black",
             title = j,
             mar = c(0,0,1,0))
  }
  return_list = append(list_top_corr, list_corr_mat)
  return (return_list)
}
