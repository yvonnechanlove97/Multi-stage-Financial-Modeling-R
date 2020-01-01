#' @title Correlation plot
#' @description Generate correlation heatmap of given independent variable(s) and dependent variable(s) in a given data frame
#'
#' @param independent_var_names Character vector specifying the independent variable names
#' @param dependent_var_names Character vector specifying the dependent variable names
#' @param remove_words Words to be removed that were not removed as stopwords
#' @param text_correlates Text (independent) variables to consider for computing and plotting correlation
#' @param price_correlates Price (dependent) variables to consider for computing and plotting correlation
#' @return Plot object containing the heatmap plot of correlation between independent variable(s) and dependent variable(s)
#' @examples
#' independent_var_names <- setdiff(
#' colnames(df), c(colnames(df)[grep(colnames(df), pattern = "_Close$")],
#'                  colnames(df)[grep(colnames(df), pattern = "_Open$")],
#'                  colnames(df)[grep(colnames(df), pattern = "_High$")],
#'                  colnames(df)[grep(colnames(df), pattern = "_Low$")],
#'                  colnames(df)[grep(colnames(df), pattern = "_sign$")],
#'                  "Date"))
#' dependent_var_names <- c(colnames(df)[grep(colnames(df), pattern = "_Close$")],
#'                          colnames(df)[grep(colnames(df), pattern = "_Open$")],
#'                          colnames(df)[grep(colnames(df), pattern = "_High$")],
#'                          colnames(df)[grep(colnames(df), pattern = "_Low$")],
#'                          colnames(df)[grep(colnames(df), pattern = "_sign$")])
#' create_corr_plot(independent_var_names, dependent_var_names, df)
#' @export
create_corr_plot = function(independent_var_names, dependent_var_names, df,
                            remove_words = c(
                              "amp", "get", "want", "many", "will", "just",
                              "want", "can", "realdonaldtrump",
                              "made", "going", "must", "make"),
                            text_correlates = c(
                              "china", "trade", "money", "deal", "tariffs",
                              "economy", "currency", "china_jobs"),
                            price_correlates = c(
                              "july_delta_Close", "may_delta_Close",
                              "march_delta_Close", "july_delta_Close_sign",
                              "may_delta_Close_sign", "march_delta_Close_sign")) {
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

  independent_var <- independent_var_names
  independent_var <- setdiff(independent_var, remove_words)
  dependent_var = dependent_var_names
  corr_mat = cor(x = df[, independent_var],
                 y = df[, dependent_var],
                 method = "pearson")
  corr_mat <- corr_mat[text_correlates, price_correlates]

  return (corrplot(corr_mat,
                   tl.cex = .6,
                   method = "color", hclust.method = "ward"))

}
