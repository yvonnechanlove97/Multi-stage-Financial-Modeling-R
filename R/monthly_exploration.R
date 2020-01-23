#' @title Visualize the time-series price plot
#' @description Plot the monthly data points against time
#'
#' @param df Data frame with date column and required columns
#' @param date_col_index Index of column in `df` that corresponds to date column
#' @param other_cols_index Index of other columns in `df` used for plotting against date column
#' @param main Title of the plot
#' @param xlab x label of the plot
#' @param ylab y label of the plot
#' @return A time-series plot
#' @examples
#' data("soybeanWASDE")
#' soybeanWASDE_clean <- clean_wasde(combined_data = soybeanWASDE)
#' plot_monthly_data(soybeanWASDE_clean)
#' @export
plot_monthly_data <- function(df, date_col_index = 1,
                              other_cols_index = 2:7,
                              main = NULL, xlab = 'Date',
                              ylab = 'Value') {
  df[, date_col_index] <-
    as.Date(df[, date_col_index])
  df <-
    df[order(df[, date_col_index],
                              decreasing = F), ]
  color <- c(1:length(df))
  if(!is.null(other_cols_index)) {
    df <- df[, c(date_col_index,
                                                   other_cols_index)]
  }
  matplot(x = df[, 1],
          y = df[, 2:ncol(df)],
          type = c("b"), pch = 1,
          col = color, main = main,
          xlab = xlab, ylab = ylab, xaxt = "n") #plot
  axis(1, df[, 1], format(df[, 1], "%m-%d-%y"),
       cex.axis = .56, las = 2)
  legend("bottomleft", legend = colnames(df)[other_cols_index],
         col = color, pch = 1, cex = 0.6, bty = "n", inset = c(0.01, 0.01),
         pt.cex = 0.6) #legend
}
