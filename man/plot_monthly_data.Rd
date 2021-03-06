% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monthly_exploration.R
\name{plot_monthly_data}
\alias{plot_monthly_data}
\title{Visualize the time-series price plot}
\usage{
plot_monthly_data(
  df,
  date_col_index = 1,
  other_cols_index = 2:7,
  main = NULL,
  xlab = "Date",
  ylab = "Value"
)
}
\arguments{
\item{df}{Data frame with date column and required columns}

\item{date_col_index}{Index of column in `df` that corresponds to date column}

\item{other_cols_index}{Index of other columns in `df` used for plotting against date column}

\item{main}{Title of the plot}

\item{xlab}{x label of the plot}

\item{ylab}{y label of the plot}
}
\value{
A time-series plot
}
\description{
Plot the monthly data points against time
}
\examples{
data("soybeanWASDE")
soybeanWASDE_clean <- clean_wasde(combined_data = soybeanWASDE)
plot_monthly_data(soybeanWASDE_clean)
}
