#' @title Visualize the time-series price plot
#' @description Plot the price-date plot
#'
#' @param date a data frame or a column of dataframe containing the date with date format
#' @param price_data the dataframe containing the price data
#' @param main title of the plot
#' @param xlab x label of the plot
#' @param ylab y lable of the plot
#' @return a time-series plot
#' @examples
#' plot=time_plot(wasde[,1],wasde[,2:7])
#' @export
time_plot=function(date,price_data,main=NULL,xlab = 'Date',ylab='Price'){
  color=c(1:length(price_data))
  matplot(date,price_data, type = c("b"),pch=1,col = color,main=main,xlab =xlab,ylab=ylab,xaxt = "n") #plot
  axis(1, date, format(date, "%m-%d-%y") ,cex.axis = .56,las=2)
  legend("bottomleft",legend = colnames(price_data), col=color,pch =1,cex = 0.6,bty="n",inset = c(0.01, 0.01),pt.cex = 0.6) #legend
}
