#' @title Combine monthly data and daily data and return monthly data
#' @description Calculate the average price of from the day after the monthly observation day to day before next monthly observation day
#'
#' @param daily_data the dataframe containing daily data with Date as the first column
#' @param monthly_data the dataframe containing monthly data with Date as the first column
#' @return Data frame of the combined final monthly data
#' @examples
#' finaldata=monthly_trans(daily_data,monthly_data)
#' @export
monthly_trans <- function(daily_data,monthly_data, common_col = 'Date'){
  m=dplyr::left_join(daily_data,monthly_data,by=common_col)
  #create index and group variables

  m$index=ifelse(apply(!is.na(m[,-c(1:length(daily_data))]),1, any),1,0)
  m$group=NA
  m$group[1]=0
  for (i in 2:dim(m)[1]) {
    if(m$index[i] !=1) {
      m$group[i]=m$group[i-1]+1
    }
    else {m$group[i]=m$group[i-1]}
  }

  #summarise
  #monthly average data
  avg_m=m%>%group_by(group)%>%summarise_at(vars(colnames(daily_data)),mean)
  final=cbind(avg_m[-1,-1],merge(daily_data,monthly_data,by='Date'))
  final=wasde[,-c(length(avg_m):(length(avg_m)+length(daily_data))-1)]
  fianl$Date=as_date(final$Date,tz = NULL)
  final=final%>%arrange(Date)%>% select(Date,everything())
  return(final)
}
