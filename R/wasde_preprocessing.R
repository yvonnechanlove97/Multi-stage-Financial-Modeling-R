#' @title Clean the read-in WASDE soybean data
#' @description Remove hifens and asterisk as well as transpose the data into time-series data
#'
#' @param com_data Data frame of the combined soybean data
#' @return Data frame of the clean combined soybean data
#' @examples
#' data=read_wasde(path="C:/Users/40463/OneDrive/Documents/2019 Fall/Minne/WASDE")
#' @export
clean_wasde=function(com_data){
  #remove hifen
  test=grepl("-",as.matrix(com_data[,-1]))
  test=matrix(test,dim(com_data))
  temp  <- strsplit(as.character(com_data[,-1][test]), "-")
  non_hyphen=sapply(temp, function(x) mean(as.numeric(x)))
  com_data[,-1][test]=non_hyphen

  #remove asterisk
  com_data[,-1]=apply(com_data[,-1],2,function(x){gsub("\\*",'',x)})

  #as.numeric
  com_data[,-1]=apply(com_data[,-1],2,as.numeric)

  #transpose the data
  trans_data=as.data.frame(t(com_data[,-1]))
  colnames(trans_data)=com_data[,1]
  trans_data=cbind(rownames(trans_data),trans_data)
  colnames(trans_data)[1]='Date'
  trans_data$Date=as.Date(trans_data$Date, format = "%m-%d-%y")
  return(trans_data)
}
