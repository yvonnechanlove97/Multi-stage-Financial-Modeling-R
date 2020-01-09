#' @title Read in WASDE soybean data
#' @description Extract only soybean data from wasde reports and combine the data
#'
#' @param path the file containing only all the wasde reports with proper file names
#' @return Data frame of the combined soybean data
#' @examples
#' data=read_wasde(path="C:/Users/40463/OneDrive/Documents/2019 Fall/Minne/WASDE")
#' @export
read_wasde=function(path){
  file_list <- list.files(path)
  data=data.frame()
  com_data=readxl::read_xls(file_list[1], "Page 15", range='A13:A58',col_names = 'SOYBEANS')%>%slice(-c(3, 5, 17:21, 33:37))
  com_data$SOYBEANS[15:25]=paste0('Oil-',com_data$SOYBEANS[15:25])
  com_data$SOYBEANS[26:34]=paste0('Meal-',com_data$SOYBEANS[26:34])
  for (i in 1:length(file_list)){
    newdata=readxl::read_xls(file_list[i], "Page 15", range ="E13:E58",col_names =substr(file_list[i],start=1,stop=8))%>%slice(-c(3, 5, 17:21, 33:37))
    com_data=cbind(com_data,newdata)
  }
  return(com_data)
}
