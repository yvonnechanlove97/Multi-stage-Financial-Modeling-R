#' @title Read in WASDE data
#' @description Extract only specific part of WASDE reports across multiple WASDE files in a folder
#'
#' @param path Folder containing only all the Excel WASDE reports with proper file names
#' @param sheet_name Name of the sheet (string) with the required crop data
#' @param col_range Range of columns to read from the input file and sheet
#' @param remove_slice Indices of columns to remove from the column range. Use NULL to keep all columns
#' @return Data frame of the combined data for required commodity (example: soybeans, corn)
#' @examples
#' data=read_wasde(path="raw_data/WASDE")
#' @export
read_wasde <- function(path, sheet_name = "Page 15", col_range = "A13:A58",
                       remove_slice = c(3, 5, 17:21, 33:37)) {
  wd <- getwd()
  setwd(path)
  file_list <- list.files()
  data=data.frame()
  com_data=readxl::read_xls(path = file_list[1], sheet = sheet_name, range=col_range,col_names = 'SOYBEANS')
  if(!is.null(remove_slice)) {
    com_data <- com_data %>% slice(-remove_slice)
  }
  # This is very specific to soybeans. Can we change this in some way?
  com_data$SOYBEANS[15:25]=paste0('Oil-',com_data$SOYBEANS[15:25])
  com_data$SOYBEANS[26:34]=paste0('Meal-',com_data$SOYBEANS[26:34])
  for (i in 1:length(file_list)){
    newdata=readxl::read_xls(path = file_list[i], sheet = sheet_name, range ="E13:E58",col_names =substr(file_list[i],start=1,stop=8))
    if(!is.null(remove_slice)) {
      newdata <- newdata %>% slice(-remove_slice)
    }
    com_data=cbind(com_data,newdata)
  }
  setwd(wd)
  return(com_data)
}


#' @title Clean the read-in WASDE soybean data
#' @description Remove hiphens and asterisk as well as transpose the data into time-series data
#'
#' @param combined_data Data frame of the combined soybean data
#' @return Data frame of the clean combined soybean data
#' @examples
#' data <- clean_wasde(combined_data = soybeanWASDE)
#' @export

# Example should be changed as it is for read_wasde
clean_wasde <- function(combined_data){
  #remove hyphen
  test=grepl("-",as.matrix(combined_data[,-1]))
  test=matrix(test,dim(combined_data))
  temp  <- strsplit(as.character(combined_data[,-1][test]), "-")
  non_hyphen=sapply(temp, function(x) mean(as.numeric(x)))
  combined_data[,-1][test]=non_hyphen

  #remove asterisk
  combined_data[,-1]=apply(combined_data[,-1],2,function(x){gsub("\\*",'',x)})

  #as.numeric
  combined_data[,-1]=apply(combined_data[,-1],2,as.numeric)

  #transpose the data
  trans_data=as.data.frame(t(combined_data[,-1]))
  colnames(trans_data)=combined_data[,1]
  trans_data=cbind(rownames(trans_data),trans_data)
  colnames(trans_data)[1]='Date'
  trans_data$Date=as.Date(trans_data$Date, format = "%m-%d-%y")
  return(trans_data)
}
