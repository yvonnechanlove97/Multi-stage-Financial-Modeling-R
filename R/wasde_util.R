#' @title Read in WASDE soybean data
#' @description Extract only soybean data from WASDE reports and combine the data
#'
#' @param path Folder containing only all the Excel WASDE reports with proper file names
#' @param sheet_name Name of the sheet (string) with the required crop data
#' @param col_range Range of columns to read from the input file and sheet
#' @param remove_slice Indices of columns to remove from the column range. Use NULL to keep all columns
#' @return Data frame of the combined data for required commodity (example: soybeans, corn)
#' @examples
#' data=read_wasde(path="C:/Users/40463/OneDrive/Documents/2019 Fall/Minne/WASDE")
#' @export
read_wasde <- function(path, sheet_name = "Page 15", col_range = "A13:A58",
                       remove_slice = c(3, 5, 17:21, 33:37)) {
  wd <- getwd()
  setwd(path)
  file_list <- list.files(path)
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
