#' @title Crop progress row-diff
#' @description Computes row-wise diff for crop progress data
#'
#' @param df Data frame containing raw crop progress data
#' @return Data fraome of diff
#' @examples
#' diff_df_2017 <- crop_progress_group_diff(soybeanCropProgress2017)
#' @export
crop_progress_group_diff <- function(df) {
  tmp <- apply(df[-c(1:8)], MARGIN = 2, FUN = function(x) diff(x))
  return(tmp)
}
