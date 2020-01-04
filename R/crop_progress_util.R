#' @title Crop progress row-diff
#' @description Computes row-wise diff for crop progress data
#'
#' @param df Data frame containing raw crop progress data
#' @return Data frame of diff
#' @examples
#' diff_df_2017 <- crop_progress_group_diff(soybeanCropProgress2017)
#' @export
crop_progress_group_diff <- function(df) {
  reqd_cols <- grep(colnames(df), pattern = "^PROGRESS", ignore.case = T)
  renamed_cols <- paste0("weekly_", colnames(df)[reqd_cols])
  df[, renamed_cols] <- apply(df[, reqd_cols], MARGIN = 2, FUN = function(x) diff(x))
  return(df)
}
