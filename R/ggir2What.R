#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
ggir2What=function(df){
  cat("\nComputing 'WHAT' intervals using GGIR epochs\n")
  pb <- progress_estimated(nrow(df))
  
  ggir2IntervalWithProgress <- function(...){
    pb$tick()$print()
    ggir2Interval(...)
  }
  
  pmap(df,ggir2IntervalWithProgress)
}