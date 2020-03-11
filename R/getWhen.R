#' Title
#'
#' @param df 
#' @param what 
#'
#' @return
#' @export
#'
#' @examples
getWhen <- function(df,what="what") {
  cat("\nComputing 'WHEN' intervals\n")
  
  pb <- progress_estimated(nrow(df))
  getWhenWithProgress <- function(...){
    pb$tick()$print()
    currentProject$defineWhen(...)
  }
  
  
  pmap( list(intervals=pluck(df,"what"), idBIN=pluck(df,"RAW")) ,.f = getWhenWithProgress)
}