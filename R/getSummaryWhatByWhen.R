#' Title
#'
#' @param what 
#' @param when 
#'
#' @return
#' @export
#'
#' @examples
getSummaryWhatByWhen=function(what,when){
  cat("\nComputing summaries of 'WHAT' inside `WHEN` \n")
  
  pb <- progress_estimated(length(what))
  summaryWhatByWhenWithProgress=function(...){
    pb$tick()$print()
    summaryWhatByWhen(...)
  }
    
  
  map2(what,when,summaryWhatByWhenWithProgress)
  }