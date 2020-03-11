#' Title
#'
#' @param when 
#' @param invalidWhen 
#'
#' @return
#' @export
#'
#' @examples
getValidWhen=function(when,invalidWhen){
  cat("\nComputing valid 'WHEN' intervals\n")
  pb <- progress_estimated(length(when))
  
  cleanInvalidWhenWithProgress <- function(...){
    pb$tick()$print()
    cleanInvalidWhen(...)
  }
  
  map2(when,invalidWhen,cleanInvalidWhenWithProgress)
}
