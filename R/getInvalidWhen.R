#' Title
#'
#' @param summaryList 
#'
#' @return
#' @export
#'
#' @examples
getInvalidWhen=function(summaryList){
  map(summaryList,currentProject$whenValidity$directInvalidation)
}