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
  map2(what,when,summaryWhatByWhen)
  }