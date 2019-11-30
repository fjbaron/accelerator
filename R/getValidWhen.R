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
  map2(when,invalidWhen,cleanInvalidWhen)
}
