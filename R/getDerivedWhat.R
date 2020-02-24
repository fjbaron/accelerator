#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
getDerivedWhat=function(df) {
  if(!is.null(currentProject$derivedWhat) & length(currentProject$derivedWhat)>0)
     pmap( df %>% select(what,when) ,.f = currentProject$derivedWhat)
  else NULL
}