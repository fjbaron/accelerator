#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
ggir2What=function(df){
  pmap(df,ggir2Interval)
}