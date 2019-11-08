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
  pmap( list(intervals=pluck(df,"what"), idBIN=pluck(df,"RAW")) ,.f = currentProject$defineWhen)
}