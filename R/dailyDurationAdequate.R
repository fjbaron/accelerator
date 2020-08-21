#' Title
#'
#' @param whenList 
#' @param whatList 
#' @param durMin 
#' @param durMax 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
dailyDurationAdequate=function(whenList,whatList,durMin=NA,durMax=NA,...){
  if(!is.na(durMin)) durMin = as.numeric(duration(durMin,units="secs"))
  if(!is.na(durMax)) durMax = as.numeric(duration(durMax,units="secs"))
  
  dailyDuration(whenList,whatList) %>% 
    mutate(value= (is.na(durMin) | (durMin<= duration)) & (is.na(durMax) | (durMax>= duration))) %>%
    select(day,what,value) 
}
