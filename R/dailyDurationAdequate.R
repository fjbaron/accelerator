#' Title
#'
#' @param whenList 
#' @param whatList 
#' @param durMin 
#' @param durMax 
#' @param durRelMin 
#' @param durRelMax 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
dailyDurationAdequate=function(whenList,whatList,durMin=NA,durMax=NA,durRelMin=NA,durRelMax=NA,...){
  if(!is.na(durMin)) durMin = as.numeric(duration(durMin,units="secs"))
  if(!is.na(durMax)) durMax = as.numeric(duration(durMax,units="secs"))
  #Para usar duraciones relativas
  whenList=whenList %>% mutate(durRef=as.numeric(difftime(to,from,units = "secs"))) %>%
    filter(durRef>0) #Sanity check

  dailyDuration(whenList,whatList) %>% 
    mutate(value= (is.na(durMin) | (durMin<= duration)) &
                  (is.na(durMax) | (durMax>= duration)) &
                  (is.na(durRelMin) | (durRef*durRelMin <=duration)) &
                  (is.na(durRelMax) | (durRef*durRelMax >=duration))
           ) %>%
    select(day,what,value) 
}
