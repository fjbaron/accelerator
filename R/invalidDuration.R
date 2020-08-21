#' Title
#'
#' @param summary 
#' @param whatCond 
#' @param whenCond 
#' @param minDur 
#' @param maxDur 
#' @param whenInvalid 
#' @param reason 
#'
#' @return
#' @export
#'
#' @examples
invalidDuration=function(summary,whatCond,whenCond,minDur=NA,maxDur=NA,whenInvalid=NA_character_,reason="unexplained"){
  if(is.na(whenInvalid)) whenInvalid=whenCond
  asDurationMin="Duration" %in% class(minDur) 
  asDurationMax="Duration" %in% class(maxDur)
  summary %>% filter(when==whenCond,what==whatCond) %>%
    mutate(loLim=ifelse(asDurationMin,minDur, as.duration(minDur*durationRef)),
           hiLim=ifelse(asDurationMax,maxDur, as.duration(maxDur*durationRef))) %>% 
    filter((!is.na(minDur) & as.duration(duration)<as.duration(loLim))|
             (!is.na(maxDur) & as.duration(duration)>as.duration(hiLim)) ) %>%
    select(day) %>% mutate(when=whenInvalid,reason=reason)
}
