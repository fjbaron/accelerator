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
  summary %>% filter(when==whenCond,what==whatCond) %>% 
    filter((!is.na(minDur) & as.duration(duration)<=as.duration(minDur))|
             (!is.na(maxDur) & as.duration(duration)>=as.duration(maxDur)) ) %>%
    select(day) %>% mutate(when=whenInvalid,reason=reason)
}
