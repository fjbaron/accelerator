propagateInvalidation=function(when,dfInvalid,cascadeReason,cascadeConsequence){
  invalidate=when %>% pluck(cascadeConsequence) %>% select(day,label) %>% rename(when=label)
  result=dfInvalid
  if(!is.null(invalidate)){}
  dfInvalid %>% 
    filter(when==cascadeReason) %>% select(day,reason) %>%
    left_join(invalidate,by="day") %>%
    filter(!is.na(when)) %>% 
    mutate(reason=str_c(reason,"_",cascadeReason))
}


invalidaEnCascada=function(when,dfInvalid,cascadeList){
  map2_df(names(cascadeList),cascadeList,~map_df(.y,partial(propagateInvalidation,when=when,dfInvalid = dfInvalid,cascadeReason = .x)))
}


getCascadeInvalid=function(when,dfInvalid){
  resultado=dfInvalid
  state_old=resultado %>% distinct(day,when)
  repeat {
    resultado=dfInvalid %>% bind_rows(invalidaEnCascada(when,dfInvalid,currentProject$whenValidity$cascadeInvalid))
    new_state=resultado %>% distinct(day,when)
    if(isTRUE(all.equal(new_state,state_old))) break
    state_old=new_state
  }
  resultado
}


#' Title
#'
#' @param summaryList 
#'
#' @return
#' @export
#'
#' @examples
getInvalidWhen=function(summaryList,when=NULL){
  result=map(summaryList,currentProject$whenValidity$directInvalidation)
  
  
  if(!is.null(when) & 
     !is.null(currentProject$whenValidity$cascadeInvalid) &
     length(currentProject$whenValidity$cascadeInvalid)>0){
    result=map2(when,result,getCascadeInvalid)
  }
  result
}




