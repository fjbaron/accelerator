#' Title
#'
#' @param whenList 
#' @param whatList 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
dailyDuration=function(whenList,whatList,...){
  map2_df(whatList,names(whatList),~intervalIntersectv2(whenList,.x) %>% mutate(what=.y)) %>%
    transmute(day=day,from=fromNew,to=toNew,what=what)  %>% group_by(day,what) %>% summarise(
      duration=sum(as.integer(difftime(to,from,units="secs"))),
    ) %>% ungroup() %>% arrange(what,day)
}

