#' Title
#'
#' @param dfWhatWhen 
#' @param variablesWhatWhen 
#' @param functionDailySummary 
#'
#' @return
#' @export
#'
#' @examples
generalDailySummary=function(dfWhatWhen,variablesWhatWhen,functionDailySummary) {
  argnames <- sys.call()
  cat("\nComputing summary: ",unlist(lapply(argnames, as.character)) %>% paste(sep=" ",collapse=" "),"\n")
  pb <- progress_estimated(nrow(dfWhatWhen))
  summaryWithProgress <-function(what,when,...) {
    pb$tick()$print()
    generalDailySummaryPhase1(what,when,variablesWhatWhen=variablesWhatWhen,functionDailySummary=functionDailySummary,...)
  }
  dfWhatWhen  %>% pmap(summaryWithProgress)
}


generalDailySummaryPhase1=function(what,when,variablesWhatWhen,functionDailySummary,...){
  map2_df(variablesWhatWhen,names(variablesWhatWhen), ~ generalDailySummaryPhase2(what,when,whenTxt=.y,whatVector=.x,functionDailySummary=functionDailySummary,...))
}


generalDailySummaryPhase2=function(what,when,whenTxt,whatVector,functionDailySummary,...){
  functionDailySummary(whenList=when %>% pluck(whenTxt), whatList=what[intersect(whatVector,names(what))]) %>% mutate(when=whenTxt)
}


