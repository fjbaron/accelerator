#' Convert a dataframe of intervals, given a vector of timestamps representing epochs to
#' a boolean vector representing if that epoch belong to one of the intervals
#'
#' @param ts vector of timestamps
#' @param intervalos dataframe of intervals, with columns \code{to} and \code {from}
#' @param ts 
#'
#' @param intervalos 
#'
#' @return a logical vector of length(ts) indicating if that time velong to a interval
#'
#' @export
interval2criterio=function(ts,intervalos){
  #ts=df$timestamp
  #intervalos=intervalosNW
  durEpoch=as.duration(ts[2]-ts[1])
  criterio=vector("logical",length(ts))

  if(is.null(intervalos)  | nrow(intervalos)==0) return(criterio)

  for(i in 1:nrow(intervalos)){
    iDesde = min(difftime(intervalos[["from"]][i],ts[1])/durEpoch,length(ts)-1)
    iHasta = min(difftime(intervalos[["to"]][i],ts[1])/durEpoch,length(ts)-1)
#    message(iDesde, " ",iHasta,"\n")
    if(iDesde<=iHasta) criterio[1+iDesde:iHasta]=1
  }

  criterio
}
