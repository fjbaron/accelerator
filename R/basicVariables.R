
nonWear= function(df,...) {
  #' summarises Non Wear time.
  #' Criteria: GGIR. Viene de los epochs, creado en fase 1. FALTA describirlo.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  df %>% mutate(.criterioRaw=.criterioNW | criterioSIB(., critAnglez=2,durBoutMin=dminutes(120)),.criterioBout=.criterioRaw) %>%
    getSummary(maximoHorasNonWear=Inf,minimoHorasValidas = -Inf)
}


wear= function(df,...) {
  #' summarises wear time.
  #' Criteria: GGIR. Viene de los epochs, creado en fase 1. FALTA describirlo.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  df %>% mutate(.criterioRaw=!(.criterioNW | criterioSIB(., critAnglez=2,durBoutMin=dminutes(120))),.criterioBout=.criterioRaw) %>%
    getSummary(maximoHorasNonWear=Inf,minimoHorasValidas = -Inf)
}

bedGGIR = function(df,...) {
  #' summarises acc_onset + acc_wake de fase 5 ggir.
  #' Criteria: GGIR. Viene de los fase 5. FALTA describirlo.
  #' @param df Dataframe of epochs with one columns: datetime
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  if(is.null(df$.criteriocamaGGIR)) df$.criteriocamaGGIR=0
  df %>% mutate(.criterioRaw=.criteriocamaGGIR,.criterioBout=.criterioRaw) %>%
    getSummary(maximoHorasNonWear=Inf,minimoHorasValidas = -Inf)
}


isOn= function(df,...) {
  #' summarises time of Non Wear.
  #' Criteria: GGIR. Viene de los epochs, creado en fase 1. FALTA describir.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  df %>% mutate(.criterioRaw=1,.criterioBout=1) %>%
    getSummary(maximoHorasNonWear=Inf,minimoHorasValidas = -Inf)
}



VPA_Hildebrand_Child <-  function(df,...){
  #' Time in Vigorous Physical Activity for Children (Hildebrand)
  #' 
  #' Find intervals of Vigorous Physical Activity (VPA, >6METs) such as jogging or running.
  #' Criteria: 
  #' \itemize{
  #' \item VPA cut-off for children with ENMO metrics, >700mg, according to Hildebrand 2014: Hildebrand M, Van Hees VT, Hansen BH, Ekelund U. Age-group comparibility of raw accelerometer output from wrist- and hipworn monitors. Med Sci Sports Exerc. 2014;46(9):1816–24.
  #'  \item Non-dominant wrist cut-off point with GENEActiv or Actigraph accelerometer.
  #'  \item  1-s epochs with 100\% of the time in VPA.
  #' }
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return dataframe of time interval matching the criteria
  #' @export
  #'
  enmoOver(df,limInf=700/1000,pctBouts=1,durBoutMin=dseconds(1))
  }


MVPA_Hildebrand_Child  <-  function(df,...){
  #' Time in Moderate and Vigorous Physical Activity for Children (Hildebrand)
  #'
  #' Find intervals of Moderate and Vigorous Physical Activity (MVPA, >3METs) such as brisk walking.
  #' Criteria:   
  #' \itemize{
  #' \item MVPA cut-off for children with ENMO metrics, >200mg, according to Hildebrand 2014: Hildebrand M, Van Hees VT, Hansen BH, Ekelund U. Age-group comparibility of raw accelerometer output from wrist- and hipworn monitors. Med Sci Sports Exerc. 2014;46(9):1816–24.
  #' \item Non-dominant wrist cut-off point with GENEActiv or Actigraph accelerometer.
  #' \item 1-s epochs with 100\% of the time in MVPA.
  #' }
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return dataframe of time intervals matching the criteria
  #' @export
  #'
  enmoOver(df,limInf=200 /1000,pctBouts=1,durBoutMin=dseconds(1))}

INA_Hildebrand_Child  <- function(df,...){
  #' Time in inactivity for Children (Hildebrand)
  #' 
  #' Find intervals of inactivity (INA, <1.5METs) such lying, sitting or standing.
  #' Criteria: 
  #' \itemize{
  #' \item INA inactivity cut-off for children with ENMO metrics <36mg, according to Hildebrand 2017: Hildebrand M, Hansen BH, Van Hees VT, Ekelund U. Evaluation of raw acceleration sedentary thresholds in children and adults. Scand J Med Sci Sports. 2017 Dec;27(12):1814-1823. doi: 10.1111/sms.12795.
  #' \item Non-dominant wrist cut-off point with Actigraph accelerometer.
  #' \item 1-s epochs with 100\% of the time in INA.
  #' }
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return dataframe of time intervals  matching the criteria
  #' @export
  #'
  enmoUnder(df,limSup=36/1000,pctBouts=1,durBoutMin=dseconds(1))}





VPA_B30s=function(df,...) {
  #' Summarises bouts of Vigorous Physical Activity (VPA, >6METs) such as jogging or running.
  #' Criteria: 30-seconds-bouts with >80% of VPA, treashold ENMO <500mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>% mutate(.criterioRaw =criterioENMO(.,limInf = 500/1000)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dseconds(30))) %>%
    getSummary()
}

VPA_B1=function(df,limInf = 400/1000,...) {
  #' Summarises bouts of Vigorous Physical Activity (VPA, >6METs) such as jogging or running.
  #' Criteria: 1-min-bout with >80% of VPA, treashold ENMO <500mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>% mutate(.criterioRaw =criterioENMO(.,limInf = limInf)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(1))) %>%
    getSummary()
}

MVPA_B1=function(df,limInf = 100/1000,...) {
  #' Summarises bouts of Moderate-to-Vigorous Physical Activity (MVPA, >3METs) such as brisk walking.
  #' Criteria: 1-minute-bouts with >80% of MVPA, treashold ENMO <100mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  #'
  df %>% mutate(.criterioRaw =criterioENMO(.,limInf = limInf )) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(1))) %>%
    getSummary()
}


MVPA_B1_H=function(df,...) {
  #' Summarises bouts of Moderate-to-Vigorous Physical Activity (MVPA, >3METs) such as brisk walking.
  #' Criteria: 1-minute-bouts with >80% of MVPA, treashold ENMO <100mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  #'
  df %>% mutate(.criterioRaw =criterioENMO(.,limInf = 100/1000) & ! criterioNoMVPA(.) ) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(1))) %>%
    getSummary()
}



MVPA_B10=function(df,limInf = 100/1000,...) {
  #' Summarises bouts of Moderate-to-Vigorous Physical Activity (MVPA, >3METs) such as brisk walking during at least 10 min.
  #' Criteria: 10-minute-bouts with >80% of MVPA, treashold ENMO <100mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>% mutate(.criterioRaw =criterioENMO(.,limInf = limInf)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(10))) %>%
    getSummary()
}



MVPA_B10_H=function(df,...) {
  #' Summarises bouts of Moderate-to-Vigorous Physical Activity (MVPA, >3METs) such as brisk walking during at least 10 min.
  #' Criteria: 10-minute-bouts with >80% of MVPA, treashold ENMO <100mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>% mutate(.criterioRaw =criterioENMO(.,limInf = 100/1000) & ! criterioNoMVPA(.)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(10))) %>%
    getSummary()
}


LMVPA_B5=function(df,...) {
  #' Summarises bouts of at least Light Physical Activity (LMVPA, >1,5 METs).
  #' Can be used to define inactivity BREAKS.
  #' Criteria: 5min-bouts with >100% of at least LPA, treashold ENMO >40 mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>% mutate(.criterioRaw=criterioENMO(.,limInf = 40/1000)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 1,durBoutMin = dminutes(5))) %>%
    getSummary()
}


LPA_B1=function(df,limInf=40/1000,limSup=100/1000,...) {
  #' Summarises bouts of Light Physical Activity (LPA, <2,9METs) such as lying, sitting, standing, moving, including light home activities.
  #' Criteria: 1-minute-bouts with >80% of LPA, treashold ENMO >40 mg, but excluding MVPA+1.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>%
    mutate(.criterioRaw=criterioENMO (.,limInf=limSup)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(1))) %>%
    mutate(.criterioRaw=!.criterioBout &  criterioENMO(.,limInf=limInf)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(1))) %>%
    getSummary()
}


LPA_B5=function(df,limInf=40/1000,limSup=100/1000,...) {
  #' Summarises bouts of Light Physical Activity (LPA, <2,9METs) such as lying, sitting, standing, moving, including light home activities.
  #' Criteria: 5-minutes-bouts with >80% of Light Physical Activity, treashold ENMO <80mg, but excluding MVPA+1.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>%
    mutate(.criterioRaw=criterioENMO (.,limInf=limSup)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(1))) %>%
    mutate(.criterioRaw=!.criterioBout &  criterioENMO(.,limInf=limInf)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(5))) %>%
    getSummary()
}


LPA_B30=function(df,limInf=40/1000,limSup=100/1000,...) {
  #' Summarises bouts of Light Physical Activity (LPA, <2,9METs) such as lying, sitting, standing, moving, including light home activities.
  #' Criteria: 30-minutes-bouts with >80% of Light Physical Activity, treashold ENMO <80mg, but excluding MVPA+1.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>%
    mutate(.criterioRaw=criterioENMO (.,limInf=limSup)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(1))) %>%
    mutate(.criterioRaw=!.criterioBout &  criterioENMO(.,limInf=limInf)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(30))) %>%
    getSummary()
}


INA_B1=function(df,limSup=40/1000,...) {
  #' Summarises Inactivity bouts (INA, <1.5METs). Activities such as lying, sitting, standing, moving slowly.
  #' Criteria: 1-minute-bout with 100% of inactivity, treashold ENMO <40mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>%
    mutate(.criterioRaw= criterioENMO(.,limSup=limSup)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 01,durBoutMin = dminutes(1))) %>%
    getSummary()
}


INA_B30=function(df,limSup=40/1000,...) {
  #' Summarises Inactivity (INA, <1.5METs) bout of 30 min without breaks of LPA30s+. Activities such as lying, sitting, standing, moving slowly.
  #' Criteria: 30-minute-bout with >90% of inactivity, treashold ENMO <40mg, but can't include breaks of LPA30s+.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>%
    mutate(.criterioRaw=criterioENMO (.,limInf=limSup)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 1,durBoutMin=dseconds(30))) %>%
    mutate(.criterioRaw=!.criterioBout &  criterioENMO(.,limSup=limSup)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.90,durBoutMin = dminutes(30))) %>%
    getSummary()
}


OINA_B1=function(df,limSup=40/1000,...) {
  #' Summarises Inactivity bouts (INA, <1.5METs). But no SIBs are included. Activities such as lying, sitting, standing, moving slowly.
  #' Criteria: 1-minute-bout with 100% of inactivity, treashold ENMO <40mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>%
    mutate(.criterioRaw=criterioSIB(.)) %>%
    mutate(.criterioRaw= !.criterioRaw & criterioENMO(.,limSup=limSup)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.95,durBoutMin = dminutes(1))) %>%
    getSummary()
}

OINA_B30=function(df,limSup=40/1000,...) {
  #' Summarises Inactivity bouts (INA, <1.5METs). But no SIBs are included. Activities such as lying, sitting, standing, moving slowly.
  #' Criteria: 30-minute-bout with 100% of inactivity, treashold ENMO <40mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>%
    mutate(.criterioRaw=criterioSIB(.)) %>%
    mutate(.criterioRaw= !.criterioRaw & criterioENMO(.,limSup=limSup)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.95,durBoutMin = dminutes(30))) %>%
    getSummary()
}




SIB=function(df,critAnglez = 5, durBoutMin = dminutes(5),...) {
  #' Summarises Sustained Inactivity Bouts (SIB) according to the criteria by Vincent van Hees
  #' Critera: Identify 5-min-bouts with < 5 degrees change in angle.
  #' @param df Dataframe of epochs with two columns: datetime and XXXXXX.
  #' @return a list with a summary of the periods matching the criteria.
  #' @export
  #'
  df %>%
    mutate(.criterioRaw=criterioSIB(.,critAnglez =critAnglez, durBoutMin = durBoutMin),.criterioBout=.criterioRaw) %>%
    getSummary(offset=dhours(2))
}



ZIB=function(df,critAnglez = 5, durBoutMin = dminutes(5),...) {
  #' Title
  #'
  #' @param df 
  #' @param critAnglez 
  #' @param durBoutMin 
  #' @param ... 
  #'
  #' @return
  #' @export
  #'
  #' @examples  
  df %>%
    mutate(.criterioRaw=criterioZIB(.,critAnglez =critAnglez, durBoutMin = durBoutMin),.criterioBout=.criterioRaw) %>%
    getSummary(offset=dhours(2))
}




SIB2=function(df,...) {
  #' Summarises Sustained Inactivity Bouts (SIB) according to the criteria by Vincent van Hees
  #' Critera: Identify 5-min-bouts with < 5 degrees change in angle.
  #' @param df Dataframe of epochs with two columns: datetime and XXXXXX.
  #' @return a list with a summary of the periods matching the criteria.
  #' @export
  #'
  df %>%
    mutate(.criterioRaw=criterioSIB(.,critAnglez = 10, durBoutMin = dminutes(1)),.criterioBout=.criterioRaw) %>%
    getSummary(offset=dhours(2))
}


MuyQuieto=function(df,...) {
  #' Summarises bouts of very quiet home circuits (<1,5METs).
  #' Mainly lying or sitting, but allows for a 20% of interruptions, including quiet moving circuits principally at nightime.
  #' Varible used to define In Bed criteria.
  #' Criteria: 30-minutes-bouts with >80% of inactivity, treashold ENMO <25mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>% mutate(.criterioRaw=criterioENMO(.,limSup = 12/1000,useNW=FALSE)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.95,durBoutMin = dminutes(20))) %>%
    getSummary()
}


INA_Break10min=function(df,...) {
  #' Summarises bouts of at least Light Physical Activity (LMVPA, >1,5 METs).
  #' Criteria: 10 min-bouts with >70% of at least LPA, treashold ENMO >20 mg.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  #'
  df %>% mutate(.criterioRaw=criterioENMO(.,limInf = 20/1000)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = 0.8,durBoutMin = dminutes(10))) %>%
    getSummary()
}








