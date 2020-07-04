library(rstatix)

source(paste0(getwd(),'/modules/df2Table.R'))

descriptiveStatisticsUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    br(), strong("Estatística Descritiva"), br()
    , df2TableUI(ns("descrStatisticTable"))
  )
}


descriptiveStatisticsMD <- function(id, data, dvs, group, dv.var = 'var') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      df2TableMD("descrStatisticTable", do.call(rbind, lapply(dvs, FUN = function(dv) { 
        dat <- as.data.frame(data[which(data[[dv.var]] == dv), unique(c(dv, group))])
        df <- get_summary_stats(group_by_at(dat, vars(group)))
        if (nrow(df) > 0) return(as.data.frame(df))
      })))
    }
  )
}

