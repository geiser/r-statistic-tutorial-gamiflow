library(rstatix)

source(paste0(getwd(),'/modules/df2Table.R'))

descriptiveStatisticsUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    br(), strong("Estatística Descritiva"), br()
    , df2TableUI(ns("descrStatisticTable"))
  )
}


descriptiveStatisticsMD <- function(id, data, dvs, group=c(), dv.var = 'var') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      df <- do.call(rbind, lapply(dvs, FUN = function(dv) {
        dat <- as.data.frame(data[,dv])
        colnames(dat) <- c(dv)
        if (!is.null(dv.var)) {
          dat <- as.data.frame(data[which(data[[dv.var]] == dv), unique(c(dv, group))])
        }
        if (length(group) > 0) dat <- group_by_at(dat, vars(group))
        df <- get_summary_stats(dat)
        if (nrow(df) > 0) return(as.data.frame(df))
      }))
      
      df2TableMD("descrStatisticTable", df)
    }
  )
}

