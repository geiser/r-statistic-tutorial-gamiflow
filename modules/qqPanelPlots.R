
library(DT)
library(plotly)
library(rstatix)

source(paste0(getwd(),'/common/df2qqs.R'))
source(paste0(getwd(),'/common/qqPlotly.R'))
source(paste0(getwd(),'/common/identify_nonnormal.R'))

source(paste0(getwd(),'/modules/df2Table.R'))

qqPanelPlotsUI <- function(id, dv = id) {
  ns <- NS(id)
  verticalLayout(
    br(), p(strong(paste("Gráfico Q-Q do modelo residual:", dv))),
    plotOutput(ns("qqPlotResidual")), br(), hr(),
    uiOutput(ns("qqPlotGroups"))
  )
}

qqPanelPlotsMD <- function(id, data, dv, between = c(), within = c(), wid = 'UserID', dv.var = NULL, width = 400, height = 400) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      values <- reactiveValues()
      
      output$qqPlotResidual <- renderPlot({
        dat <- data
        if (!is.null(dv.var)) dat <- dat[which(dat[[dv.var]] == dv),]
        sformula <- paste(dv ,'~', paste0(between, collapse = "*"))
        rownames(dat) <- dat[[wid]]
        mdl <- lm(as.formula(sformula), data = dat)
        car::qqPlot(residuals(mdl))
      }, width = width, height = height)
      
      output$qqPlotGroups <- renderUI({
        group <- between
        dat <- data
        if (!is.null(dv.var)) dat <- dat[which(dat[[dv.var]] == dv),]
        
        freq_df <- subset(freq_table(dat, vars = group), n >= 3)
        do.call(verticalLayout, lapply(seq(1,nrow(freq_df)), FUN = function(i) {
          tbl <- freq_df[i,c(group)]
          df <- as.data.frame(subset_by_tbl(dat, tbl, group = group))
          df <- group_by_at(df, vars(group))
          
          ntitle <- paste(sapply(names(tbl), FUN = function(nc) paste0(nc,':', tbl[[nc]])), collapse = " - ")
          verticalLayout(
            br(), p(strong(paste("Gráfico Q-Q dos modelos por grupo, ", ntitle))),
            splitLayout(
              renderPlot({
                rownames(df) <- df[[wid]]
                car::qqPlot(as.formula(paste('~', dv)), data = df)
              }, width = width, height = height),
              renderPlotly({
                plotly::layout(qqPlotly(df[[wid]], df[[dv]], width = width, height = height), title = 'Interative Q-Q')
              })
            )
          )
        }))
      })
    }
  )
}
