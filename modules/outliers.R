library(DT)
library(plotly)
library(dplyr)
library(rstatix)
library(ggplot2)

source(paste0(getwd(),'/common/df2qqs.R'))
source(paste0(getwd(),'/modules/df2Table.R'))

getOutliers <- function (data, dvs, ivs) {
  dat <- group_by_at(df2qqs(data, ivs), vars(ivs))
  do.call(rbind, lapply(dvs, FUN = function(dv) {
    outliers <- identify_outliers(dat, variable = dv)
    if (nrow(outliers) > 0) {
      return(cbind(var = dv, outliers))
    }
  }))
}

outliersUI <- function(id) {
  ns <- NS(id)
  
  verticalLayout(
    br(), strong("Deteção de outliers"),
    HTML("<p>A tabela a seguir apresenta a lista de todos os outliers</p>"), br(),
    df2TableUI(ns("outliersTable")), br(), hr(),
    checkboxInput(ns("showOutliersBoxPlot"), "Apresentar gráficos de boxplots para identificação de outliers"),
    uiOutput(ns("dataBoxPlotsParams")),
    uiOutput(ns("dataBoxPlotsTabset"))
  )
}

outliersMD <- function(id, initData, outliers, dvs, ivs, wid = 'row.pos') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$dataBoxPlotsParams <- renderUI({
        if (input$showOutliersBoxPlot) {
          radioButtons(ns("boxpoints"), "Visualizaçao de pontos", choices = list(
            "Todos os pontos"="all", "Só suspeitos"="suspectedoutliers", "Só outliers"="outliers"), inline = T)
        }
      })
      
      output$dataBoxPlotsTabset <- renderUI({
        dat <- df2qqs(initData, ivs)
        if (input$showOutliersBoxPlot) {
          do.call(tabsetPanel, c(
            list(type="pills"),
            lapply(dvs, function(dv) {
              tabPanel(
                dv, 
                br(), p(strong(paste("Boxplots da variável", dv))),
                do.call(verticalLayout, lapply(ivs, FUN = function(iv) {
                  verticalLayout(
                    splitLayout(
                      renderPlotly({
                        p <- plot_ly(
                          data=dat, type = "box", boxpoints = input$boxpoints, text=as.formula(paste0("~",wid)),
                          x=as.formula(paste0("~",iv)), y=as.formula(paste0("~",dv)), color=as.formula(paste0("~",iv)))
                        p <- layout(p, title=paste("Com outliers"), showlegend = F)
                        p
                      }),
                      renderPlotly({
                        outliersIds <- subset(outliers, var == dv)[[wid]]
                        p <- plot_ly(
                          data=dat[which(!dat[[wid]] %in%  outliersIds),],
                          type = "box", boxpoints = input$boxpoints, text=as.formula(paste0("~",wid)),
                          x=as.formula(paste0("~",iv)), y=as.formula(paste0("~",dv)), color=as.formula(paste0("~",iv)))
                        p <- layout(p, title=paste("Sem outliers"), showlegend = F)
                        p
                      })
                    ),
                    br(), br()
                  )
                }))
              )
            })
          ))
        }
      })
      
      df2TableMD("outliersTable", outliers)
    }
  )
}
