
library(DT)
library(rstatix)
library(plotly)
library(tidyr)

source(paste0(getwd(),'/modules/df2Table.R'))

tTestTex <- "
<br/>
<p>
O objetivo do teste de hipótese é determinar a probabilidade (<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;0&space;\\leq&space;p&space;\\leq&space;1\" title=\"0 \\leq p \\leq 1\" />)
dos resultados observados, pressupondo que a hipótese nula seja verdadeira.
</p>
<p>
<ul>
<li> Hipótese nula
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{null}\" title=\"H_{null}\" />):
A diferença média entre as duas observações é zero.
</li>
<li> Hipótese alternativa - Bicaudal
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A diferença média entre as duas observações é significativamente diferente de zero.
</li>
<li> Hipótese alternativa - Maior que
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A diferença média entre as duas observações é significativamente maior que zero.
</li>
<li> Hipótese alternativa - Menor que
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A diferença média entre as duas observações é significativamente menor que zero.
</li>
</ul>
</p>
"

hypothesisPairedTTestUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    fixedRow(
      column(
        width = 12,
        fixedRow(
          column(width = 6, HTML(tTestTex)),
          column(width = 2, radioButtons(ns("alternative"), "Direção do teste", inline = F, choices = list("Bicaudal" = "two.sided", "Maior que" = "greater", "Menor que" = "less"))),
          column( width = 2, radioButtons(ns("hedgescorrection"), "Hedges' effsize", inline = F, choices = list("Não" = "FALSE", "Sim" = "TRUE")))
        )
      )
    ), hr(),
    df2TableUI(ns("ttestResultTable")), br(), hr(),
    checkboxInput(ns("tTestPlot"), "Apresentar gráficos e como reportar o resultado"),
    uiOutput(ns("boxPlotParams")), br(), uiOutput(ns("tTestTabset"))
  )
}


hypothesisPairedTTestMD <- function(id, wdata, dvCondition1, dvCondition2, wid) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      dat <- as.data.frame(wdata[which(wdata[['var']] == 'diff'),])
      dat <- as.data.frame(pivot_longer(dat, c(dvCondition1, dvCondition2), names_to = "group"))
      
      values <- reactiveValues(data = dat, t.test = NULL, tt = NULL, ez = NULL)
      
      updateTTestResult <- function() {
        values$tt <- t_test(values$data, value ~ group, alternative = input$alternative, paired = T, detailed = T)
        values$ez <- cohens_d(values$data, value ~ group, hedges.correction = as.logical(input$hedgescorrection), paired = T)
        values$t.test <- add_significance(merge(values$tt, values$ez))
      }
      
      observeEvent(input$alternative, { updateTTestResult() })
      observeEvent(input$hedgescorrection, { updateTTestResult() })
      
      observeEvent(values$t.test, {
        cnames <- c(".y.", "group1", "group2", "n1", "n2", "df", "statistic", "estimate",
                    "conf.low", "conf.high", "p", "effsize", "magnitude", "p.signif")
        df2TableMD("ttestResultTable", values$t.test, cnames)
      })
      
      output$boxPlotParams <- renderUI({
        if (input$tTestPlot) {
          flowLayout(
            numericInput(ns("width"), "Width", value = 500, min=100, step = 50),
            numericInput(ns("height"), "Height", value = 500, min=100, step = 50),
            textInput(ns("xlab"), "Etiqueta do eixo x", "condição"),
            textInput(ns("ylab"), "Etiqueta do eixo y", "valor"),
            radioButtons(ns("addParam1"),  "Apresentação de pontos",
                         choices = list("todos"="jitter", "média"= "mean", "não"= "none"), inline = T)
          )
        }
      })
      
      output$tTestTabset <- renderUI({
        if (input$tTestPlot) {
          stat.test <- add_xy_position(add_significance(values$tt), x = "group")
          print(wid)
          print(values$data)
          bxp <- ggpaired(values$data, x = "group", y = "value", color = "group", id=wid,
                          width=0.5, add=input$addParam1, palette="jco",
                          order = c(dvCondition1, dvCondition2),
                          ylab = input$ylab, xlab = input$xlab)
          
          bxp <- bxp + stat_pvalue_manual(stat.test, tip.length = 0)
          bxp <- bxp + labs(subtitle = get_test_label(stat.test, detailed= T))
          
          verticalLayout(
            br() , renderPlot({ bxp }, width = input$width, height = input$height), br(),
            strong("Como reportar o resultado ..."),
            renderText({
              signifText <- ""
              
              descr.df <- get_summary_stats(wdata)
              idx <- which(descr.df$variable == dvCondition1)
              dvMetric1 <- paste0('(M=', round(descr.df$mean[idx], 2), ', SD=', round(descr.df$sd[idx], 2), ')')
              idx <- which(descr.df$variable == dvCondition2)
              dvMetric2 <- paste0('(M=', round(descr.df$mean[idx], 2), ', SD=', round(descr.df$sd[idx], 2), ')')
              
              if (values$t.test$p.signif != "ns") {
                signifText <- paste(
                  "There was a significant difference in the", input$ylab, "for",
                  dvCondition1, dvMetric1, "and", dvCondition2, dvMetric2,"conditions with",
                  paste0("t(",values$t.test$df,") ="), round(values$t.test$statistic, 2),", p",
                  ifelse(values$t.test$p < 0.0001, '< 0.0001', paste0('= ',round(values$t.test$p, 2))),
                  ", and",
                  ifelse(as.logical(input$hedgescorrection), "Hedge's g", "Cohen's d")
                  ,'=', round(values$t.test$effsize, 2),
                  '(', as.character(values$t.test$magnitude), ')')
              } else {
                signifText <- paste(
                  "There was not significant difference in the", input$ylab, "for",
                  dvCondition1, dvMetric1, "and", dvCondition2, dvMetric2,"conditions")
              }
              paste("A paired-samples t-test was conducted to compare", input$ylab, "in",
                    dvCondition1, "and", dvCondition2, "conditions.", signifText)
            })
          )
        }
      })
    }
  )
}
