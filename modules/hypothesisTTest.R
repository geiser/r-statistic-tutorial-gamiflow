
library(DT)
library(rstatix)
library(plotly)

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
A média dos grupos são aproximadamente iguais.
</li>
<li> Hipótese alternativa - Bicaudal
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A média dos grupos são significativamente diferentes.
</li>
<li> Hipótese alternativa - Maior que
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A média do grupo 1 é significativamente maior do que a média do grupo 2.
</li>
<li> Hipótese alternativa - Menor que
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
A média do grupo 1 é significativamente menor do que a média do grupo 2.
</li>
</ul>
</p>
<br/>
<p>
Observações:
<ul>
<li>O parâmetro de interesse é a diferença entre duas médias, <img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;u_{1}&space;-&space;u_{2}\" title=\"u_{1} - u_{2}\" /></li>
<li>t-statistic é <img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;s&space;=&space;\\frac{(\\overline{x}_{1}&space;-&space;\\overline{x}_{2})-(u_{1}&space;-&space;u_{2})}{\\sqrt{\\frac{s_{1}^{2}}{n_{1}}&space;&plus;&space;\\frac{s_{2}^{2}}{n_{2}}}}\\\" title=\"statistic = \\frac{(\\overline{x}_{1} - \\overline{x}_{2})-(u_{1} - u_{2})}{\\sqrt{\\frac{s_{1}^{2}}{n_{1}} + \\frac{s_{2}^{2}}{n_{2}}}}\" /></li>
</ul>
</p>"

hypothesisTTestUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    fixedRow(
      column(
        width = 12,
        fixedRow(
          column(width = 6, HTML(tTestTex)),
          column(width = 2, radioButtons(ns("alternative"), "Direção do teste", inline = F,
                                         choices = list("Bicaudal" = "two.sided", "Maior que" = "greater", "Menor que" = "less"))),
          column(width = 2, radioButtons(ns("varequal"), "Método", inline = F,
                                         choices = list("Welch's t-test" = "FALSE", "Student's t-test" = "TRUE"))),
          column( width = 2, radioButtons(ns("hedgescorrection"), "Hedges' effsize", inline = F,
                                          choices = list("Não" = "FALSE", "Sim" = "TRUE")))
        )
      )
    ), hr(),
    df2TableUI(ns("ttestResultTable")), br(), hr(),
    checkboxInput(ns("tTestPlot"), "Apresentar gráficos e como reportar o resultado"),
    uiOutput(ns("boxPlotParams")), br(), uiOutput(ns("tTestTabset"))
  )
}


hypothesisTTestMD <- function(id, data, dvs, group, dv.var = 'var') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      values <- reactiveValues(t.test = NULL, tt = list(), ez = list())
      
      updateTTestResult <- function() {
        values$t.test <- do.call(rbind, lapply(dvs, FUN = function(dv) {
          dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
          
          sformula <- as.formula(paste(paste0('`',dv,'`'), "~", paste0(paste0('`',group,'`'), collapse = "*")))
          tt <- t_test(dat, sformula, alternative = input$alternative
                       , var.equal = as.logical(input$varequal), detailed = T)
          values$tt[[dv]] <- tt
          
          ez <- cohens_d(dat, sformula, var.equal = as.logical(input$varequal)
                         , hedges.correction = as.logical(input$hedgescorrection))
          values$ez[[dv]] <- ez
          
          return(add_significance(merge(tt, ez)))
        }))
      }
      
      observeEvent(input$alternative, { updateTTestResult() })
      observeEvent(input$varequal, { updateTTestResult() })
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
            radioButtons(ns("addParam1"),  "Apresentação de pontos",
                         choices = list("todos"="jitter", "média"= "mean", "não"= "none"), inline = T)
          )
        }
      })
      
      output$tTestTabset <- renderUI({
        if (input$tTestPlot) {
          do.call(tabsetPanel, c(list(type="pills"), lapply(dvs, function(dv) {
            df <- as.data.frame(data[which(data[[dv.var]] == dv),])
            tabPanel(dv, do.call(verticalLayout, lapply(group, FUN = function(iv) {
              stat.test <- add_xy_position(add_significance(values$tt[[dv]]), x=iv)
              bxp <- ggboxplot(
                df, x=iv, y=dv, color=iv, width=0.5, add=input$addParam1, palette="jco"
              )
              bxp <- bxp + stat_pvalue_manual(stat.test, tip.length = 0)
              bxp <- bxp + labs(subtitle = get_test_label(stat.test, detailed=T))
              
              verticalLayout(
                br() , renderPlot({ bxp }, width = input$width, height = input$height), br(),
                strong("Como reportar o resultado ..."), renderText({ 
                  paste("A média do grupo", stat.test$group1, "foi de"
                        , round(mean(df[[dv]][which(df[[iv]] == stat.test$group1)]), 4)
                        , ", enquanto a média no grupo", stat.test$group2, "foi de"
                        , round(mean(df[[dv]][which(df[[iv]] == stat.test$group2)]), 4)
                        , "O t-teste de", ifelse(input$varequal == "FALSE", "Welch", "Student")
                        , "indica que", ifelse(stat.test$p > 0.05, "não há", "há"), "diferença estatisticamente significante com valores"
                        , "t(",round(stat.test$df, 4),") = ", round(stat.test$statistic, 4),", p = ", round(stat.test$p, 4)
                        , " e tamanho de efeito d = ", round(values$ez[[dv]]$effsize, 4), "(",values$ez[[dv]]$magnitude,")"
                  )
                })
              )
            })))
          })))
        }
      })
    }
  )
}
