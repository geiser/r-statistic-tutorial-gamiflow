
library(DT)
library(rstatix)
library(plotly)

source(paste0(getwd(),'/modules/df2Table.R'))

WilcoxonTex <- "
<br/>
<p>
<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{null}:&space;m_{1}&space;=&space;m_{2}\" title=\"H_{null}: m_{1} = m_{2}\" /> (hipótese nula - mediana dos grupos são similares)
</p>
<br/>
<p>
O objetivo do teste de hipótese é determinar a probabilidade (<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;0&space;\\leq&space;p&space;\\leq&space;1\" title=\"0 \\leq p \\leq 1\" />)
dos resultados observados, pressupondo que a hipótese nula seja verdadeira.
</p>
<ul>
<li>O parâmetro de interesse é a diferença entre duas medianas estimadas, <img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;m_{1}&space;-&space;m_{2}\" title=\"m_{1} - m_{2}\" /></li>
</ul>"

hypothesisWilcoxonUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    fixedRow(
      column(
        width = 12,
        fixedRow(
          column(width = 6, HTML(WilcoxonTex)),
          column(width = 2, radioButtons(ns("alternative"), "Direção do teste", inline = F,
                                         choices = list("Bicaudal" = "two.sided", "Maior que" = "greater", "Menor que" = "less")))
        )
      )
    ), hr(),
    df2TableUI(ns("wilcoxonResultTable")), br(), hr(),
    checkboxInput(ns("wilcoxonPlot"), "Apresentar gráficos e como reportar o resultado"),
    uiOutput(ns("wilcoxonPlotParams")), br(), uiOutput(ns("wilcoxonTabset"))
  )
}


hypothesisWilcoxonMD <- function(id, data, dvs, group, dv.var = 'var') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      values <- reactiveValues(w.test = NULL, wt = list(), ez = list())
      
      updateWilcoxonResult <- function() {
        values$w.test <- do.call(rbind, lapply(dvs, FUN = function(dv) {
          dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
          
          sformula <- as.formula(paste(dv, "~", paste0(group, collapse = "*")))
          wt <- wilcox_test(dat, sformula, alternative = input$alternative, paired = F, detailed = T)
          values$wt[[dv]] <- wt
          
          ez <- wilcox_effsize(dat, sformula, alternative = input$alternative, paired = F)
          values$ez[[dv]] <- ez
          return(add_significance(merge(wt, ez)))
        }))
      }
      
      observeEvent(input$alternative, { updateWilcoxonResult() })
      
      observeEvent(values$w.test, {
        cnames <- c(".y.", "group1", "group2", "n1", "n2", "statistic", "estimate",
                    "conf.low", "conf.high", "p", "effsize", "magnitude", "p.signif")
        df2TableMD("wilcoxonResultTable", values$w.test, cnames)
      })
      
      output$wilcoxonPlotParams <- renderUI({
        if (input$wilcoxonPlot) {
          flowLayout(
            numericInput(ns("width"), "Width", value = 500, min=100, step = 50),
            numericInput(ns("height"), "Height", value = 500, min=100, step = 50),
            radioButtons(ns("addParam1"),  "Apresentação de pontos",
                         choices = list("todos"="jitter", "média"= "mean", "não"= "none"), inline = T)
          )
        }
      })
      
      output$wilcoxonTabset <- renderUI({
        if (input$wilcoxonPlot) {
          do.call(tabsetPanel, c(list(type="pills"), lapply(dvs, function(dv) {
            df <- as.data.frame(data[which(data[[dv.var]] == dv),])
            tabPanel(dv, do.call(verticalLayout, lapply(group, FUN = function(iv) {
              stat.test <- add_xy_position(add_significance(values$wt[[dv]]), x=iv)
              bxp <- ggboxplot(
                df, x=iv, y=dv, color=iv, width=0.5, add=input$addParam1, palette="jco"
              )
              bxp <- bxp + stat_pvalue_manual(stat.test, tip.length = 0)
              bxp <- bxp + labs(subtitle = get_test_label(stat.test, detailed=T))
              
              verticalLayout(
                br() , renderPlot({ bxp }, width = input$width, height = input$height), br(),
                strong("Como reportar o resultado ..."), renderText({ 
                  paste("A mediana do grupo", stat.test$group1, "foi de"
                        , round(median(df[[dv]][which(df[[iv]] == stat.test$group1)]), 4)
                        , ", enquanto a mediana no grupo", stat.test$group2, "foi de"
                        , round(median(df[[dv]][which(df[[iv]] == stat.test$group2)]), 4)
                        , "O teste de Wilcoxon "#, ifelse(input$varequal == "FALSE", "Welch", "Student")
                        , "indica que", ifelse(stat.test$p > 0.05, "não há", "há"), "diferença estatisticamente significante com valores"
                        , "W = ", round(stat.test$statistic, 4),", p = ", round(stat.test$p, 4)
                        , " e tamanho de efeito r = ", round(values$ez[[dv]]$effsize, 4), "(",values$ez[[dv]]$magnitude,")"
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


