
library(DT)
library(plotly)
library(rstatix)


source(paste0(getwd(),'/common/df2qqs.R'))
source(paste0(getwd(),'/common/qqPlotly.R'))
source(paste0(getwd(),'/common/shapiro.R'))

source(paste0(getwd(),'/common/identify_nonnormal.R'))
source(paste0(getwd(),'/common/dealing_with_groups.R'))

source(paste0(getwd(),'/modules/qqPanelPlots.R'))
source(paste0(getwd(),'/modules/df2Table.R'))

normalityTest <- "
<p><b>Teste de normalidade</b>:
Para efetuar testes paramétricos, se a amostra é suficientemente grande (pelo menos 30 observações), a distribuição da média
precissa apenas ter uma distribuição <i>aproximadamente</i> normal, não estritamente normal. Em amostras de tamanho pequeno,
a abordagem para seleção de dados deve ser proveniente de uma distribuição normal através dos gráficos Q-Q.
</p>
<ul>
<li> Hipótese nula
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{null}\" title=\"H_{null}\" />):
Os dados são provenientes de uma distribuição normal.
</li>
<li> Hipótese alternativa
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
Os dados são provenientes de uma distribuição não normal.
</li>
</ul>
"



getNonNormality <- function(dat, dvs, ivs, wid, outliers, type="aov") {
  dat <- df2qqs(dat, ivs)
  do.call(rbind, lapply(dvs, FUN = function(dv) {
    outlierIds <- outliers[[wid]][which(outliers$var == dv)]
    nonNormalityIds <- identify_nonnormal(dat, dv, ivs, wid, outlierIds)
    pdat <- dat[!dat[[wid]] %in% nonNormalityIds,]
    if (nrow(pdat) > 0) {
      cbind(var = dv, mutate(pdat, is.outlier = pdat[[wid]] %in% outlierIds))
    }
  }))
}

nonNormalityUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    br(), p(strong("Shapiro-Wilk normality test")), br(),
    div(HTML(normalityTest)), br(),
    br(), p(strong("Teste Shapiro-Wilk no modelo residual")),
    df2TableUI(ns("shapiroResTable")),
    br(), checkboxInput(ns("showQQPlot"), "Apresentar gráficos Q-Q"),
    uiOutput(ns("qqPlotUI")), br(),
    #uiOutput(ns("parameterPlotUI")), uiOutput(ns("qqPlotTabset")), br(),
    br(), p(strong("Teste Shapiro-Wilk para todos os grupos")),
    df2TableUI(ns("shapiroGroupTable")), br(),
    checkboxInput(ns("showInadequate"), paste("Apresentar grupos nos quais não foir efetuado teste",
                                              "de normalidade pelo tamanho da amostra"), width = "100%"), br(),
    uiOutput(ns("sampleSizeGroupUI"))
  )
}

nonNormalityMD <- function(id, data, dvs, between = c(), within = c(), wid = 'UserID', dv.var = 'var') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      values <- reactiveValues(
        sample.size.problem = do.call(rbind, lapply(dvs, FUN = function(dv) {
          group <- between
          dat <- data_with_min_per_group(subset(data, var == dv), group)
          dat <- dat[!is.na(dat[[wid]]),]
          if (nrow(dat) > 0) return(cbind(var = dv, freq_table(dat, vars = group)))
        })),
        shapiro.res = shapiro_by_res(data, dvs, between, within, dv.var),
        shapiro.group = shapiro_per_group(data, dvs, between, dv.var)
      )
      
      observeEvent(values$shapiro.res, {
        df2TableMD("shapiroResTable", values$shapiro.res)
      })
      
      observeEvent(values$shapiro.group, {
        df2TableMD("shapiroGroupTable", values$shapiro.group)
      })
      
      #
      
      output$qqPlotUI <- renderUI({
        if (input$showQQPlot) {
          verticalLayout(
            fixedRow(
              column(width = 3, numericInput(ns("width"), "Plot width", value = 400, min=100, step = 50)),
              column(width = 3, numericInput(ns("height"), "Plot height", value = 400, min=100, step = 50))
            ),
            do.call(tabsetPanel, c(list(id = ns('qqPanelSet'), type= 'pills'), lapply(dvs, FUN = function(dv) {
              tabPanel(
                dv, value = dv,
                qqPanelPlotsUI(ns(paste0("qqPanel", dv)), dv)
              )
            })))
          )
        }
      })
      
      observeEvent(input$qqPanelSet, {
        dv <- input$qqPanelSet
        qqPanelPlotsMD(paste0("qqPanel", dv), data, dv,
                       between = between, wid = wid, dv.var = 'var', width = input$width, height = input$height)
      })
      
      
      # .. show inadequate groups
      
      output$sampleSizeGroupUI <- renderUI({
        if (input$showInadequate) {
          verticalLayout(
            strong("Groups with inadequate sample size (n < 3)"),
            renderDataTable({
              if (!is.null(values$sample.size.problem) && nrow(values$sample.size.problem) > 0) {
                df2DT(values$sample.size.problem)
              }
            })
          )
        }
      })
      
    }
  )
}

