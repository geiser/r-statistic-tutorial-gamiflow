library(dplyr)

source(paste0(getwd(),'/common/utilities.R'))

source(paste0(getwd(),'/modules/df2Table.R'))
source(paste0(getwd(),'/modules/outliers.R'))
source(paste0(getwd(),'/modules/nonNormality.R'))
source(paste0(getwd(),'/modules/homogeneity.R'))
source(paste0(getwd(),'/modules/hypothesisPairedTTest.R'))
source(paste0(getwd(),'/modules/descriptiveStatistics.R'))

pairedTTestUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Paired t-test"),
    sidebarLayout(
      sidebarPanel(width = 3, uiOutput(ns("tTestParamsUI"))),
      mainPanel(width = 9, uiOutput(ns("tTestTabsetUI")))
    )
  )
}


pairedTTestMD <- function(id, initData) {
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      values <- reactiveValues(initData = cbind(row.pos = seq(1, nrow(initData)), initData), dvs = 'diff',
                               outliers = NULL, nonNormality = NULL, data = NULL, isSetupVars = F)
      
      output$tTestTabsetUI <- renderUI({
        if (values$isSetupVars) {
          tabsetPanel(
            id = ns("tTestTabset"), type = "tabs",
            tabPanel("Teste de hipóteses", value = "hypothesisTest", hypothesisPairedTTestUI(ns("tTestSummaryUI"))),
            tabPanel("Premissa: Outliers", value = "outlierAssumption", outliersUI(ns("outlierAssumptionUI"))),
            tabPanel("Premissa: Distr. Normal", value = "normalityAssumption", nonNormalityUI(ns("normalityAssumptionUI"))),
            tabPanel("Estatística descritiva", value = "descrStatistics", descriptiveStatisticsUI(ns("descrStatisticsUI")))
          )
        }
      }) 
      
      output$tTestParamsUI <- renderUI({
        choices <- c(get_choices(initData, "id"), "row.pos")
        verticalLayout(
          fluid = T,
          selectInput(ns('wid'), 'Identificador da obs', choices = choices, multiple = F),
          uiOutput(ns('condition1UI')), uiOutput(ns('condition2UI')),
          uiOutput(ns('assumptionParamsUI')),
          actionButton(ns('executeButton'), "Executar teste")
        )
      })
      
      output$condition1UI <- renderUI({
        choices <- get_choices(initData, "dv", input$wid)
        selectInput(ns('dvCondition1'), 'Dados coletados na primeira condição', choices = choices, multiple = F)
      })
      
      observeEvent(input$dvCondition1, {
        if (length(input$dvCondition1) > 0 && length(input$dvCondition2) > 0) {
          values$initData[["diff"]] = values$initData[[input$dvCondition2]] - values$initData[[input$dvCondition1]]
          shinyjs::enable("executeButton")
        } else {
          shinyjs::disable("executeButton")
        }
      })
      
      output$condition2UI <- renderUI({
        choices <- get_choices(initData, "dv", c(input$wid, input$dvCondition1))
        selectInput(ns('dvCondition2'), 'Dados coletados na segunda condição', choices = choices, multiple = F)
      })
      
      observeEvent(input$dvCondition2, {
        if (length(input$dvCondition1) > 0 && length(input$dvCondition2) > 0) {
          values$initData[["diff"]] = values$initData[[input$dvCondition2]] - values$initData[[input$dvCondition1]]
          shinyjs::enable("executeButton")
        } else {
          shinyjs::disable("executeButton")
        }
      })
      
      ## .. outliers e dados que afetam a distribuição não normal 
      
      output$assumptionParamsUI <- renderUI({
        if (values$isSetupVars) {
          verticalLayout(
            hr(), strong("Indicar outliers"),
            actionLink(ns("identifyingOutliers"), "Identificar automaticamente outliers"),
            uiOutput(ns('outliersInputUI')), hr(),
            strong("Indicar obs que afetam a distribuição normal"),
            uiOutput(ns('nonNormalInputUI'))           
          )
        }
      })
      
      ## .. dealing with outliers 
      
      outliersObserve <- observe({
        wid <- input$wid
        values$outliers <- do.call(rbind, lapply(values$dvs, FUN = function(dv) {
          outlierIds <- input[[paste0('outliers', dv, 'Input')]]
          dat <- values$initData[values$initData[[wid]] %in% outlierIds,]
          if (length(outlierIds) > 0 && nrow(dat) > 0) return(cbind(var = dv, dat))
        }))
      }, suspended = T)
      
      observeEvent(input$identifyingOutliers, {
        wid <- input$wid
        for (dv in values$dvs) {
          outliers <- getOutliers(values$initData, dv)
          selected <- outliers[[wid]][which(outliers$var == dv)]
          updateSelectInput(session, paste0('outliers', dv, 'Input'), selected=selected)
        }
      })
      
      output$outliersInputUI <- renderUI({
        wid <- input$wid
        outliersInputs <- lapply(values$dvs, FUN = function(dv) {
          choices <- values$initData[[wid]]
          outliers <- getOutliers(values$initData, dv)
          selected <- outliers[[wid]][which(outliers$var == dv)]
          selectInput(ns(paste0('outliers', dv, 'Input')), dv, choices=choices, selected=selected, multiple=T)
        })
        do.call(verticalLayout, outliersInputs)
      })
      
      ## .. dealing with non-normality 
      
      nonNormalityObserve <- observe({
        wid <- input$wid
        values$nonNormality <- do.call(rbind, lapply(values$dvs, FUN = function(dv) {
          nonNormalityIds <- input[[paste0('nonNormality',dv,'Input')]]
          dat <- values$initData[values$initData[[wid]] %in% nonNormalityIds,]
          if (length(nonNormalityIds) > 0 && nrow(dat) > 0) return(cbind(var = dv, dat))
        }))
      }, suspended = T)
      
      output$nonNormalInputUI <- renderUI({
        wid <- input$wid
        nonNormalityInputs <- lapply(values$dvs, FUN = function(dv) {
          choices <- values$initData[[wid]]
          selectInput(ns(paste0('nonNormality', dv, 'Input')), dv, choices, multiple=T)
        })
        do.call(verticalLayout, nonNormalityInputs)
      })
      
      ## .. update data
      
      observeEvent(input$executeButton, {
        if (values$isSetupVars) {
          outliersObserve$suspend();
          nonNormalityObserve$suspend();
          
          shinyjs::enable("wid")
          shinyjs::enable("dvCondition1")
          shinyjs::enable("dvCondition2")
          updateActionButton(session, "executeButton", label = "Voltar a executar teste")
          
          values$initData = cbind(row.pos = seq(1, nrow(initData)), initData,
                                  diff = initData[[input$dvCondition2]] - initData[[input$dvCondition2]])
          values$isSetupVars <- FALSE
        } else { #executar teste
          updateActionButton(session, "executeButton", label = "Parar teste e mudar variáveis")
          
          shinyjs::disable("wid")
          shinyjs::disable("dvCondition1")
          shinyjs::disable("dvCondition2")
          
          values$isSetupVars <- TRUE
          
          updateData()
          outliersObserve$resume()
          nonNormalityObserve$resume()
        }
      })
      
      updateData <- function() {
        wid <- input$wid
        values$data <- do.call(rbind, lapply(values$dvs, FUN = function(dv) {
          
          outlierIds <- c()
          if (!is.null(values$outliers) && nrow(values$outliers) > 0) {
            outlierIds <- values$outliers[[wid]][which(values$outliers$var == dv)]
          }
          
          nonNormalityIds <- c()
          if (!is.null(values$nonNormality) && nrow(values$nonNormality) > 0) {
            nonNormalityIds <- values$nonNormality[[wid]][which(values$nonNormality$var == dv)]
          }
          
          dat <- values$initData[!values$initData[[wid]] %in% c(outlierIds, nonNormalityIds),]
          dat <- dat[!is.na(dat$diff),]
          ## .. 
          if (nrow(dat) > 0) return(cbind(var = dv, dat))
        }))
      }
      
      observeEvent(values$outliers, { updateData() })
      observeEvent(values$nonNormality, { updateData() })
      
      observeEvent(values$data, {
        if (nrow(values$data) > 0) {
          hypothesisPairedTTestMD("tTestSummaryUI", values$data, input$dvCondition1, input$dvCondition2, wid = input$wid)
          outliersMD("outlierAssumptionUI", values$initData, values$outliers, values$dvs, wid = input$wid)
          nonNormalityMD("normalityAssumptionUI", values$data, values$dvs, wid=input$wid)
          descriptiveStatisticsMD("descrStatisticsUI", values$data, c(input$dvCondition1,  input$dvCondition2, values$dvs), dv.var = NULL)
        }
      })
      
    }
  )
}
