library(dplyr)

source(paste0(getwd(),'/modules/df2Table.R'))
source(paste0(getwd(),'/modules/outliers.R'))
source(paste0(getwd(),'/modules/nonNormality.R'))
source(paste0(getwd(),'/modules/homogeneity.R'))
source(paste0(getwd(),'/modules/hypothesisTTest.R'))
source(paste0(getwd(),'/modules/descriptiveStatistics.R'))

twoSampleTTestUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Two sample t-test"),
    sidebarLayout(
      sidebarPanel(width = 3, uiOutput(ns("tTestParamsUI"))),
      mainPanel(width = 9, uiOutput(ns("tTestTabsetUI")))
    )
  )
}


twoSampleTTestMD <- function(id, initData) {
  
  get_choices <- function(df, type = "id", wid = c()) {
    ids <- colnames(df)[sapply(colnames(df), function (x) { anyDuplicated(df[[x]]) == 0 })]
    two_between <- colnames(df)[sapply(colnames(df), function (x) { length(unique(df[[x]])) == 2 })]
    
    between <- setdiff(colnames(df), c(wid, "row.pos"))
    
    dvs <- setdiff(colnames(df), c(wid, "row.pos"))
    dvs <- dvs[sapply(dvs, FUN = function(x) is.numeric(df[[x]]))]
    
    if (type == "id") return(ids)
    else if (type == "between") return(between)
    else if (type == "two-between") return(two_between)
    else if (type == "dv") return(dvs)
  }
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      values <- reactiveValues(initData = cbind(row.pos = seq(1, nrow(initData)), initData),
                               outliers = NULL, nonNormality = NULL, data = NULL, isSetupVars = F)
      
      output$tTestTabsetUI <- renderUI({
        if (values$isSetupVars) {
          tabsetPanel(
            id = ns("tTestTabset"), type = "tabs",
            tabPanel("Teste de hipóteses", value = "hypothesisTest", hypothesisTTestUI(ns("tTestSummaryUI"))),
            tabPanel("Premissa: Outliers", value = "outlierAssumption", outliersUI(ns("outlierAssumptionUI"))),
            tabPanel("Premissa: Distr. Normal", value = "normalityAssumption", nonNormalityUI(ns("normalityAssumptionUI"))),
            tabPanel("Premissa: Homog. Variança", value = "homogeneityAssumption", homogeneityUI(ns("homogeneityAssumptionUI"))),
            tabPanel("Estatística descritiva", value = "descrStatistics", descriptiveStatisticsUI(ns("descrStatisticsUI")))
          )
        }
      }) 
      
      output$tTestParamsUI <- renderUI({
        choices <- c(get_choices(initData, "id"), "row.pos")
        verticalLayout(
          fluid = T,
          selectInput(ns('wid'), 'Identificador da obs', choices = choices, multiple = F),
          uiOutput(ns('betweenUI')), uiOutput(ns('dvsUI')), uiOutput(ns('assumptionParamsUI')),
          actionButton(ns('executeButton'), "Executar teste")
        )
      })
      
      output$betweenUI <- renderUI({
        choices <- get_choices(initData, "two-between", input$wid)
        selectInput(ns('between'), 'Variável independente (between-subject)', choices = choices, multiple = F)
      })
      
      observeEvent(input$between, {
        if (length(input$between) > 0 && length(input$dvs) > 0) {
          shinyjs::enable("executeButton")
        } else {
          shinyjs::disable("executeButton")
        }
      })
      
      output$dvsUI <- renderUI({
        choices <- setdiff(get_choices(initData, "dv", c(input$wid, input$between)), input$between)
        selectInput(ns('dvs'), 'Variáveis dependentes', choices = choices, multiple = T)
      })
      
      observeEvent(input$dvs, {
        if (length(input$between) > 0 && length(input$dvs) > 0) {
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
        values$outliers <- do.call(rbind, lapply(input$dvs, FUN = function(dv) {
          outlierIds <- input[[paste0('outliers', dv, 'Input')]]
          dat <- values$initData[values$initData[[wid]] %in% outlierIds,]
          if (length(outlierIds) > 0 && nrow(dat) > 0) return(cbind(var = dv, dat))
        }))
      }, suspended = T)
      
      observeEvent(input$identifyingOutliers, {
        wid <- input$wid
        outliers <- getOutliers(values$initData, input$dvs, input$between)
        for (dv in input$dvs) {
          selected <- outliers[[wid]][which(outliers$var == dv)]
          updateSelectInput(session, paste0('outliers', dv, 'Input'), selected=selected)
        }
      })
      
      output$outliersInputUI <- renderUI({
        wid <- input$wid
        outliers <- getOutliers(values$initData, input$dvs, input$between)
        outliersInputs <- lapply(input$dvs, FUN = function(dv) {
          choices <- values$initData[[wid]]
          selected <- outliers[[wid]][which(outliers$var == dv)]
          selectInput(ns(paste0('outliers', dv, 'Input')), dv, choices=choices, selected=selected, multiple=T)
        })
        do.call(verticalLayout, outliersInputs)
      })
      
      ## .. dealing with non-normality 
      
      nonNormalityObserve <- observe({
        wid <- input$wid
        values$nonNormality <- do.call(rbind, lapply(input$dvs, FUN = function(dv) {
          nonNormalityIds <- input[[paste0('nonNormality',dv,'Input')]]
          dat <- values$initData[values$initData[[wid]] %in% nonNormalityIds,]
          if (length(nonNormalityIds) > 0 && nrow(dat) > 0) return(cbind(var = dv, dat))
        }))
      }, suspended = T)
      
      output$nonNormalInputUI <- renderUI({
        wid <- input$wid
        nonNormalityInputs <- lapply(input$dvs, FUN = function(dv) {
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
          shinyjs::enable("between")
          shinyjs::enable("dvs")
          updateActionButton(session, "executeButton", label = "Voltar a executar teste")
          
          values$initData = cbind(row.pos = seq(1, nrow(initData)), initData)
          values$isSetupVars <- FALSE
        } else { #executar teste
          updateActionButton(session, "executeButton", label = "Parar teste e mudar variáveis")
          
          shinyjs::disable("wid")
          shinyjs::disable("between")
          shinyjs::disable("dvs")
          
          values$isSetupVars <- TRUE
          
          updateData()
          outliersObserve$resume()
          nonNormalityObserve$resume()
        }
      })
      
      updateData <- function() {
        wid <- input$wid
        values$data <- do.call(rbind, lapply(input$dvs, FUN = function(dv) {
          
          outlierIds <- c()
          if (!is.null(values$outliers) && nrow(values$outliers) > 0) {
            outlierIds <- values$outliers[[wid]][which(values$outliers$var == dv)]
          }
          
          nonNormalityIds <- c()
          if (!is.null(values$nonNormality) && nrow(values$nonNormality) > 0) {
            nonNormalityIds <- values$nonNormality[[wid]][which(values$nonNormality$var == dv)]
          }
          
          dat <- values$initData[!values$initData[[wid]] %in% c(outlierIds, nonNormalityIds),]
          dat <- df2qqs(dat, input$between)
          
          ## .. 
          if (nrow(dat) > 0) return(cbind(var = dv, dat))
        }))
      }
      
      observeEvent(values$outliers, { updateData() })
      observeEvent(values$nonNormality, { updateData() })
      
      observeEvent(values$data, {
        if (nrow(values$data) > 0) {
          hypothesisTTestMD("tTestSummaryUI", values$data, input$dvs, input$between)
          outliersMD("outlierAssumptionUI", values$initData, values$outliers, input$dvs, input$between, input$wid)
          nonNormalityMD("normalityAssumptionUI", values$data, input$dvs, input$between, wid=input$wid)
          homogeneityMD("homogeneityAssumptionUI", values$data, input$dvs, input$between)
          descriptiveStatisticsMD("descrStatisticsUI", values$data, input$dvs, input$between)
        }
      })
      
    }
  )
}
