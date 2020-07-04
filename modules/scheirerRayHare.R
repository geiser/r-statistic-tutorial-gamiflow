library(dplyr)

source(paste0(getwd(),'/common/df2qqs.R'))

source(paste0(getwd(),'/modules/df2Table.R'))
source(paste0(getwd(),'/modules/hypothesisScheirer.R'))
source(paste0(getwd(),'/modules/descriptiveStatistics.R'))

scheirerRayHareUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Scheirer–Ray–Hare test como alternativa a two-way ANOVA (dois a mais grupos em duas variáveis)"),
    sidebarLayout(
      sidebarPanel(width = 3, uiOutput(ns("scheirerParamsUI"))),
      mainPanel(width = 9, uiOutput(ns("scheirerTabsetUI")))
    )
  )
}


scheirerRayHareMD <- function(id, initData) {
  
  get_choices <- function(df, type = "id") {
    ids <- colnames(df)[sapply(colnames(df), function (x) { anyDuplicated(df[[x]]) == 0 })]
    between <- colnames(df)[sapply(colnames(df), function (x) { anyDuplicated(df[[x]]) != 0 })]
    between <- setdiff(between, ids)
    
    dvs <- setdiff(colnames(df), c(ids, "row.pos"))
    dvs <- dvs[sapply(dvs, FUN = function(x) is.numeric(df[[x]]))]
    
    if (type == "id") return(ids)
    else if (type == "between") return(between)
    else if (type == "dv") return(dvs)
  }
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      values <- reactiveValues(initData = cbind(row.pos = seq(1, nrow(initData)), initData), data = NULL, isSetupVars = F)
      
      output$scheirerTabsetUI <- renderUI({
        if (values$isSetupVars) {
          tabsetPanel(
            id = ns("kruskalTabset"), type = "tabs",
            tabPanel("Teste de hipóteses", value = "hypothesisTest", hypothesisScheirerUI(ns("scheirerSummaryUI"))),
            tabPanel("Estatística descritiva", value = "descrStatistics", descriptiveStatisticsUI(ns("descrStatisticsUI")))
          )
        }
      }) 
      
      output$scheirerParamsUI <- renderUI({
        choices <- c(get_choices(initData, "id"), "row.pos")
        verticalLayout(
          fluid = T,
          selectInput(ns('wid'), 'Identificador da obs', choices = choices, multiple = F),
          uiOutput(ns('betweenUI')), uiOutput(ns('dvsUI')),
          actionButton(ns('executeButton'), "Executar teste")
        )
      })
      
      output$betweenUI <- renderUI({
        choices <- get_choices(initData, "between")
        selectInput(ns('between'), 'Variável independente (between-subject)', choices = choices, multiple = T)
      })
      
      observeEvent(input$between, {
        if (length(input$between) > 2) {
          updateSelectInput(session, "between", selected = input$between[1:2])
        }
        if (length(input$between) > 0 && length(input$dvs) > 0) {
          shinyjs::enable("executeButton")
        } else {
          shinyjs::disable("executeButton")
        }
      })
      
      output$dvsUI <- renderUI({
        choices <- setdiff(get_choices(initData, "dv"), input$between)
        selectInput(ns('dvs'), 'Variáveis dependentes', choices = choices, multiple = T)
      })
      
      observeEvent(input$dvs, {
        if (length(input$between) > 0 && length(input$dvs) > 0) {
          shinyjs::enable("executeButton")
        } else {
          shinyjs::disable("executeButton")
        }
      })
      
      ## .. update data
      
      observeEvent(input$executeButton, {
        if (values$isSetupVars) {
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
        }
      })
      
      updateData <- function() {
        wid <- input$wid
        values$data <- do.call(rbind, lapply(input$dvs, FUN = function(dv) {
          dat <- df2qqs(values$initData, input$between)
          if (nrow(dat) > 0) return(cbind(var = dv, dat))
        }))
      }
      
      observeEvent(values$data, {
        if (nrow(values$data) > 0) {
          hypothesisScheirerMD("scheirerSummaryUI", values$data, input$dvs, between = input$between, wid = input$wid)
          descriptiveStatisticsMD("descrStatisticsUI", values$data, input$dvs, input$between)
        }
      })
      
    }
  )
}
