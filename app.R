wants <- c('ggpubr', 'readxl', 'stats', 'DT', 'shiny', 'rcompanion', 'emmeans', 'rstatix', 'plotly', 'readr', 'careless', 'car', 'dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
if (packageVersion("shiny") < "1.5.0") {
  install.packages("remotes")
  remotes::install_github("rstudio/shiny")
}

library(readxl)
library(readr)
library(shiny)
library(plotly)
library(rstatix)

options(scipen = 999)
options(stringsAsFactors=F)

source('modules/twoSampleTTest.R')
source('modules/pairedTTest.R')
source('modules/df2Table.R')
source('modules/factorialAnova.R')
source('modules/twoSampleWilcoxon.R')
source('modules/kruskalWallis.R')
source('modules/scheirerRayHare.R')

encoding <- c("UTF-8", "latin1","WINDOWS-1252","ASCII","BIG5","GB18030","GB2312",
              "ISO-2022-JP","ISO-2022-KR","ISO-8859-1","ISO-8859-2","ISO-8859-7","SHIFT-JIS")

ui <- navbarPage(
  "Statistic", id = "mainNavPage",
  tabPanel(
    "Load Data", value = "load",
    fluidPage(
      titlePanel("Loading data from file"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fileInput("file", "Choose CSV or Excel File", multiple = F,
                    accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain",
                               ".xlsx", "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
          uiOutput("fileOptions")
        ),
        mainPanel(width = 9, DT::DTOutput("dataDT"))
      )
    )
  ),
  navbarMenu(
    "T-test",
    tabPanel("Two-sample T-test", value="two-sample-t-test", twoSampleTTestUI("twoSampleTTestUI")),
    tabPanel("Paired T-test", value="paired-t-test", pairedTTestUI("pairedTTestUI"))
  ),
  navbarMenu(
    "ANOVA",
    tabPanel("factorial ANOVA", value="factorial-anova",
             factorialAnovaUI("factorialAnovaUI", "Factorial ANOVA  (comparing independent groups)"))
  ),
  navbarMenu(
    "não paramétricos",
    tabPanel("Two-sample Wilcoxon test (alernativa t-test)", value="two-sample-wilcoxon", twoSampleWilcoxonUI("twoSampleWilcoxonUI")),
    tabPanel("Kruskal-Wallis Test (alternativa one-way ANOVA)", value="kruskal-wallis", kruskalWallisUI("kruskalWallisUI")),
    tabPanel("Scheirer–Ray–Hare Test (alternativa a two-way ANOVA)", value="scheirer-ray-hare", scheirerRayHareUI("scheirerRayHareUI"))
  )
)

### SERVER LOGIC ###

server <- function(input, output, session) {
  
  values <- reactiveValues(initData = NULL, data = NULL)
  
  output$fileOptions <- renderUI({
    req(input$file)
    formatFile <- format_from_signature(input$file$datapath)
    if (!is.na(formatFile) && formatFile == "xlsx") {
      choices = readxl::excel_sheets(input$file$datapath)
      selectInput("sheet", "Folha", choices = choices, multiple = F)
    } else {
      selected = guess_encoding(input$file$datapath)$encoding[1]
      verticalLayout(
        radioButtons("sep", "Separator", c(Comma = ",", Semicolon = ";", Tab = "\t")),
        radioButtons("quote", "Quote", c("Double Quote" = '"', "Single Quote" = "'", None = "")),
        selectInput("fileEncoding", "Encoding do arquivo", choices = c("", encoding), selected = selected, multiple = F),
        selectInput("encoding", "Encoding do texto", choices = c("unknown", encoding), selected = "unknown", multiple = F)
      )
    }
  })
  
  output$dataDT <- DT::renderDataTable({
    req(input$file)
    tryCatch({
      formatFile <- format_from_signature(input$file$datapath)
      if (!is.na(formatFile) && formatFile == "xlsx") {
        values$initData <- read_excel(input$file$datapath, sheet = input$sheet)
      } else {
        values$initData <- read.csv(input$file$datapath, sep = input$sep, quote = input$quote,
                                    fileEncoding = input$fileEncoding, encoding = input$encoding)  
      }
      values$data <- values$initData
    }, error = function(e) stop(safeError(e)))
    df2DT(values$initData, editable = T)
  })
  
  observeEvent(input$dataDT_cell_edit, {
    row <- input$dataDT_cell_edit[['row']]
    col <- input$dataDT_cell_edit[['col']]
    values$data[row, col] <- input$dataDT_cell_edit[['value']]
  })
  
  observeEvent(input$mainNavPage, {
    if (!is.null(values$data)) {
      if (input$mainNavPage == "two-sample-t-test") {
        twoSampleTTestMD("twoSampleTTestUI", values$data)
      } else if (input$mainNavPage == "paired-t-test") {
        pairedTTestMD("pairedTTestUI", values$data)
      } else if (input$mainNavPage == "factorial-anova") {
        factorialAnovaMD("factorialAnovaUI", values$data)
      } else if (input$mainNavPage == "two-sample-wilcoxon") {
        twoSampleWilcoxonMD("twoSampleWilcoxonUI", values$data)
      } else if (input$mainNavPage == "kruskal-wallis") {
        kruskalWallisMD("kruskalWallisUI", values$data)
      } else if (input$mainNavPage == "scheirer-ray-hare") {
        scheirerRayHareMD("scheirerRayHareUI", values$data)
      }
    }
  })
  
}

### RUN APPLICATIONS ###
shinyApp(ui = ui, server = server)
