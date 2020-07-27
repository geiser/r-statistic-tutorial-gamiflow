wants <- c('ggpubr', 'stats', 'DT', 'shiny', 'rcompanion', 'emmeans', 'rstatix', 'plotly', 'readr', 'careless', 'car', 'dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
if (packageVersion("shiny") < "1.5.0") {
  install.packages("remotes")
  remotes::install_github("rstudio/shiny")
}

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
      titlePanel("Loading data from CSV-file"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fileInput("fileCSV", "Choose CSV File", multiple = F,
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          # options
          radioButtons("sep", "Separator", c(Comma = ",", Semicolon = ";", Tab = "\t")),
          radioButtons("quote", "Quote", c("Double Quote" = '"', "Single Quote" = "'", None = "")),
          selectInput("fileEncoding", "Encoding do arquivo", choices = c("", encoding), selected = "", multiple = F),
          selectInput("encoding", "Encoding do texto", choices = c("unknown", encoding), selected = "unknown", multiple = F)
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
  
  values <- reactiveValues(initData = NULL)
  
  output$dataDT <- DT::renderDataTable({
    req(input$fileCSV)
    tryCatch({
      values$initData <- read.csv(input$fileCSV$datapath, sep = input$sep, quote = input$quote,
                                  fileEncoding = input$fileEncoding, encoding = input$encoding)
    }, error = function(e) stop(safeError(e)))
    return(df2DT(values$initData))
  })
  
  observeEvent(input$mainNavPage, {
    if (!is.null(values$initData)) {
      if (input$mainNavPage == "two-sample-t-test") {
        twoSampleTTestMD("twoSampleTTestUI", values$initData)
      } else if (input$mainNavPage == "paired-t-test") {
        pairedTTestMD("pairedTTestUI", values$initData)
      } else if (input$mainNavPage == "factorial-anova") {
        factorialAnovaMD("factorialAnovaUI", values$initData)
      } else if (input$mainNavPage == "two-sample-wilcoxon") {
        twoSampleWilcoxonMD("twoSampleWilcoxonUI", values$initData)
      } else if (input$mainNavPage == "kruskal-wallis") {
        kruskalWallisMD("kruskalWallisUI", values$initData)
      } else if (input$mainNavPage == "scheirer-ray-hare") {
        scheirerRayHareMD("scheirerRayHareUI", values$initData)
      }
    }
  })
  
}

### RUN APPLICATIONS ###
shinyApp(ui = ui, server = server)
