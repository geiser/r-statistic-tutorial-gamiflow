if (!'remotes' %in% rownames(installed.packages())) install.packages('remotes')
if (!"rshinystatistics" %in% rownames(installed.packages())) {
  remotes::install_github("geiser/rshinystatistics")
} else if (packageVersion("rshinystatistics") < "0.0.0.9029") {
  remotes::install_github("geiser/rshinystatistics")
}

wants <- c('ggplot2','ggpubr','rshinystatistics','car','emmeans','careless','stats','rstatix','utils','dplyr','rmarkdown', 'shiny')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(utils)
library(ggplot2)
library(ggpubr)
library(careless)
library(ggplot2)
library(rstatix)
library(emmeans)

library(shiny)
library(rshinystatistics)

## ... Loading Application

ui <- navbarPage(
  "R Shiny-Statistics", id = "mainNavPage", selected = 'none',
  tabPanel(
    "Home", value = "none",
    br(), 
    tags$ul(
      tags$li(a("https://github.com/geiser/r-statistic-tutorial-gamiflow/wiki",
                href="https://github.com/geiser/r-statistic-tutorial-gamiflow/wiki",
                target="_blank"))
    )
    #HTML(paste0('<iframe src="https://github.com/geiser/r-statistic-tutorial-gamiflow/wiki" width="100%" height="1000px"></iframe>'))
  ),
  navbarMenu(
    "T-test"
    , tabPanel("Independent Two-Sample T-test", value="ttest", indSampleTTestUI("ttest"))
    , tabPanel("Paired Two-Sample T-test", value="paired-ttest", pairedTTestUI("paired-ttest"))
  ),
  navbarMenu(
    "ANCOVA / ANOVA"
    , tabPanel("ANCOVA", value="ancova", ancovaUI("ancova"))
    , tabPanel("Factorial ANOVA", value="anova", factorialAnovaUI("anova"))
    , tabPanel("Repeated Measures ANOVA", value="rep-anova", repMeasuresAnovaUI("rep-anova"))
  ),
  navbarMenu(
    "não paramétricos"
    , tabPanel("Two-Sample Wilcoxon Test (alernative to t-test)", value="wilcoxon", wilcoxonTestUI("wilcoxon"))
    , tabPanel("Kruskal-Wallis Test (alternative to one-way ANOVA)", value="kruskal", kruskalWallisUI("kruskal"))
    , tabPanel("Scheirer–Ray–Hare Test (alternative to two-way and three-way ANOVA)", value="scheirer", scheirerRayHareUI("scheirer"))
  ),
  navbarMenu(
    "utilities"
    , tabPanel("Identifying and removing careless responses", value="careless", removeCarelessUI("careless"))
  )
)

### SERVER LOGIC ###

server <- function(input, output, session) {
  
  observeEvent(input$mainNavPage, {
    if (input$mainNavPage == "ttest") {
      indSampleTTestMD("ttest")
    } else if (input$mainNavPage == "paired-ttest") {
      pairedTTestMD("paired-ttest")
    } else if (input$mainNavPage == "anova") {
      factorialAnovaMD("anova")
    } else if (input$mainNavPage == "rep-anova") {
      repMeasuresAnovaMD("rep-anova")
    } else if (input$mainNavPage == "ancova") {
      ancovaMD("ancova")
    } else if (input$mainNavPage == "wilcoxon") {
      wilcoxonTestMD("wilcoxon")
    }  else if (input$mainNavPage == "kruskal") {
      kruskalWallisMD("kruskal")
    } else if (input$mainNavPage == "scheirer") {
      scheirerRayHareMD("scheirer")
    } else if (input$mainNavPage == "careless") {
      removeCarelessMD("careless")
    }
  })
  
}

### RUN APPLICATIONS ###
shinyApp(ui = ui, server = server)
