library(DT)
library(plotly)
library(rstatix)
library(emmeans)

source(paste0(getwd(),'/modules/df2Table.R'))
source(paste0(getwd(),'/common/anova_plots.R'))

hypothesisAnovaUI <- function(id, type = 'factorial') {
  ns <- NS(id)
  
  mchoices <- list("Anova II" = 2, "Anova III" = 3, "Anova I (balanced)" = 1)
  if (type == 'mixed') mchoices <- list("Anova III" = 3, "Anova II" = 2, "Anova I (balanced)" = 1)
  
  verticalLayout(
    fixedRow(
      column(
        width = 12,
        fixedRow(
          column(width = 6, HTML("")),
          column(width = 3, radioButtons(ns("aovtype"), "Método", inline=F, choices=mchoices)),
          column(width = 3, radioButtons(ns("effsize"), "Tipo de effect size", inline=F,
                                         choices=list("generalized eta squared"="ges", "partial eta squared"="pes")))
        )
      )
    ), hr(),
    df2TableUI(ns("anovaResultTable")), br(), hr(),
    checkboxInput(ns("isPairwiseComp"), "Efetuar comparações em pares (pairwise comparisons)", width = "100%"),
    uiOutput(ns("pairwiseCompParams")), br(), uiOutput(ns("pairwiseCompParamsTabset")), br(),
    checkboxInput(ns("isPairwisePlot"), "Apresentar gráficos das comparações", width = "100%"),
    uiOutput(ns("pairwisePlotParams")), br(), uiOutput(ns("pairwisePlotsUI"))
  )
}


hypothesisAnovaMD <- function(id, data, dvs, between = c(), within = c(), wid = 'UserID', dv.var = 'var') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      values <- reactiveValues(anova.test = NULL, pair.wise = NULL, aov = list(), pwc = list())
      
      output$pairwiseCompParams <- renderUI({
        if (input$isPairwiseComp) {
          choices <- list("Bonferroni" = "bonferroni", "Hommel" = "hommel", "Holm" = "holm", "Hochberg" = "hochberg")
          fixedRow(
            column(width = 6, radioButtons(ns("pairwiseCompAdjustmethod"), "Ajuste do p-value", choices = choices, inline = T))
          )
        }
      })
      
      output$pairwisePlotParams <- renderUI({
        if (input$isPairwisePlot) {
          fixedRow(
            column(width = 3, numericInput(ns("width"), "Plot width", value = 800, min = 100, step = 50)),
            column(width = 3, numericInput(ns("height"), "Plot height", value = 600, min = 100, step = 50)),
            column(
              width = 3, radioButtons(
                ns("addParam1"),  "Apresentação de pontos", inline = T,
                choices = list("todos" = "jitter", "média" = "mean", "não" = "none")
              )
            )
          )
        }
      })
      
      # .. update ANOVA results 
      
      updateAnovaResult <- function() {
        ivs <- between
        ldvs <- as.list(dvs); names(ldvs) <- dvs
        livs <- as.list(ivs); names(livs) <- ivs
        
        if (length(dvs) > 0) {
          
          values$aov <- lapply(ldvs, FUN = function(dv) {
            dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
            sformula <- as.formula(paste(dv, "~", paste0(between, collapse = "*")))
            aov <- tryCatch(anova_test(dat, sformula, type = input$aovtype, effect.size = input$effsize, detailed = T)
                            , error = function(e) return(NULL))
            if (!is.null(aov)) {
              return(aov)
            }
          })
          
          values$anova.test <- do.call(rbind, lapply(ldvs, FUN = function(dv) {
            df <- as.data.frame(get_anova_table(values$aov[[dv]]))
            cbind(var = dv, add_significance(df))
          }))
          
          values$pwc <- lapply(ldvs, FUN = function(dv) {
            dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
            lapply(livs, FUN = function(iv) {
              gdat <- group_by_at(as.data.frame(dat), vars(setdiff(names(livs), iv)))
              emm <- tryCatch(emmeans_test(gdat, as.formula(paste(dv, "~", iv)), detailed=T,
                                           p.adjust.method = input$pairwiseCompAdjustmethod),
                              error = function(e) NULL)
              if (!is.null(emm)) return(emm)
            })
          })
          
          values$pair.wise <- do.call(rbind, lapply(ldvs, FUN = function(dv) {
            do.call(rbind, lapply(names(livs), FUN = function(iv) {
              pwc <- values$pwc[[dv]][[iv]]
              pdat <- as.data.frame(add_significance(pwc))
              if (nrow(pdat) > 0) {
                pdat[[iv]] <- NA 
                return(cbind(var = dv, pdat))
              }
            }))
          }))
        }
      }
      
      observeEvent(input$aovtype, { updateAnovaResult() })
      observeEvent(input$effsize, { updateAnovaResult() })
      
      # .. displays results
      
      observeEvent(values$anova.test, {
        cnames <- c(dv.var, "Effect", "SSn", "SSd", "F", "p", "ges", "p.signif")
        df2TableMD("anovaResultTable", values$anova.test, cnames)
      })
      
      observeEvent(input$pwcTabsetPanel, {
        ivs <- between
        dv <- input$pwcTabsetPanel
        pair.wise <- subset(values$pair.wise, var == dv)
        if (nrow(pair.wise) > 0) {
          cnames <- c(".y.", ivs, "group1", "group2", "df", "statistic",
                      "estimate", "conf.low", "conf.high", "p", "p.adj", "p.adj.signif")
          cnames <- c(cnames, setdiff(colnames(pair.wise), cnames))
          df2TableMD(paste0("pwc",dv,"ResultTable"), pair.wise, cnames)
        }
      })
      
      output$pairwiseCompParamsTabset <- renderUI({
        if (input$isPairwiseComp) {
          do.call(tabsetPanel, c(list(id=ns("pwcTabsetPanel"), type="pills"), lapply(dvs, function(dv) {
            tabPanel(dv, value = dv, verticalLayout(
              br(), strong(paste("Comparações de", dv, "em pares usando médias marginais estimadas")),
              df2TableUI(ns(paste0("pwc", dv, "ResultTable"))),
              div(paste("df: degree of freedom; estimate: estimate of the effect size;",
                        "ci: confidence intervale of the estimate")),
              br(), hr()
            ))
          })))
        }
      })
      
      output$pairwisePlotsUI <- renderUI({
        if (input$isPairwisePlot) {
          ivs <- between
          do.call(tabsetPanel, c(list(id=ns("pairwisePlotTabsetPanel"), type="pills"), lapply(dvs, function(dv) {
            tabPanel(
              dv, value = dv,
              if (length(ivs) == 3) {
                threeWayAnovaPlots(data, dv, ivs, aov=values$aov[[dv]], pwcs=values$pwc[[dv]],
                                   addParam = input$addParam1, width = input$width, height = input$height)
              } else if (length(ivs) == 2) {
                twoWayAnovaPlots(data, dv, ivs, aov=values$aov[[dv]], pwcs=values$pwc[[dv]],
                                 addParam = input$addParam1, width = input$width, height = input$height)
              } else if (length(ivs) == 1) {
                oneWayAnovaPlots(data, dv, ivs, aov=values$aov[[dv]], pwcs=values$pwc[[dv]],
                                 addParam = input$addParam1, width = input$width, height = input$height)
              }
            )
          })))
        }
      })
    
    }
  )
}

