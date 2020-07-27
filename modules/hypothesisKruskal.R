library(DT)
library(plotly)
library(rstatix)
library(emmeans)

source(paste0(getwd(),'/modules/df2Table.R'))
source(paste0(getwd(),'/common/non_parametric_plots.R'))

hypothesisKruskalUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    fixedRow(
      column(
        width = 12,
        fixedRow(
          column(width = 6, HTML("")),
          column(width = 3, radioButtons(ns("pwc.method"), "Método de comparação dos pares", inline=F, choices=list("Dunn's test"="dunn", "Wilcoxon's test"="wilcoxon"))),
          column(width = 3, radioButtons(ns("ci"), "Intervalo de confiança", inline=F, choices=list("Não" = "FALSE", "Sim" = "TRUE")))
        )
      )
    ), hr(),
    df2TableUI(ns("kruskalResultTable")), br(), hr(),
    checkboxInput(ns("isPairwiseComp"), "Efetuar comparações em pares (pairwise comparisons)", width = "100%"),
    uiOutput(ns("pairwiseCompParams")), br(),
    uiOutput(ns("pairwiseCompParamsTabset")), br(),
    checkboxInput(ns("isPairwisePlot"), "Apresentar gráficos das comparações", width = "100%"),
    uiOutput(ns("pairwisePlotParams")), br(), uiOutput(ns("pairwisePlotsUI"))
  )
}


hypothesisKruskalMD <- function(id, data, dvs, between = c(), within = c(), wid = 'UserID', dv.var = 'var') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      values <- reactiveValues(kruskal.test = NULL, pair.wise = NULL, kwm = list(), ezm = list(), pwc = list())
      
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
            column(width = 3, radioButtons(ns("addParam1"),  "Apresentação de pontos", inline = T,
                                           choices = list("todos" = "jitter", "média" = "mean", "não" = "none")))
          )
        }
      })
      
      # .. update  results 
      
      updateKruskalResult <- function() {
        ivs <- between
        ldvs <- as.list(dvs); names(ldvs) <- dvs
        livs <- as.list(ivs); names(livs) <- ivs
        
        if (length(dvs) > 0) {
          
          values$kwm <- lapply(ldvs, FUN = function(dv) {
            dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
            sformula <- as.formula(paste(paste0('`',dv,'`'), "~", paste0('`',between,'`')))
            kmod <- tryCatch(kruskal_test(dat, sformula), error = function(e) return(NULL))
            if (!is.null(kmod)) return(kmod)
          })
          
          values$ezm <- lapply(ldvs, FUN = function(dv) {
            dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
            sformula <- as.formula(paste(paste0('`',dv,'`'), "~", paste0('`',between,'`')))
            ezm <- tryCatch(kruskal_effsize(dat, sformula, ci = as.logical(input$ci))
                            , error = function(e) return(NULL))
            if (!is.null(ezm)) return(ezm)
          })
          
          values$kruskal.test <- do.call(rbind, lapply(ldvs, FUN = function(dv) {
            df <- as.data.frame(add_significance(values$kwm[[dv]]))
            ez <- as.data.frame(values$ezm[[dv]])
            cbind(var = dv, cbind(df, ez))
          }))
          
          values$pwc <- lapply(ldvs, FUN = function(dv) {
            dat <- as.data.frame(data[which(data[[dv.var]] == dv),])
            lapply(livs, FUN = function(iv) {
              if (input$pwc.method == "wilcoxon") {
                pmd <- tryCatch(wilcox_test(dat, as.formula(paste(paste0('`',dv,'`'), "~", paste0('`',iv,'`'))),
                                            detailed=T, p.adjust.method = input$pairwiseCompAdjustmethod)
                                , error = function(e) NULL)
              } else {
                pmd <- tryCatch(dunn_test(dat, as.formula(paste(paste0('`',dv,'`'), "~", paste0('`',iv,'`'))),
                                          detailed=T, p.adjust.method = input$pairwiseCompAdjustmethod)
                                , error = function(e) NULL)
              }
              if (!is.null(pmd)) return(pmd)
            })
          })
          
          values$pair.wise <- do.call(rbind, lapply(ldvs, FUN = function(dv) {
            lpws <- lapply(ivs, FUN = function(iv) {
              pwc <- values$pwc[[dv]][[iv]]
              pdat <- as.data.frame(add_significance(pwc))
              return(pdat)
            })
            cnames <- unique(do.call(c, lapply(lpws, FUN = colnames)))
            cbind(var = dv, do.call(rbind, lapply(lpws, FUN = function(lpw) {
              for (cname in  setdiff(cnames, colnames(lpw))) {
                lpw[[cname]] <- rep(NA, nrow(lpw))
              }
              return(lpw)
            })))
          }))
        }
      }
      
      observeEvent(input$pwc.method, { updateKruskalResult() })
      observeEvent(input$ci, { updateKruskalResult() })
      observeEvent(input$pairwiseCompAdjustmethod, { updateKruskalResult() })
      
      # .. displays results
      
      observeEvent(values$kruskal.test, {
        cnames <- c(dv.var, "n", "df", "statistic", "effsize", "magnitude", "p.signif")
        df2TableMD("kruskalResultTable", values$kruskal.test, cnames)
      })
      
      observeEvent(input$pwcTabsetPanel, {
        ivs <- between
        dv <- input$pwcTabsetPanel
        pair.wise <- subset(values$pair.wise, var == dv)
        if (nrow(pair.wise) > 0) {
          #cnames <- c(".y.", ivs, "group1", "group2", "df", "statistic",
          #            "estimate", "conf.low", "conf.high", "p", "p.adj", "p.adj.signif")
          #cnames <- c(cnames, setdiff(colnames(pair.wise), cnames))
          df2TableMD(paste0("pwc",dv,"ResultTable"), pair.wise)
        }
      })
      
      output$pairwiseCompParamsTabset <- renderUI({
        if (input$isPairwiseComp) {
          do.call(tabsetPanel, c(list(id=ns("pwcTabsetPanel"), type="pills"), lapply(dvs, function(dv) {
            tabPanel(dv, value = dv, verticalLayout(
              br(), strong(paste("Comparações de", dv, "em pares das medianas")),
              df2TableUI(ns(paste0("pwc", dv, "ResultTable"))), br(), hr()
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
              kruskalPlots(data, dv, ivs, kwm=values$kwm[[dv]], pwcs=values$pwc[[dv]],
                           addParam = input$addParam1, width = input$width, height = input$height)
            )
          })))
        }
      })
    
    }
  )
}
