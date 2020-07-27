library(DT)

df2DT <- function(df, pageLength = -1) {
  datatable(
    df, escape = F, rownames = F, extensions = c("Buttons"),
    options = list(pageLength = pageLength, dom = 'Bfrtip', filter = 'top',
                   buttons = list('pageLength','csv','pdf','copy','print'),
                   lengthMenu = list(c(25, 50, 100, -1),
                                     c('25 rows', '50 rows', '100 rows','Show all')),
                   columnDefs = list(list(targets = 0:(length(names(df))-1)))),
    class = 'cell-border compact stripe')
}


df2TableUI <- function(id, digits = 4) {
  ns <- NS(id)
  verticalLayout(fixedRow(
    column(width = 12, fixedRow(
      column(width = 10, uiOutput(ns('colnamesInput'))),
      column(width = 2, numericInput(ns("digits"), 'Decimais:', value = digits, step = 1))
    ))),
    DTOutput(ns("dataframeDT"))
  ) 
}

df2TableMD <- function(id, df, columns = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$colnamesInput <- renderUI({
        choices <- colnames(df)
        if (is.null(columns)) columns <- choices
        selectInput(ns('colnames'), 'Colunas', choices = choices, selected = columns, multiple = T, width = '100%')
      }) 
      
      output$dataframeDT <- renderDataTable({
        if (input$digits >= 0) {
          for (cname in colnames(df)[sapply(colnames(df), function(x) is.numeric(df[[x]]))]) {
            df[[cname]] <- round(df[[cname]], digits = input$digits)
          }
        }
        df2DT(df[, input$colnames])
      })
    }
  )
}
