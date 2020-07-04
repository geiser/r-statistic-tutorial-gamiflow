library(DT)
library(dplyr)
library(plotly)
library(rstatix)

source(paste0(getwd(),'/modules/df2Table.R'))


homogeneityTestText <- "
<p>O teste de Levene é utilizado para avaliar a igualdade de variâncias em dois ou mais grupos.
Alguns procedimentos estatísticos (Students' t-test e ANOVA tipo I) parâmetricos precissam de igualdade de variações das amostras.
Assim o Levene's test, avalia a homogeidade das varianças empregando:
</p>
<ul>
<li> Hipótese nula
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{null}\" title=\"H_{null}\" />):
As variações das amostras dos diferentes grupos são iguais.
</li>
<li> Hipótese alternativa
(<img src=\"https://latex.codecogs.com/png.latex?\\dpi{100}&space;H_{alt}\" title=\"H_{alt}\" />):
As variações das amostras dos diferentes grupos são diferentes.
</li>
</ul>
"

homogeneityUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    br(), strong("Levene's Test"),
    HTML(homogeneityTestText),
    br(), df2TableUI(ns("leveneTestTable"))
  )
}

homogeneityMD <- function(id, data, dvs, group, dv.var = 'var') {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      levene.test <- do.call(rbind, lapply(dvs, FUN = function(dv) {
        dat <- as.data.frame(data[which(data[[dv.var]] == dv), unique(c(dvs, group))])
        sformula <- paste(dv, "~", paste0(group, collapse = "*"))
        cbind(var = dv, levene_test(dat, as.formula(sformula)))
      }))
      
      df2TableMD("leveneTestTable", add_significance(levene.test))
    }
  )
}
