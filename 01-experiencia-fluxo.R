wants <- c('plotly','processx','dplyr','rstatix','readr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
options(scipen = 999)

library(readr)
library(dplyr)
library(rstatix)

## Leitura de dados 

fss <- read_csv('data/fss.csv')
dat <- select(fss, starts_with("UserID"), starts_with("cenario"), starts_with("dimensao"), starts_with("media"))
dat$cenario = factor(dat$cenario)

## Efeituar estatistica descritiva

(fss_summary <- get_summary_stats(dat))
(fss_frequency <- freq_table(dat, cenario))

write_csv(fss_summary, 'report/fss-summary.csv')
write_csv(fss_frequency, 'report/fss-frequency.csv')

## Verificando premissas para efetuar metodo parametricos ou não-paramêtricos

shapiro <- shapiro_test(dat, media, dimensao1, dimensao2, dimensao3
                         , dimensao4, dimensao5, dimensao6, dimensao7, dimensao8, dimensao9)
shapiro[["dist.normal"]] <- if_else(shapiro$p > 0.05, 'sim', 'não')
print(shapiro)

write_csv(shapiro, "report/h1-experiencia-fluxo/shapiro-test.csv")

identify_outliers(dat, media) # identificação de outliers na media do fluxo - método boxplot

## Análises paramêtrica - apenas para dados com distribução normal

t_test(dimensao1~cenario, data = dat)
t_test(dimensao1~cenario, data = dat, alternative = "less")
t_test(dimensao1~cenario, data = dat, alternative = "greater")
cohens_d(dimensao1~cenario, data = dat)

param_test <- do.call(
  rbind
  , lapply(
    list(list("dimensao1", "balanço desafio/habilidade")
         , list("dimensao5", "concentração"))
    , FUN = function(param) {
      formula <- as.formula(paste0(param[[1]],'~cenario'))
      return(
        cbind(
          "fator latente" = param[[2]]
          , merge(t_test(formula, data = dat, alternative="greater")
                  , cohens_d(formula, data = dat))
        )
      )
    }
  )
)
param_test

write_csv(param_test, "report/h1-experiencia-fluxo/parametric-test.csv")

## Análises não-paramêtrica

wilcox_test(media~cenario, data = dat, alternative = "greater")
wilcox_effsize(media~cenario, data = dat, alternative = "greater")

param_list <- list(list("dimensao1", "balanço desafio/habilidade")
                   , list("dimensao2", "fusão ação e atenção")
                   , list("dimensao3", "objetivos claros")
                   , list("dimensao4", "feedback")
                   , list("dimensao5", "concentração")
                   , list("dimensao6", "controle")
                   , list("dimensao7", "perda autoconsciência")
                   , list("dimensao8", "transformação do tempo")
                   , list("dimensao9", "experiência autotélica")
                   , list("media", "média do fluxo"))

non_param_test <- do.call(rbind, lapply(param_list, FUN = function(param) {
  formula <- as.formula(paste0(param[[1]],'~cenario'))
  cbind("fator latente" = param[[2]]
        , merge(wilcox_test(formula, data = dat, alternative="greater")
                , wilcox_effsize(formula, data = dat)))
}))
non_param_test

write_csv(non_param_test, "report/h1-experiencia-fluxo/non-parametric-test.csv")

## Efetuar plot
library(plotly)

p <- plot_ly(data=dat, type = "box", boxpoints = "all", jitter = 0.3
             , x=~cenario, y=~media, color=~cenario, text=~UserID)
p <- layout(p, yaxis = list(title="média do fluxo"), showlegend = F)
print(p)

for (param in param_list) {
  p <- plot_ly(data=dat, type = "box", boxpoints = "all", jitter = 0.3
               , x=~cenario, y=as.formula(paste0("~", param[[1]])), color=~cenario, text=~UserID)
  p <- layout(p, yaxis = list(title=param[[2]]), showlegend = F)
  print(p)
  
  readline(prompt="Press [enter] to continue")
}

