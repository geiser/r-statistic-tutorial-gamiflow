wants <- c('plotly','dplyr','rstatix','readr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
options(scipen = 999)

library(plotly)
library(readr)
library(dplyr)
library(rstatix)

## Leitura de dados 

fss <- read_csv("data/fss.csv")
participants <- read_csv("data/participants.csv")
dat <- merge(participants, fss, by="UserID")

dat$cenario <- factor(dat$cenario)

## Efeituar estatistica descritiva

(frequency <- freq_table(dat, cenario))

(summary <- get_summary_stats(group_by(dat, cenario)))

write_csv(frequency, 'report/h1-experiencia-fluxo/frequency.csv')
write_csv(summary, 'report/h1-experiencia-fluxo/summary.csv')

## Visualização de dados

p <- plot_ly(data=dat, type = "box", boxpoints = "outliers"#, jitter = 0.3
             , x=as.formula("~cenario"), y=~media, color=~cenario, text=~UserID)
p <- layout(p, yaxis = list(title="média do fluxo"), showlegend = F)

print(p)

p <- p + geom_boxplot()


p <- ggplot(dat, aes_string("cenario", "media", colour = "cenario"))
p <- p + geom_boxplot(outlier.colour = "red", outlier.size = 1.5)
p <- p + geom_text(aes_string(label="UserID"), position = "dodge")




ggplotly(p)
print(p)

for (param in list(list("dimensao1", "balanço desafio/habilidade")
                   , list("dimensao2", "fusão ação e atenção")
                   , list("dimensao3", "objetivos claros")
                   , list("dimensao4", "feedback")
                   , list("dimensao5", "concentração")
                   , list("dimensao6", "controle")
                   , list("dimensao7", "perda autoconsciência")
                   , list("dimensao8", "transformação do tempo")
                   , list("dimensao9", "experiência autotélica")
                   , list("media", "média do fluxo"))) {
  p <- plot_ly(data=dat, type = "box", boxpoints = "all", jitter = 0.3
               , x=~cenario, y=as.formula(paste0("~", param[[1]])), color=~cenario, text=~UserID)
  p <- layout(p, yaxis = list(title=param[[2]]), showlegend = F)
  print(p)
  
  readline(prompt=paste0("Current graph: ",param[[1]]," >> Press [enter] to continue"))
}


## Escolha do teste estatístico
##################################

## Verificando premissas para efetuar t-test com as médias

## ... removendo outliers

(media_outliers <- identify_outliers(group_by(dat, cenario), media))

media_dat <- dat[!dat$UserID %in% media_outliers$UserID,]

## ... validando distribuição normal

shapiro_test(group_by(media_dat, cenario), media)

rownames(media_dat) <- media_dat$UserID
car::qqPlot(~media, subset(media_dat, cenario == "gamificado"))

media_dat <- media_dat[!media_dat$UserID %in% c("b7c1ac58","a9d87bce"),]
shapiro_test(group_by(media_dat, cenario), media)

source('common/identify_nonnormal.R')
(non_normal_ids <- identify_nonnormal(subset(media_dat, cenario == "gamificado"), "media", wid = "UserID"))

media_dat <- media_dat[!media_dat$UserID %in% c("b7c1ac58","a9d87bce", non_normal_ids),]
(shapiro <- shapiro_test(group_by(media_dat, cenario), media))

write_csv(media_dat, 'report/h1-experiencia-fluxo/media-data.csv')
write_csv(shapiro, 'report/h1-experiencia-fluxo/media-shapiro.csv')

## ... validando distribuição normal

(levene <- levene_test(media ~ cenario, data = media_dat))

write_csv(levene, 'report/h1-experiencia-fluxo/media-shapiro.csv')


## Efetuamos a execução do teste estatístico paramétrico t-test
#############################################################

## ... empregamos Student t-test

(tt <- t_test(media ~ cenario, data = media_dat))
(tt <- t_test(media ~ cenario, data = media_dat, alternative = "greater", detailed = T))

add_significance(tt)

(ez <- cohens_d(media ~ cenario, data = media_dat, var.equal = T))

(t_test <- add_significance(merge(tt, ez)))
write_csv(t_test, 'report/h1-experiencia-fluxo/media-ttest.csv')

## ... criamos novamente a gráfica do boxplot para a média do fluxo (porque foram removidos dados)
p <- plot_ly(data=media_dat, type = "box", boxpoints = "all", jitter = 0.3
             , x=~cenario, y=~media, color=~cenario, text=~UserID)
p <- layout(p, yaxis = list(title="média do fluxo"), showlegend = F)
print(p)

## ... criamos novamente a estatistica descritiva da média do fluxo
(media_summary <- get_summary_stats(media_dat, media))


## Análises não-paramêtrica

wilcox_test(media~cenario, data = dat, alternative = "greater")
wilcox_effsize(media~cenario, data = dat, alternative = "greater")



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


