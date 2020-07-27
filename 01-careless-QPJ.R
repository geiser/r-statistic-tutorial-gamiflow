wants <- c('readr', 'careless', 'car','dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(utils)
library(readr)      # biblioteca para leitura de arquivos CVS
library(careless)   # biblioteca para tratamento de respostas descuidadas
library(car)        # biblioteca para graficar Boxplots com identificação de pontos
library(dplyr)      # biblioteca para manipular data.frames

## Leitura do questionario DFS usando arquivo CSV

qpj_dat <- read.csv("data/survey-qpj.csv")
resp_qpj_dat <- select(qpj_dat, starts_with("Item")) # selecionar apenas respostas das questões "Item"

## Tratamento de respostas descuidadas (careless response)

(careless_qpj_long <- longstring(resp_qpj_dat)) 
Boxplot(careless_qpj_long, main = "Boxplot do Longstring para o QPJ") # nemhum > 12 => não há careless

careless_qpj_irv <- irv(resp_qpj_dat)
careless_qpj_irv_idx <- order(careless_qpj_irv)

careless_irv_df <- cbind(resp = careless_qpj_irv_idx
                         , IRV=careless_qpj_irv[careless_qpj_irv_idx]
                         , resp_qpj_dat[careless_qpj_irv_idx,])
head(careless_irv_df) # careless 40

## Salvar respostas descuidadas no arquivo <../data/qpj-careless.csv> e
## Salvar respostas do DFS sem respostas descuidadas no arquivo <../data/qpj.csv>

write.csv(qpj_dat[c(40),], 'data/qpj-careless.csv')
write.csv(qpj_dat[-c(40),], 'data/qpj.csv')

