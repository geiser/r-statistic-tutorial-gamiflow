wants <- c('readr', 'careless', 'car','dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(utils)
library(readr)      # biblioteca para leitura de arquivos CVS
library(careless)   # biblioteca para tratamento de respostas descuidadas
library(car)        # biblioteca para graficar Boxplots com identificação de pontos
library(dplyr)      # biblioteca para manipular data.frames

## Leitura do questionario DFS usando arquivo CSV
fss_dat <- read.csv("data/survey-fss.csv")
resp_fss_dat <- select(fss_dat, starts_with("Q"))

## Tratamento de respostas descuidadas (careless response)
(careless_fss_long <- longstring(resp_fss_dat)) 
Boxplot(careless_fss_long, main = "Boxplot do Longstring para o FSS") # 49,3,38 respostas a ser eliminada

careless_fss_irv <- irv(resp_fss_dat)
careless_fss_irv_idx <- order(careless_fss_irv)

careless_irv_df <- cbind(resp = careless_fss_irv_idx
                         , IRV=careless_fss_irv[careless_fss_irv_idx]
                         , resp_fss_dat[careless_fss_irv_idx,])
head(careless_irv_df) # 49, 3, 27 e 13 < 0.5

## Salvar respostas descuidadas no arquivo <../data/fss-careless.csv> e
## Salvar respostas do DFS sem respostas descuidadas no arquivo <../data/fss.csv>

write.csv(fss_dat[c(49,3,27,13),], 'data/fss-careless.csv')
write.csv(fss_dat[-c(49,3,27,13),], 'data/fss.csv')

