wants <- c('readr', 'careless', 'car','dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(utils)
library(readr)      # biblioteca para leitura de arquivos CVS
library(careless)   # biblioteca para tratamento de respostas descuidadas
library(car)        # biblioteca para graficar Boxplots com identificação de pontos
library(dplyr)      # biblioteca para manipular data.frames

## Leitura do questionario DFS usando arquivo CSV

dfs_dat <- read.csv("data/survey-dfs.csv")
resp_dfs_dat <- select(dfs_dat, starts_with("Q")) # selecionar apenas respostas das questões "Q"

## Tratamento de respostas descuidadas (careless response)

# .. metodo longstring
(careless_dfs_long <- longstring(resp_dfs_dat)) 
Boxplot(careless_dfs_long, main = "Boxplot do Longstring para o DFS") # careless 2,54

# .. metodo irv
careless_dfs_irv <- irv(resp_dfs_dat)
careless_dfs_irv_idx <- order(careless_dfs_irv)

careless_irv_df <- cbind(resp = careless_dfs_irv_idx
                         , IRV=careless_dfs_irv[careless_dfs_irv_idx]
                         , resp_dfs_dat[careless_dfs_irv_idx,])
head(careless_irv_df) # careless 2, 54, 59

## Salvar respostas descuidadas no arquivo <../data/dfs-careless.csv> (posições 2, 54 e 59)
## Salvar respostas do DFS sem respostas descuidadas no arquivo <../data/dfs.csv>


write.csv(dfs_dat[c(2,54,59),], 'data/dfs-careless.csv')
write.csv(dfs_dat[-c(2,54,59),], 'data/dfs.csv')

