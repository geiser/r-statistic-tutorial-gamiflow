wants <- c('digest', 'readr', 'dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(readr)      # biblioteca para leitura de arquivos CVS
library(digest)     # biblioteca para anonimizar dados
library(dplyr)      # biblioteca para manipular data.frames

## Anonimizando dados dos participantes

participants <- read_csv("raw-data/participants.csv")

participants[["UserID"]] <- sapply(participants$email, FUN = function(x) { # anonimizando por email
  digest(x, algo = "crc32")
})

write_csv(participants[,c("UserID","cenario")], "data/participants.csv") # escrevendo resultados anonimizados

## Anonimizando questionario de predispocição de fluxo (DFS)

pre_dfs <- read_csv("raw-data/survey-0-dfs.csv")

dfs <- merge(participants, pre_dfs, by='email') # combinando tabelas pelo emails
dfs <- select(dfs,
              starts_with("UserID"),
              starts_with("cenario"),
              starts_with("media"),
              starts_with("dimensao"),
              starts_with("Q"))               #selecionando dados não sensiveis

write_csv(dfs, "data/survey-dfs.csv")

## Anonimizando questionario da experiência de fluxo (FSS)

pre_fss <- read_csv("raw-data/survey-0-fss.csv")

fss <- merge(participants, pre_fss, by='email')
(fss <- select(fss,
              starts_with("UserID"),     # identificado único
              starts_with("cenario"),    # distribuição do participante entre cenário gamificado e não gamificado
              starts_with("media"),      # score da media de fluxo
              starts_with("dimensao"),   # score da dimensao 1 até 9 
              starts_with("Q")))         # resposta no item Q1 até Q36

write_csv(fss, "data/survey-fss.csv")

## Anonimizando e calculando média dos componentes no questionario QPJ-Br

pre_qpj <- read_csv("raw-data/survey-0-qpj.csv")
pre_qpj <- data.frame(
  nome = pre_qpj$`Nome completo`
  , email = pre_qpj$email
  # componente 1 - realização
  , ItemAcom0 = pre_qpj$`...estar em vantagem em relação aos outros jogadores?`
  , ItemAcom3 = pre_qpj$`... observar seu desempenho em relação a outros jogadores?`
  , ItemAcom5 = pre_qpj$`... derrotar outros jogadores?`
  , ItemAcom6 = pre_qpj$`... tenta provocar ou irritar de propósito outros jogadores?`
  , ItemAcom8 = pre_qpj$`... fazer coisas que incomodam outros jogadores?`
  , ItemAcom9 = pre_qpj$`... competir com outros jogadores?` 
  , ItemAAva2 = pre_qpj$`... tornar-se muito bom em um jogo?`
  # componente 2 - imersão
  , ItemIcus0 = pre_qpj$`... pensa em itens ou características que poderiam ser mudadas para customizar a aparência do seu personagem ou o jogo em si?`
  , ItemIcus1 = pre_qpj$`... que estejam combinando em cor e estilo as armadura ou roupas de seu personagem ou que as peças do jogo tenham uma aparência interessante?`
  , ItemIcus3 = pre_qpj$`... que a aparência do seu personagem seja diferente da aparência de outros personagens?`
  , ItemIcus9 = pre_qpj$`4 - Quanto tempo você passa customizando seu personagem durante a criação dele?`
  , ItemIrol8 = pre_qpj$`... estar imerso em um mundo de fantasia?`
  # componente 3 - social
  , ItemSrel0 = pre_qpj$`... conversa com outros jogadores (on-line) sobre seus problemas/questões pessoais?`
  , ItemSrel1 = pre_qpj$`... outros jogadores (on-line) te ofereceram ajuda quando você teve um problema na vida real?`
  , ItemSrel9 = pre_qpj$`... tem conversas significativas com outros jogadores?`
  , ItemSsoc7 = pre_qpj$`... ajudar outros jogadores?`
  , ItemSsoc8 = pre_qpj$`... conhecer outros jogadores?`
  , ItemSsoc9 = pre_qpj$`... conversar com outros jogadores?`
  , ItemStra2 = pre_qpj$`... procura fazer parte de um grupo em jogos?`
)
pre_qpj[["ItemIcus9"]] <- sapply(pre_qpj$ItemIcus9, FUN = function(x) {
  if (x == 'Sempre') return(5)
  if (x == 'Frequentemente') return(4)
  if (x == 'Não sei ao certo') return(3)
  if (x == 'Pouco tempo') return(2)
  if (x == 'Pouquíssimo tempo') return(1)                             
})

pre_qpj <- mutate(
  pre_qpj
  , "realizacao" = (ItemAcom0+ItemAcom3+ItemAcom5+ItemAcom6+ItemAcom8+ItemAcom9+ItemAAva2)/7
  , "imersao" = (ItemIcus0+ItemIcus1+ItemIcus3+ItemIcus9+ItemIrol8)/5
  , "social" = (ItemSrel0+ItemSrel1+ItemSrel9+ItemSsoc7+ItemSsoc8+ItemSsoc9+ItemStra2)/7
)


qpj <- merge(participants, pre_qpj, by='email')
qpj <- select(qpj,
              starts_with("UserID"),
              starts_with("cenario"),
              starts_with("Item"),
              starts_with("realizacao"),
              starts_with("imersao"),
              starts_with("social"))

write_csv(qpj, "data/survey-qpj.csv")


## Anonimizando tabela de notas da provinha durante pré-teste

pre_pre <- read_csv("raw-data/pre-test.csv")

pre <- merge(participants, pre_pre, by='email') # combinando tabelas pelo emails
pre <- select(pre,
              starts_with("UserID"),
              starts_with("cenario"),
              starts_with("nota"))               #selecionando dados não sensiveis

write_csv(pre, "data/pre-test.csv")



## Anonimizando tabela das notas da provinha durante pós-teste

pos_pre <- read_csv("raw-data/pos-test.csv")

pos <- merge(participants, pos_pre, by='email') # combinando tabelas pelo emails
pos <- select(pos,
              starts_with("UserID"),
              starts_with("cenario"),
              starts_with("nota"))               #selecionando dados não sensiveis

write_csv(pos, "data/pos-test.csv")


## Anonimizando dados coletados do engajamento dos participantes

engagement_pre <- read_csv("raw-data/engagement.csv")

engagement <- merge(participants, engagement_pre, by='email') # combinando tabelas pelo emails
engagement <- select(engagement,
                     starts_with("UserID"),
                     starts_with("cenario"),
                     starts_with("pontos"),
                     starts_with("tempo.permanencia"),
                     starts_with("tentativas"),
                     starts_with("submissoes"),
                     starts_with("rep.erradas"),
                     starts_with("escolhas"))               #selecionando dados não sensiveis

write_csv(engagement, "data/engagement.csv")

