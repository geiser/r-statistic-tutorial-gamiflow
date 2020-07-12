library(readr)

fss <- read_csv("data/fss.csv")
dfs <- read_csv("data/dfs.csv")
qpj <- read_csv("data/qpj.csv")
engagement <- read_csv("data/engagement.csv")

participants <- read_csv("data/participants.csv")

###################################################################################################
## Generação de tabelas para testes estatísticos das médias em referência à experiencia de fluxo ##
###################################################################################################

# fss

fss <- select(fss,
              starts_with("UserID"), starts_with("cenario"),
              starts_with("media"), starts_with("dimensao"))
write_csv(fss, "data/fss.csv")

# .. qpj + fss
qpj_fss <- merge(qpj, fss)
qpj_fss <- select(qpj_fss,
                  starts_with("UserID"), starts_with("cenario"),
                  starts_with("realizacao"), starts_with("social"), starts_with("imersao"),
                  starts_with("media"), starts_with("dimensao"))

write_csv(qpj_fss, 'data/qpj-fss.csv')

# .. dfs + fss
dfs_fss <- merge(dfs, fss, by=c("UserID","cenario"), suffixes = c(".dfs",".fss"))
dfs_fss <- select(dfs_fss,
                  starts_with("UserID"), starts_with("cenario"),
                  ends_with(".dfs"), ends_with(".fss"))

write_csv(dfs_fss, 'data/dfs-fss.csv')

# .. qpj+ dfs + fss
qpj_dfs_fss <- merge(qpj, dfs)
qpj_dfs_fss <- merge(qpj_dfs_fss, fss, by=c("UserID","cenario"), suffixes = c(".dfs",".fss"))

qpj_dfs_fss <- select(qpj_dfs_fss,
                      starts_with("UserID"), starts_with("cenario"),
                      starts_with("realizacao"), starts_with("social"), starts_with("imersao"),
                      ends_with(".dfs"), ends_with(".fss"))

write_csv(qpj_dfs_fss, 'data/qpj-dfs-fss.csv')

###########################################################################################
## Generação de tabelas para testes estatísticos das médias em referência ao engajamento ##
###########################################################################################

# .. qpj + engagement
qpj_engagement <- merge(qpj, engagement)
qpj_engagement <- select(qpj_engagement,
                         starts_with("UserID"), starts_with("cenario"),
                         starts_with("realizacao"), starts_with("social"), starts_with("imersao"),
                         starts_with("pontos"), starts_with("tempo.permanencia"), starts_with("tentativas"),
                         starts_with("submissoes"), starts_with("rep.erradas"), starts_with("escolhas"))

write_csv(qpj_engagement, 'data/qpj-engagement.csv')

# .. dfs + engagement
dfs_engagement <- merge(dfs, engagement, by=c("UserID","cenario"))
dfs_engagement <- select(dfs_engagement,
                  starts_with("UserID"), starts_with("cenario"),
                  starts_with("media"), starts_with("dimensao"),
                  starts_with("pontos"), starts_with("tempo.permanencia"), starts_with("tentativas"),
                  starts_with("submissoes"), starts_with("rep.erradas"), starts_with("escolhas"))

write_csv(dfs_engagement, 'data/dfs-engagement.csv')

# .. qpj+ dfs + engagement
qpj_dfs_engagement <- merge(qpj, dfs)
qpj_dfs_engagement <- merge(qpj_dfs_engagement, engagement, by=c("UserID","cenario"))

qpj_dfs_engagement <- select(qpj_dfs_engagement,
                      starts_with("UserID"), starts_with("cenario"),
                      starts_with("realizacao"), starts_with("social"), starts_with("imersao"),
                      starts_with("media"), starts_with("dimensao"),
                      starts_with("pontos"), starts_with("tempo.permanencia"), starts_with("tentativas"),
                      starts_with("submissoes"), starts_with("rep.erradas"), starts_with("escolhas"))

write_csv(qpj_dfs_engagement, 'data/qpj-dfs-engagement.csv')


###########################################################################################
## Generação de tabelas para testes estatísticos das médias em referência ao aprendizado ##
###########################################################################################

pre <- read_csv('data/pre-test.csv')
pos <- read_csv('data/pos-test.csv')

# .. prepos

prepos <- merge(pos, pre, by=c("UserID", "cenario"), suffixes = c(".pos",".pre"))
prepos[["dif.nota"]] <- prepos$nota.pos - prepos$nota.pre 

write_csv(prepos, 'data/prepos.csv')

# .. qpj + prepos
qpj_prepos <- merge(qpj, prepos)
qpj_prepos <- select(qpj_prepos,
                     starts_with("UserID"), starts_with("cenario"),
                     starts_with("realizacao"), starts_with("social"), starts_with("imersao"),
                     starts_with("nota"), starts_with("dif.nota"))

write_csv(qpj_prepos, 'data/qpj-prepos.csv')

# .. dfs + prepos
dfs_prepos <- merge(dfs, prepos, by=c("UserID","cenario"))
dfs_prepos <- select(dfs_prepos,
                     starts_with("UserID"), starts_with("cenario"),
                     starts_with("media"), starts_with("dimensao"),
                     starts_with("nota"), starts_with("dif.nota"))

write_csv(dfs_prepos, 'data/dfs-prepos.csv')

# .. qpj+ dfs + prepos
qpj_dfs_prepos <- merge(qpj, dfs)
qpj_dfs_prepos <- merge(qpj_dfs_prepos, prepos, by=c("UserID","cenario"))

qpj_dfs_prepos <- select(qpj_dfs_prepos,
                         starts_with("UserID"), starts_with("cenario"),
                         starts_with("realizacao"), starts_with("social"), starts_with("imersao"),
                         starts_with("media"), starts_with("dimensao"),
                         starts_with("nota"), starts_with("dif.nota"))

write_csv(qpj_dfs_prepos, 'data/qpj-dfs-prepos.csv')






