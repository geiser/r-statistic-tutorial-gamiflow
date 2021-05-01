# Instalar a aplicação

### Instalar a aplicação em r-studio

Prerequisitos:
- Instalar r-studio (https://www.rstudio.com/products/rstudio/)

Passos:
1. Baixar o código fonte: Pode baixar o aplicativo fazendo click no botão (code) ou diretamente no link: https://github.com/geiser/r-statistic-tutorial-gamiflow/archive/master.zip
   [![Watch the video](https://img.youtube.com/vi/Dcpdo4MW2g4/hqdefault.jpg)](https://www.youtube.com/embed/Dcpdo4MW2g4)
2. Abrir o arquivo app.R 
3. Executar o aplicativo do arquivo app.R 
   [![Watch the video](https://img.youtube.com/vi/5qjEO99wZuE/hqdefault.jpg)](https://www.youtube.com/embed/5qjEO99wZuE)

### Instalar a aplicação usando Docker

Prerequisitos:
- Instalar docker (https://docs.docker.com/engine/install/)
- Instalar docker-compose (https://docs.docker.com/compose/install/)

Passos:
1. No terminal, baixar o código fonte da aplicação executando:
```
git clone https://github.com/geiser/r-statistic-tutorial-gamiflow.git
```
2. configurar o arquivo Makefile 
```
./configure
```
3. baixar imagem do docker
```
make pull service=shiny
```
4. executar aplicação
```
make up service=shiny
```
5. parar aplicação
```
make down service=shiny 
```

# Tutorial em português para análises de dados em R

O tutorial de passo a apasso na wiki:
https://github.com/geiser/r-statistic-tutorial-gamiflow/wiki

Pode baixar o aplicativo fazendo click no botão (code) ou diretamente no link: https://github.com/geiser/r-statistic-tutorial-gamiflow/archive/master.zip 

Versão demo da aplicação: https://weblab.nees.com.br/r-statistic-tutorial-gamiflow/

# Análises Estatísca em R (Pesquisa com Gamiflow)

Seja bem-vindo ao tutorial de **análises estatistica** em R para pesquisas de gamificação na educação com o framework **Gamiflow**.
Prévio a seguir com o tutorial, recomendamos assistir o [material de apoio](https://github.com/geiser/r-statistic-tutorial-gamiflow/wiki/material-de-apoio), para ter alguma familiaridade básica com R e análises estatística.

**Conteúdo**
* 1ra Etapa: Tratamento prévio dos dados
  * [Identificação e remoção das respostas descuidadas](https://github.com/geiser/r-statistic-tutorial-gamiflow/wiki/01-Identificação-e-remoção-das-respostas-descuidadas)
  * [Anonimização dos dados](https://github.com/geiser/r-statistic-tutorial-gamiflow/wiki/02-Anonimização-dos-dados)
* [2da Etapa: Realizando os testes de hipóteses para as médias](https://github.com/geiser/r-statistic-tutorial-gamiflow/wiki/03-Testes-de-hipóteses)
  

# Vídeos tutorial de apoio

* Como subir seu projeto e iniciar as análises estatíscas no [https://rstudio.cloud/](https://rstudio.cloud/) <- alternativa ao rstudio desktop

  [![Watch the video](https://img.youtube.com/vi/smMmkKKXuLw/hqdefault.jpg)](https://www.youtube.com/embed/smMmkKKXuLw)

* Mini-tutorial de análises estatístico de testes de hipóteses em rshiny-statistics

  [![Watch the video](https://img.youtube.com/vi/DEn4n4XeRx0/hqdefault.jpg)](https://www.youtube.com/embed/DEn4n4XeRx0)

  [![Watch the video](https://img.youtube.com/vi/Joi_k_NmWvM/hqdefault.jpg)](https://www.youtube.com/embed/Joi_k_NmWvM)

* Execução de teste t de amostras independentes em rshiny-statistics  (independent two-sample t-test) 

  [![Watch the video](https://img.youtube.com/vi/zzGPqNTpu70/hqdefault.jpg)](https://www.youtube.com/embed/zzGPqNTpu70)

* Execução de teste t de amostras emparelhadas em rshiny-statistics (paired two-sample t-test) 

  [![Watch the video](https://img.youtube.com/vi/SlZIFdrQr9o/hqdefault.jpg)](https://www.youtube.com/embed/SlZIFdrQr9o)

* Execução de teste ANCOVA em rshiny-statistics

  [![Watch the video](https://img.youtube.com/vi/kSDY78TxsLc/hqdefault.jpg)](https://www.youtube.com/embed/kSDY78TxsLc)

* Execução de teste ANOVA fatorial (factorial ANOVA) em rshiny-statistics (one-way ANOVA, two-way ANOVA and three-way ANOVA) 

  [![Watch the video](https://img.youtube.com/vi/MRhdolkQWdM/hqdefault.jpg)](https://www.youtube.com/embed/MRhdolkQWdM)

* Execução de teste ANOVA com medidas repetidas (repeated/mixed ANOVA) - one-way ANOVA, two-way ANOVA and three-way mixed ANOVA

  [![Watch the video](https://img.youtube.com/vi//hqdefault.jpg)](https://www.youtube.com/embed/)




