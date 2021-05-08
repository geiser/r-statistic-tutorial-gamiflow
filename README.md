# Instalar a aplicação

### Instalar a aplicação usando Docker (recomendado)

Prerequisitos:
- Instalar Docker (https://docs.docker.com/get-docker/) - após instalação precisa reiniciar o sistema operativo
- Instalar WLS2 se usar Windows 10/8 (https://aka.ms/wsl2kernel)
- Instalar github para baixar o código usando github (https://git-scm.com/downloads)

Passos:
1. Baixar o código fonte da aplicação usando alguma das duas alternativas
   - Pode baixar o código do aplicativo fazendo click no botão (code) ou diretamente no link: https://github.com/geiser/r-statistic-tutorial-gamiflow/archive/master.zip
   - Pode baixar o código do aplicativo usando git
   ```
   git clone https://github.com/geiser/r-statistic-tutorial-gamiflow.git
   ```

2. No terminal, acessar na pasta do código fonte
   ```
   cd r-statistic-tutorial-gamiflow
   ```

3. Em linux ou mac os, você devera criar e atribuir permissões de leitura e escrita para todos usuários na pasta `report`
   ```
   mkdir report
   sudo chmod a+rw report
   ```

4. Baixar a imagem docker da aplicação
   ```
   docker-compose pull shiny
   ```

5. Inicializar a aplicação usando docker
   ```
   docker-compose up -d shiny
   ```

6. Acessar à aplicação no browser no site: http://localhost:3838/r-statistic-tutorial-gamiflow/


7. Parar a aplicação usando docker
   ```
   docker-compose down
   ```

*Video de instalação da aplicação usando Docker em windows* 

   [![Watch the video](https://img.youtube.com/vi/aPuLWGwQYHU/hqdefault.jpg)](https://www.youtube.com/embed/aPuLWGwQYHU)



### Alternativamente pode instalar a aplicação em r-studio

Prerequisitos:
- Instalar r-studio (https://www.rstudio.com/products/rstudio/)
- Ativar encoding UTF-8 para windows 10/8 (https://tinyurl.com/v3rpa992)
- Instalar github para baixar o código usando github (https://git-scm.com/downloads)

> **OBSERVAÇÃO IMPORTANTE**
>  Quando o aplicativo é executado em Windows r-studio, o arquivo de dados (dataset) a ser utilizado na análise de dados necesáriamente deve evitar o uso de
>  cedilhas, acentuação e outros caracteres especiais (remova todos esses caracteres no seu arquivo de dados)

Passos:
1. Baixar o código fonte da aplicação usando alguma das duas alternativas
   - Pode baixar o código do aplicativo fazendo click no botão (code) ou diretamente no link: https://github.com/geiser/r-statistic-tutorial-gamiflow/archive/master.zip
   - Pode baixar o código do aplicativo usando git
   ```
   git clone https://github.com/geiser/r-statistic-tutorial-gamiflow.git
   ```
   
2. Abrir o arquivo app.R e executar o aplicativo do arquivo app.R 
   
*Video de instalação da aplicação usando r-studio* 

   [![Watch the video](https://img.youtube.com/vi/Dcpdo4MW2g4/hqdefault.jpg)](https://www.youtube.com/embed/Dcpdo4MW2g4)
   [![Watch the video](https://img.youtube.com/vi/5qjEO99wZuE/hqdefault.jpg)](https://www.youtube.com/embed/5qjEO99wZuE)



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




