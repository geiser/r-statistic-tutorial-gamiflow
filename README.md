# Instalar a Aplicação

### Instalar a aplicação usando Docker (método recomendado)

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



### Instalar a aplicação em r-studio (método alternativo)

Prerequisitos:
- Instalar R 4.0.5 (https://vps.fmvz.usp.br/CRAN/)
- Instalar r-studio (https://www.rstudio.com/products/rstudio/)
- Ativar encoding UTF-8 para windows 10/8 (https://tinyurl.com/v3rpa992)
- Instalar github para baixar o código usando github (https://git-scm.com/downloads)

> **OBSERVAÇÃO IMPORTANTE**
>  Quando o aplicativo é executado em Windows usando o r-studio, o arquivo de dados (dataset) a ser utilizado na análise de dados
>  necesáriamente deve evitar o uso de cedilhas, acentuação e outros caracteres especiais (remova todos esses caracteres no seu arquivo de dados)

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



# Como Usar a Aplicação e Versão Demo

* O tutorial em português para análise de dados passo a apasso: https://github.com/geiser/r-statistic-tutorial-gamiflow/wiki
* Versão demo da aplicação: https://weblab.nees.com.br/r-statistic-tutorial-gamiflow/
