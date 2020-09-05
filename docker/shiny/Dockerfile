FROM rocker/shiny-verse:4.0.2 AS prod

MAINTAINER Geiser Chalco <geiser@usp.br>

LABEL org.label-schema.license="GPL-3.0" \
      org.label-schema.vcs-url="https://github.com/geiser/docker" \
      org.label-schema.vendor="R-studio for my-projects" \
      maintainer="Geiser Chalco <geiser@usp.br>"

RUN /rocker_scripts/install_texlive.sh

ENV SHINY_SERVER_VERSION 1.5.14.948
RUN /rocker_scripts/install_shiny_server.sh

RUN R -e 'remotes::install_github("geiser/rshinystatistics")'
RUN install2.r -s robcbi \
    && install2.r -s remotes \
    && install2.r -s rmarkdown \
    && install2.r -s shinythemes
RUN install2.r -s lavaan \
	&& install2.r -s psych \
	&& install2.r -s sem \
	&& install2.r -s semPlot \
	&& install2.r -s TAM
RUN install2.r -s moments \
    && install2.r -s rcompanion \
    && install2.r -s emmeans \
    && install2.r -s careless 
RUN install2.r -s shinyBS
RUN install2.r -s plotly
RUN install2.r -s nortest


