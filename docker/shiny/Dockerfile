FROM rocker/shiny-verse:4.0.5 AS prod

MAINTAINER Geiser Chalco <geiser@alumni.usp.br>

LABEL org.label-schema.license="GPL-3.0" \
      org.label-schema.vcs-url="https://github.com/geiser/docker" \
      org.label-schema.vendor="R-studio for my-projects" \
      maintainer="Geiser Chalco <geiser@alumni.usp.br>"

RUN apt-get update -qq \
    && apt-get -y --no-install-recommends install

RUN /rocker_scripts/install_texlive.sh
RUN chown -R shiny:shiny /opt/texlive \
    && chmod -R 777 /opt/texlive
RUN chown -R shiny:shiny /usr/local/texlive \
    && chmod -R 777 /usr/local/texlive

RUN R -e 'remotes::install_github("geiser/rshinystatistics")'
RUN install2.r -s robcbi \
    && install2.r -s remotes \
    && install2.r -s rmarkdown \
    && install2.r -s shinythemes
RUN install2.r -s lavaan \
	&& install2.r -s psych \
	&& install2.r -s sem \
	&& install2.r -s semPlot
RUN install2.r -s moments \
    && install2.r -s rcompanion \
    && install2.r -s emmeans \
    && install2.r -s careless 
RUN install2.r -s shinyBS
RUN install2.r -s plotly
RUN install2.r -s nortest

RUN rm -rf /var/lib/apt/lists/*
