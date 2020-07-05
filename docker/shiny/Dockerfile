FROM rocker/shiny:latest AS prod
MAINTAINER Geiser Chalco <geiser@usp.br>

LABEL org.label-schema.license="GPL-3.0" \
      org.label-schema.vcs-url="https://github.com/geiser/docker" \
      org.label-schema.vendor="Statistics tools through R-shiny" \
      maintainer="Geiser Chalco <geiser@usp.br>"

# Install R packages
RUN install2.r -s plotly \
	&& install2.r -s DT \
	&& install2.r -s ggplot2 \
	&& install2.r -s rcompanion 
RUN install2.r -s rstatix \
	&& install2.r -s emmeans \
	&& install2.r -s stats \
	&& install2.r -s car \
	&& install2.r -s readr \
	&& install2.r -s dplyr \
	&& install2.r -s careless
RUN install2.r -s ggpubr
RUN install2.r -s shinyjs

