FROM rocker/shiny-verse:4.0.5 AS v0.01

MAINTAINER Geiser Chalco <geiser@alumni.usp.br>

LABEL org.label-schema.license="GPL-3.0" \
      org.label-schema.vcs-url="https://github.com/geiser/docker" \
      org.label-schema.vendor="R-studio for my-projects" \
      maintainer="Geiser Chalco <geiser@alumni.usp.br>"

RUN apt-get update -qq \
    && apt-get -y --no-install-recommends install

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

RUN R -e 'if (!"rshinystatistics" %in% rownames(installed.packages())) remotes::install_github("geiser/rshinystatistics")'
RUN R -e 'if (packageVersion("rshinystatistics") < "0.0.0.9100") { remove.packages("rshinystatistics"); remotes::install_github("geiser/rshinystatistics") }'

RUN rm -rf /var/lib/apt/lists/*
