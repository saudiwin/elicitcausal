# Use rocker/r2u:noble as the base image
FROM rocker/r2u:noble

RUN useradd --create-home --shell /bin/bash shiny \
    && chown -R shiny:shiny /home/shiny

# Install system dependencies for Shiny and database support
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libpq-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    gdebi-core \
    curl \
    libcairo2-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages via APT from r2u
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    r-cran-shiny \
    r-cran-devtools \
    r-cran-shinydashboard \
    r-cran-dbi \
    r-cran-password \
    r-cran-rpostgres \
    r-cran-shiny.i18n \
    r-cran-cognitor \
    r-cran-glue \
    r-cran-shinyvalidate \
    r-cran-countries \
    r-cran-httr \
    r-cran-sortable \
    r-cran-jsonlite \
    r-cran-readr \
    r-cran-tidyverse \
    r-cran-openssl \
    r-cran-shinyalert \
    r-cran-quarto \
    r-cran-countrycode \
    r-cran-shinywidgets \
    r-cran-shinybrowser \
    r-cran-lazyeval \
    r-cran-crosstalk \
    r-cran-pool \
    r-cran-ip2location.io \
    && rm -rf /var/lib/apt/lists/*

# Create and set the correct permissions for Shiny Server directories
#RUN mkdir -p /var/lib/shiny-server/bookmarks && \
#    mkdir -p /srv/shiny-server/ && \
#    chown -R shiny:shiny /var/lib/shiny-server /srv/shiny-server /usr/lib/shiny-server

#RUN echo "\noptions(shiny.port=3838, shiny.host='0.0.0.0')" >> /usr/local/lib/R/etc/Rprofile.site

# add quarto support
RUN curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb

# Install GitHub packages using remotes
RUN R -e "remotes::install_github('saudiwin/sortable_survey@main')"
RUN R -e "remotes::install_github('saudiwin/surveydown@sdnextselect')"

# Modify Shiny Server config to increase timeout
# Copy custom Shiny Server configuration file
#COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Copy the .Renviron file to the home directory (where R will read it automatically)
# Copy .Renviron globally
COPY .Renviron /etc/R/.Renviron
RUN chown shiny:shiny /etc/R/.Renviron

COPY .Renviron /home/shiny/.Renviron
RUN chown shiny:shiny /home/shiny/.Renviron

# Copy the Shiny app to the container
COPY ./app /home/shiny/app
RUN chown -R shiny:shiny /home/shiny/app
#RUN chmod -R 755 /srv/shiny-server

# Set working directory to prevent permission issues
WORKDIR /home/shiny/app

# Expose the default Shiny server port
EXPOSE 3838

# Switch to the shiny user before running the app
USER shiny

# Start Shiny Server
CMD ["R", "-q", "-e", "shiny::runApp('/home/shiny/app')"]