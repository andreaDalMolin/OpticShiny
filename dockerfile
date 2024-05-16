# Base R Shiny image from Rocker project
FROM rocker/shiny:latest

# Set the working directory in the container
WORKDIR /home/shiny-app

# Install necessary system libraries and cron
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    cron

# Install R packages
RUN R -e "install.packages(c('shiny', 'bslib', 'lubridate', 'dplyr', 'ggplot2', 'naniar', 'tidyr', 'plotly', 'zoo', 'gridExtra', 'patchwork', 'htmlwidgets', 'bsicons', 'htmltools', 'leaflet', 'purrr', 'readr', 'data.table', 'shinydashboard', 'shinyjs', 'DT', 'shinycssloaders', 'RcppRoll', 'readxl', 'cronR'), dependencies=TRUE)"

# Copy the Shiny app code and other necessary files into the working directory
COPY app.R ./app.R
COPY global.R ./global.R
COPY functions.R ./functions.R
COPY data_preparation.R ./data_preparation.R
COPY OpticDataCleaner.R ./OpticDataCleaner.R
COPY surge_periods_updater.R ./surge_periods_updater.R
COPY schedule_cron.R ./schedule_cron.R

# Expose the application port
EXPOSE 8180

# Add the cron job
RUN Rscript /home/shiny-app/schedule_cron.R

# Start cron and the Shiny app
CMD cron && R -e "shiny::runApp('/home/shiny-app', host='0.0.0.0', port=8180)"
