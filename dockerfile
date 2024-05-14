# Base R Shiny image from Rocker project
FROM rocker/shiny:latest

# Set the working directory in the container
WORKDIR /home/shiny-app

# Install necessary system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'bslib', 'lubridate', 'dplyr', 'ggplot2', 'naniar', 'tidyr', 'plotly', 'zoo', 'gridExtra', 'patchwork', 'htmlwidgets', 'bsicons', 'htmltools', 'leaflet', 'purrr', 'readr', 'data.table', 'shinydashboard', 'shinyjs', 'DT', 'shinycssloaders', 'RcppRoll'), dependencies=TRUE)"

# Copy the Shiny app code and other necessary files into the working directory
COPY app.R ./app.R
COPY global.R ./global.R
COPY functions.R ./functions.R
COPY data_preparation.R ./data_preparation.R

# Copy the entire Data folder into the working directory
COPY Data ./Data

# Expose the application port
EXPOSE 8180

# Command to run the app
CMD ["R", "-e", "shiny::runApp('/home/shiny-app', host='0.0.0.0', port=8180)"]
