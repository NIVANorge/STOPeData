FROM rocker/shiny:latest

# Install system dependencies 
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/shiny/stopedata


COPY renv.lock renv.lock
RUN R -e "install.packages(c('renv', 'pak'), repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -s -e "options(renv.config.pak.enabled = TRUE); renv::restore()"
# Set the working directory *inside the shiny home directory*

COPY manifest.json ./manifest.json
COPY ./DESCRIPTION ./DESCRIPTION
COPY ./NAMESPACE ./NAMESPACE
COPY ./app.R ./app.R 
COPY ./R ./R
COPY ./inst ./inst

# Run app
CMD ["R", "--quiet" "-e", "shiny::runApp('app.R', host='0.0.0.0', port=3838)"]
