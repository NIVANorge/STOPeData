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

COPY ./DESCRIPTION ./DESCRIPTION
RUN R -s -e "install.packages('pak', repos = sprintf('https://r-lib.github.io/p/pak/stable/%s/%s/%s', .Platform\$pkgType, R.Version()\$os, R.Version()\$arch)); pak::local_install_deps()"

COPY manifest.json ./manifest.json
COPY ./NAMESPACE ./NAMESPACE
COPY ./app.R ./app.R 
COPY ./R ./R
COPY ./inst ./inst

# Run app
CMD ["R", "--quiet", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=3838)"]