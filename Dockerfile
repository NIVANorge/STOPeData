FROM rocker/shiny:latest AS base

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
    libgdal-dev \
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
COPY ./man ./man
COPY ./data ./data

# Test stage: installs STOPeData and testthat, then runs the headless test
# suite. Fails the build if any test fails.
FROM base AS test

COPY ./tests ./tests

RUN R -s -e "pak::pak('testthat')" \
    && R -s -e "pak::local_install(dependencies = FALSE, ask = FALSE)" \
    && R -s -e "testthat::test_local(stop_on_failure = TRUE)"

# Final image: COPY --from=test forces the test stage to build (and pass)
# before this stage can complete.
FROM base AS final

COPY --from=test /home/shiny/stopedata/DESCRIPTION ./DESCRIPTION

# Run app
CMD ["R", "--quiet", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=3838)"]