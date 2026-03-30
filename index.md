  

[![Logo](images/stopsquarecropped.png)](https://github.com/NIVANorge/STOPeData)

### STOP Exposure Data App

STOPeData is a data entry and formatting Shiny app in the Source to
Outcome Pathway/Risk assessment database family, designed to make
extracting data on chemical concentrations in the environment from
papers and reports easier.  
  
[Test Version](https://edata.test-stop.niva.no) (Login required) ·
[Prod](https://edata.stop.niva.no) (Login required) · [Report
Bug](https://github.com/NIVANorge/STOPeData/issues/new?labels=bug&template=bug-report---.md)
· [Request
Feature](https://github.com/NIVANorge/STOPeData/issues/new?labels=enhancement&template=feature-request---.md)

Table of Contents

1.  [About The Project](#about-the-project)
    - [Built With](#built-with)
2.  [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Installation](#installation)
3.  [Usage](#usage)
4.  [Roadmap](#roadmap)
5.  [Contact](#contact)
6.  [Acknowledgments](#acknowledgments)

## About The Project

![workflow](images/efve.png)

workflow

This app designed to guide users through the formatting, cleaning and
annotation of exposure/pollution/monitoring data (e.g. mg/L of a
chemical in an environmental matrix). Published studies and reports are
an important source of this data, but it is often fragmented and
difficult to analyse without extensive data cleaning and transformation.
By assisting and automating this step, we hope to make exposure
assessment - and therefore the risk assessment of chemicals in the
environment - as easy as possible.

This app is part of the [Source to Outcome Pathway/Risk assessment
database](https://www.niva.no/radb) family of R Shiny apps, and provides
one-half of the data necessary for environmental risk assessment. Its
counterpart for toxicity/bioassay data is [STOP
qData](https://github.com/NIVANorge/stop-q-data). Environmental risk
predictions can be viewed at the [Source To Outcome
Predictor](https://github.com/NIVANorge/STOP).

([back to top](#readme-top))

### Built With

[![R](https://img.shields.io/badge/R-%23276DC3.svg?logo=r&logoColor=white)](#)

([back to top](#readme-top))

## Getting Started

I haven’t tested setting the app up to run locally on systems other than
my home PC. It *should* work following the below instructions, although
it depends on quite a lot of R packages which you will have to download.
In general, I recommend

### Prerequisites

- R version 4.5.2
- Various R packages (see MANIFEST)

### Installation

1.  (Optional) Get an [Anthropic API key](https://console.anthropic.com)
    for LLM data extraction, and Zenodo/Zenodo Sandbox tokens for upload
    to Zenodo

2a. Clone the repo, or
`sh git clone https://github.com/NIVANorge/STOPeData.git`

2b. Download the package with devtools/pak
`R devtools::install_github("NIVANorge/STOPeData") # pak::pak("NIVANorge/STOPeData")`

3.  Install missing R packages

4.  (Optional) Enter your API keys/tokens in your `.Renviron` file

        ANTHROPIC_API_KEY="sk-ant-api03-..."
        ZENODO_TOKEN="..."
        ZENODO_SANDBOX_TOKEN="..."

5.  Change git remote url to avoid accidental pushes to base project

    ``` sh
    git remote set-url origin sawelch-NIVA/STOPeData
    git remote -v # confirm the changes
    ```

6.  Run app locally:

    ``` r
    golem::run_dev()
    ```

([back to top](#readme-top))

## Usage

This version of the app demo is hosted on Posit Connect Cloud on my
personal account:
<https://sawelch-niva-stopedata.share.connect.posit.cloud/>. This will
be migrated to NIVA’s standard severs when practical.

### Screenshots and Diagrams

![complicated_workflow](inst/app/www/app_mapp.png) An overview of the
manual/LLM assisted workflow.

![sites](images/mod_sites_screenshot.png) Entering data on sampling
sites.

![biota](images/mod_biota_screenshot.png) Entering data about sampled
organisms.

![creed](images/mod_creed_screenshot.png) Assisted assessment of data
quality using the CREED framework.

([back to top](#readme-top))

## Roadmap

Better test architecture and general bug fixes

More user-friendly session saving

Data extraction from structured formats (e.g. Excel spreadsheets, API
calls)

Connection to the Risk Assessment database for lookups and long-term
storage

Support for more formats

See the [open issues](https://github.com/NIVANorge/STOPeData/issues) for
a full list of proposed features (and known issues).

([back to top](#readme-top))

## Contact

Sam Welch - <sam.welch@niva.no>

Project Link: <https://github.com/NIVANorge/STOPeData>

([back to top](#readme-top))

## Acknowledgments

- Project Lead: Knut Erik Tollefsen
- Funding: [EXPECT](https://www.niva.no/en/projects/expect),
  [PARC](https://www.eu-parc.eu/), and [NCTP](https://www.niva.no/radb)
  Projects
- Testers: Li Xi, Knut Erik Tollefsen, Sophie Mentzel, Pierre Blévin,
  Camden Karon Klefbom
- Support and Advice: Viviane Giradin, Andrea Merlina, Kim Leirvik,
  Jemmima Knight, Malcolm Reid
- An LLM (Anthropic Claude Sonnet 4.5) was used in the creation of this
  app and its code.
- Readme template repo:
  [Best-README-Template](https://github.com/othneildrew/Best-README-Template/blob/main/BLANK_README.md)
- (If I’ve left you off please let me know!)

([back to top](#readme-top))
