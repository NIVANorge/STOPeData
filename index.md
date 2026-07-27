# STOPeData

STOPeData (“eData”) is a data entry and formatting Shiny application
(app) in the [Source to Outcome Pathway/Risk Assessment Database
family](https://stop.niva.no/), designed to make extracting data on
chemical concentrations in the environment from papers and reports
easier.

🖥️ [Test Version](https://edata.test-stop.niva.no) (NIVA login required)
· 🖥️ [Prod](https://edata.stop.niva.no) (Login required) · 🪲 [Report
Bug](https://github.com/NIVANorge/STOPeData/issues/new?labels=bug&template=bug-report---.md)
· ❔[Request
Feature](https://github.com/NIVANorge/STOPeData/issues/new?labels=enhancement&template=feature-request---.md)

## Table of Contents

- [About The Project](#about-the-project)
  - [Built With](#built-with)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
- [Usage](#usage)
- [Roadmap](#roadmap)
- [Contact](#contact)
- [Acknowledgments](#acknowledgments)

## About The Project

![](inst/images/graphical_abstract.png)

This app is designed to guide users through the formatting, cleaning and
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

### Built With

[![](https://img.shields.io/badge/R-%23276DC3.svg?logo=r&logoColor=white)](#)

## Getting Started

### Prerequisites

- [R](https://www.r-project.org/about.html) version \>=4.1.0 (currently
  built on 4.5.3)
- [pak](https://pak.r-lib.org/), for dependency installation

### Installation

1.  (Optional) Get an API key for LLM data extraction
    ([Anthropic](https://platform.claude.com/docs/en/get-api-key)/[OpenAI](https://platform.openai.com/api-keys)/[Google
    Gemini](https://aistudio.google.com/api-keys)), and
    [Zenodo](https://developers.zenodo.org/)/Zenodo Sandbox tokens for
    uploading to Zenodo.

2.  Clone the repo, or install from GitHub:

    ``` sh
    git clone https://github.com/NIVANorge/STOPeData.git
    ```

    ``` r

    pak::pak("NIVANorge/STOPeData")
    ```

3.  Install the companion schema package and the remaining dependencies:

    ``` r

    pak::pak("NIVANorge/eDataDRF")   # companion schema package, must be installed separately
    pak::local_install_deps()        # or source dependencies.R for the full list
    ```

4.  (Optional) Enter your API keys/tokens in your `.Renviron` file:

    ``` R
    ANTHROPIC_API_KEY="sk-ant-api03-..."
    OPENAI_API_KEY="..."
    GOOGLE_API_KEY="..."
    ZENODO_TOKEN="..."
    ZENODO_SANDBOX_TOKEN="..."
    ```

5.  If you’re developing on a fork, point `origin` at it to avoid
    accidental pushes to the base project:

    ``` sh
    git remote set-url origin sawelch-NIVA/STOPeData
    git remote -v # confirm the change
    ```

6.  Run the app locally (or use Docker):

    ``` r

    golem::run_dev() # dev mode, more verbose
    shiny::runApp('app.R', host='0.0.0.0', port=3838) # prod mode
    ```

## Usage

Run the application locally

### Diagrams

![complicated_workflow](inst/app/www/app_mapp.png)

complicated_workflow

An overview of the manual/LLM assisted workflow.

## Roadmap

See the [open issues](https://github.com/NIVANorge/STOPeData/issues) for
a full list of proposed features (and known issues).

## Contact

Sam Welch - <sam.welch@niva.no>

Project Link: <https://github.com/NIVANorge/STOPeData>

## Acknowledgments

- Project Lead: Knut Erik Tollefsen
- Funding: [EXPECT](https://www.niva.no/en/projects/expect),
  [PARC](https://www.eu-parc.eu/), and [NCTP](https://www.niva.no/radb)
  Projects
- Testers: Li Xi, Knut Erik Tollefsen, Sophie Mentzel, Pierre Blévin,
  Camden Karon Klefbom
- Support and Advice: Viviane Giradin, Andrea Merlina, Kim Leirvik,
  Jemmima Knight, Malcolm Reid
- LLMs were used in the creation of this app and its code.
- Readme template repo:
  [Best-README-Template](https://github.com/othneildrew/Best-README-Template/blob/main/BLANK_README.md)
- (If I’ve left you off please let me know!)
