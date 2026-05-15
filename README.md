<!-- Improved compatibility of back to top link: See: https://github.com/othneildrew/Best-README-Template/pull/73 -->
<a id="readme-top"></a>

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/NIVANorge/STOPeData">
    <img src="https://github.com/NIVANorge/STOPeData/blob/main/inst/images/eData_DRF_logo.svg" alt="Logo" width="80" height="80">
  </a>

<h3 align="center">STOP Exposure Data App</h3>

  <p align="center">
    STOPeData is a data entry and formatting Shiny app in the Source to Outcome Pathway/Risk assessment database family, designed to make extracting data on chemical concentrations in the environment from papers and reports easier. 
    <br />
    <br />
    <a href="https://edata.test-stop.niva.no">Test Version</a> (Login required)
    &middot;
    <a href="https://edata.stop.niva.no">Prod</a> (Login required)
    &middot;
    <a href="https://github.com/NIVANorge/STOPeData/issues/new?labels=bug&template=bug-report---.md">Report Bug</a>
    &middot;
    <a href="https://github.com/NIVANorge/STOPeData/issues/new?labels=enhancement&template=feature-request---.md">Request Feature</a>
  </p>
</div>

<!-- ABOUT THE PROJECT -->
## About The Project

This app designed to guide users through the formatting, cleaning and annotation of exposure/pollution/monitoring data (e.g. mg/L of a chemical in an environmental matrix). Published studies and reports are an important source of this data, but it is often fragmented and difficult to analyse without extensive data cleaning and transformation. By assisting and automating this step, we hope to make exposure assessment - and therefore the risk assessment of chemicals in the environment - as easy as possible.

This app is part of the [Source to Outcome Pathway/Risk assessment database](https://www.niva.no/radb) family of R Shiny apps, and provides one-half of the data necessary for environmental risk assessment. Its counterpart for toxicity/bioassay data is [STOP qData](https://github.com/NIVANorge/stop-q-data). Environmental risk predictions can be viewed at the [Source To Outcome Predictor](https://github.com/NIVANorge/STOP). 


### Built With

[![R](https://img.shields.io/badge/R-%23276DC3.svg?logo=r&logoColor=white)](#)

<!-- badges: start -->
[![R-CMD-check](https://github.com/NIVANorge/STOPeData/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NIVANorge/STOPeData/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/NIVANorge/STOPeData/graph/badge.svg)](https://app.codecov.io/gh/NIVANorge/STOPeData)
<!-- badges: end -->

<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

* R version 4.5.2
* Various R packages (see MANIFEST)

### Installation

1. (Optional) Get an [Anthropic API key](https://console.anthropic.com) for LLM data extraction, and Zenodo/Zenodo Sandbox tokens for upload to Zenodo
   
  2a. Clone the repo, or
   ```sh
   git clone https://github.com/NIVANorge/STOPeData.git
   ```

  2b. Download the package with devtools/pak
  ```R
  devtools::install_github("NIVANorge/STOPeData")
  # pak::pak("NIVANorge/STOPeData")
  ```

3. Install missing R packages
   
4. (Optional) Enter your API keys/tokens in your `.Renviron` file 
   ```
   ANTHROPIC_API_KEY="sk-ant-api03-..."
   ZENODO_TOKEN="..."
   ZENODO_SANDBOX_TOKEN="..."
   ```
5. Change git remote url to avoid accidental pushes to base project
   ```sh
   git remote set-url origin sawelch-NIVA/STOPeData
   git remote -v # confirm the changes
   ```
6. Run app locally:
   ```r
   golem::run_dev()
   ```
### Screenshots and Diagrams

TODO

<!-- CONTACT -->
## Contact

Sam Welch - sam.welch@niva.no

Project Link: [https://github.com/NIVANorge/STOPeData](https://github.com/NIVANorge/STOPeData)


<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

* Project Lead: Knut Erik Tollefsen
* Funding: [EXPECT](https://www.niva.no/en/projects/expect), [PARC](https://www.eu-parc.eu/), and [NCTP](https://www.niva.no/radb) Projects
* Testers: Li Xi, Knut Erik Tollefsen, Sophie Mentzel, Pierre Blévin, Camden Karon Klefbom
* Support and Advice: Viviane Giradin, Andrea Merlina, Kim Leirvik, Jemmima Knight, Malcolm Reid
* An LLM (Anthropic Claude Sonnet 4.5) was used in the creation of this app and its code.
* Readme template repo: [Best-README-Template](https://github.com/othneildrew/Best-README-Template/blob/main/BLANK_README.md)

