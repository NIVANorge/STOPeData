<!-- Improved compatibility of back to top link: See: https://github.com/othneildrew/Best-README-Template/pull/73 -->
<a id="readme-top"></a>

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/sawelch-NIVA/STOPeData">
    <img src="images/stopsquarecropped.png" alt="Logo" width="80" height="80">
  </a>

<h3 align="center">STOP Exposure Data App</h3>

  <p align="center">
    STOPeData is a data entry and formatting Shiny app in the Source to Outcome Pathway/Risk assessment database family, designed to make extracting data on chemical concentrations in the environment from papers and reports easier. 
    <br />
    <br />
    <a href="https://stop.t.niva.no/eData/">View Test Version</a> (requires NIVA account)
    &middot;
    <a href="https://github.com/sawelch-NIVA/STOPeData/issues/new?labels=bug&template=bug-report---.md">Report Bug</a>
    &middot;
    <a href="https://github.com/sawelch-NIVA/STOPeData/issues/new?labels=enhancement&template=feature-request---.md">Request Feature</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

![workflow]

This app designed to guide users through the formatting, cleaning and annotation of exposure/pollution/monitoring data (e.g. mg/L of a chemical in an environmental matrix). Published studies and reports are an important source of this data, but it is often fragmented and difficult to analyse without extensive data cleaning and transformation. By assisting and automating this step, we hope to make exposure assessment - and therefore the risk assessment of chemicals in the environment - as easy as possible.

This app is part of the [Source to Outcome Pathway/Risk assessment database](https://www.niva.no/radb) family of R Shiny apps, and provides one-half of the data necessary for environmental risk assessment. Its counterpart for toxicity/bioassay data is [STOP qData](https://github.com/NIVANorge/stop-q-data). Environmental risk predictions can be viewed at the [Source To Outcome Predictor](https://github.com/NIVANorge/STOP). 

<p align="right">(<a href="#readme-top">back to top</a>)</p>


### Built With

[![R](https://img.shields.io/badge/R-%23276DC3.svg?logo=r&logoColor=white)](#)

<!-- badges: start -->
[![R-CMD-check](https://github.com/NIVANorge/STOPeData/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NIVANorge/STOPeData/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- GETTING STARTED -->
## Getting Started

I haven't tested setting the app up to run locally on systems other than my home PC. It *should* work following the below instructions, although it depends on quite a lot of R packages which you will have to download. In general, I recommend 

### Prerequisites

* R version 4.5.2
* Various R packages (see MANIFEST)

### Installation

1. (Optional) Get an [Anthropic API key](https://console.anthropic.com) for LLM data extraction
2. Clone the repo
   ```sh
   git clone https://github.com/sawelch-NIVA/STOPeData.git
   ```
3. Install NPM packages
   ```sh
   npm install
   ```
4. (Optional) Enter your API in your `.Renviron` file 
   ```js
   ANTHROPIC_API_KEY = 'sk-ant-api03-...';
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

<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- USAGE EXAMPLES -->
## Usage

This version of the app demo is hosted on Posit Connect Cloud on my personal account: https://sawelch-niva-stopedata.share.connect.posit.cloud/. This will be migrated to NIVA's standard severs when practical.

### Screenshots and Diagrams


![complicated_workflow]
An overview of the manual/LLM assisted workflow.

![sites]
Entering data on sampling sites.

![biota]
Entering data about sampled organisms.

![creed]
Assisted assessment of data quality using the CREED framework.

<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- ROADMAP -->
## Roadmap

- [ ] Better test architecture and general bug fixes
- [ ] More user-friendly session saving
- [ ] Data extraction from structured formats (e.g. Excel spreadsheets, API calls)
- [ ] Connection to the Risk Assessment database for lookups and long-term storage
- [ ] Support for more formats

See the [open issues](https://github.com/sawelch-NIVA/STOPeData/issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- CONTACT -->
## Contact

Sam Welch - sam.welch@niva.no

Project Link: [https://github.com/NIVANorge/STOPeData](https://github.com/sawelch-NIVA/STOPeData)

<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

* Project Lead: Knut Erik Tollefsen
* Funding: EXPECT, PARC, and NCTP Projects
* Testers: Li Xi, Knut Erik Tollefsen, Sophie Mentzel, Pierre Blévin, Camden Karon Klefbom
* Support and Advice: Viviane Giradin, Andrea Merlina, Kim Leirvik, Jemmima Knight, Malcolm Reid
* An LLM (Anthropic Claude Sonnet 4.5) was used in the creation of this app and its code.
* Readme template repo: [Best-README-Template](https://github.com/othneildrew/Best-README-Template/blob/main/BLANK_README.md)
* (If I've left you off please let me know!)

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[workflow]: images/efve.png
[sites]: images/mod_sites_screenshot.png
[creed]: images/mod_creed_screenshot.png
[biota]: images/mod_biota_screenshot.png
[complicated_workflow]: inst/app/www/app_mapp.png
