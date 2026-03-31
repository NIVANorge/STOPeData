# Create Dummy Session Data

Creates a complete userData-like list structure populated with dummy
data from all dummy\_\*\_tibble() functions. This mirrors the structure
created by initialise_userData() but with test data instead of empty
tibbles.

Useful for testing outside of a reactive context.

## Usage

``` r
create_dummy_session_data()
```

## Value

A list matching the structure of initialise_userData() with dummy data

## See also

Other create:
[`create_compartment_combination()`](https://nivanorge.github.io/STOPeData/reference/create_compartment_combination.md),
[`create_existing_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_existing_parameter.md),
[`create_new_parameter()`](https://nivanorge.github.io/STOPeData/reference/create_new_parameter.md),
[`create_new_site()`](https://nivanorge.github.io/STOPeData/reference/create_new_site.md)

## Examples

``` r
session_data <- create_dummy_session_data()
names(session_data)
#>  [1] "ENTERED_BY"               "sitesData"               
#>  [3] "sitesDataValid"           "parametersData"          
#>  [5] "parametersDataValid"      "compartmentsData"        
#>  [7] "compartmentsDataValid"    "referenceData"           
#>  [9] "referenceDataValid"       "campaignData"            
#> [11] "campaignDataValid"        "methodsData"             
#> [13] "methodsDataValid"         "samplesData"             
#> [15] "samplesDataValid"         "biotaData"               
#> [17] "biotaDataValid"           "samplesDataWithBiota"    
#> [19] "measurementsData"         "measurementsDataValid"   
#> [21] "datasetDetails"           "creedRelevance"          
#> [23] "creedReliability"         "creedScores"             
#> [25] "creedReport"              "creedGetData"            
#> [27] "creedCalculateScores"     "schemaLLM"               
#> [29] "promptLLM"                "rawLLM"                  
#> [31] "pdfPath"                  "campaignDataLLM"         
#> [33] "referenceDataLLM"         "sitesDataLLM"            
#> [35] "parametersDataLLM"        "compartmentsDataLLM"     
#> [37] "methodsDataLLM"           "samplesDataLLM"          
#> [39] "biotaDataLLM"             "samplesDataLLM"          
#> [41] "llmExtractionComplete"    "llmExtractionSuccessful" 
#> [43] "llmPopulateModules"       "llmExtractionComments"   
#> [45] "saveExtractionComplete"   "saveExtractionSuccessful"
#> [47] "creedReliabilityValid"    "creedRelevanceValid"     
nrow(session_data$sitesData)
#> [1] 2
```
