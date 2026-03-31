# Get parameter names filtered by type and optionally subtype

Returns a character vector of parameter names filtered by type and
optionally by subtype, combining base parameters with session-specific
parameters.

## Usage

``` r
get_parameters_filtered(
  param_type,
  param_subtype = "Show all",
  dummy_parameters,
  session_parameters = NULL
)
```

## Arguments

- param_type:

  Character string specifying the parameter type. Must be a value from
  [`eDataDRF::parameter_types_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameter_types_vocabulary.html).

- param_subtype:

  Character string specifying the parameter subtype (optional). Must be
  a value from
  [`eDataDRF::parameter_types_sub_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameter_types_sub_vocabulary.html).

- dummy_parameters:

  Dataframe containing base parameters. Use
  [`eDataDRF::parameters_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameters_vocabulary.html)
  to obtain the full parameter list.

- session_parameters:

  Optional list containing session-specific parameters

## Value

Character vector of parameter names

## See also

[`eDataDRF::parameters_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameters_vocabulary.html),
[`eDataDRF::parameter_types_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameter_types_vocabulary.html),
[`eDataDRF::parameter_types_sub_vocabulary()`](https://NIVANorge.github.io/eDataDRF/reference/parameter_types_sub_vocabulary.html)

## Examples

``` r
get_parameters_filtered("Stressor", "Homogeneous metal compounds", eDataDRF::parameters_vocabulary())
#>  [1] "Actinium"          "Aluminum"          "Americium"        
#>  [4] "Antimony"          "Arsenic"           "Arsenic (III)"    
#>  [7] "Arsenic ion (5+)"  "Arsenide"          "Barium"           
#> [10] "Boron"             "Cadmium"           "Cerium"           
#> [13] "Cesium"            "Chromium"          "Chromium (III)"   
#> [16] "Chromium (VI) ion" "Cobalt"            "Copper"           
#> [19] "Copper(2+)"        "Curium"            "Dysprosium"       
#> [22] "Erbium"            "Europium"          "Gadolinium"       
#> [25] "Gallium"           "Germanium"         "Gold"             
#> [28] "Hafnium"           "Holmium"           "Indium"           
#> [31] "Iridium"           "Iron"              "Lanthanum"        
#> [34] "Lead(2+) ion"      "Lutetium"          "Magnesium"        
#> [37] "Manganese"         "Mercury"           "Molybdenum"       
#> [40] "Neodymium"         "Neptunium"         "Nickel"           
#> [43] "Niobium"           "Osmium"            "Palladium"        
#> [46] "Platinum"          "Plutonium"         "Polonium"         
#> [49] "Potassium"         "Praseodymium"      "Radium"           
#> [52] "Rhenium"           "Rhodium"           "Ruthenium"        
#> [55] "Samarium"          "Scandium"          "Silicon"          
#> [58] "Silver"            "Tantalum"          "Technetium"       
#> [61] "Tellurium"         "Terbium"           "Thallic cation"   
#> [64] "Thallium"          "Thorium"           "Thulium"          
#> [67] "Tin"               "Titanium"          "Tungsten"         
#> [70] "Uranium"           "Vanadium"          "Ytterbium"        
#> [73] "Yttrium"           "Zinc"              "Zirconium"        
get_parameters_filtered("Chemical", dummy_parameters = eDataDRF::parameters_vocabulary())
#> character(0)
```
