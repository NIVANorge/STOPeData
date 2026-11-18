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
get_parameters_filtered(
  "Stressor",
  "Homogeneous metal compounds",
  eDataDRF::parameters_vocabulary()
)
#>  [1] "(~140~Ce)Cerium"                "(~153~Eu)Europium"             
#>  [3] "(~172~Yb)Ytterbium"             "Actinium"                      
#>  [5] "Aluminum"                       "Americium"                     
#>  [7] "Americium 241"                  "Antimony"                      
#>  [9] "Arsenic"                        "Arsenic (III)"                 
#> [11] "Arsenic ion (5+)"               "Arsenide"                      
#> [13] "Barium"                         "Boron"                         
#> [15] "Cadmium"                        "Caesium-137"                   
#> [17] "Cerium"                         "Cesium"                        
#> [19] "Chromium"                       "Chromium (III)"                
#> [21] "Chromium (VI) ion"              "Cobalt"                        
#> [23] "Copper"                         "Copper(2+)"                    
#> [25] "Curium"                         "Dysprosium"                    
#> [27] "Erbium"                         "Erbium, isotope of mass 166"   
#> [29] "Europium"                       "Fe +2 ion"                     
#> [31] "Fe+3 ion"                       "Gadolinium"                    
#> [33] "Gallium"                        "Germanium"                     
#> [35] "Gold"                           "Hafnium"                       
#> [37] "Holmium"                        "Indium"                        
#> [39] "Iridium"                        "Iron"                          
#> [41] "Lanthanum"                      "Lead(2+) ion"                  
#> [43] "Lutetium"                       "Magnesium"                     
#> [45] "Manganese"                      "Mercury"                       
#> [47] "Molybdenum"                     "Neodymium"                     
#> [49] "Neodymium, isotope of mass 146" "Neptunium"                     
#> [51] "Nickel"                         "Niobium"                       
#> [53] "Osmium"                         "Palladium"                     
#> [55] "Platinum"                       "Plutonium"                     
#> [57] "Plutonium 240"                  "Polonium"                      
#> [59] "Potassium"                      "Praseodymium"                  
#> [61] "Radium"                         "Radium-228"                    
#> [63] "Rhenium"                        "Rhodium"                       
#> [65] "Ruthenium"                      "Samarium"                      
#> [67] "Scandium"                       "Silicon"                       
#> [69] "Silver"                         "Strontium-90"                  
#> [71] "Tantalum"                       "Technetium"                    
#> [73] "Tellurium"                      "Terbium"                       
#> [75] "Thallic cation"                 "Thallium"                      
#> [77] "Thorium"                        "Thorium 230"                   
#> [79] "Thulium"                        "Tin"                           
#> [81] "Titanium"                       "Tungsten"                      
#> [83] "Uranium"                        "Uranium 234"                   
#> [85] "Uranium-235"                    "Vanadium"                      
#> [87] "Ytterbium"                      "Yttrium"                       
#> [89] "Zinc"                           "Zirconium"                     
get_parameters_filtered("Chemical", dummy_parameters = eDataDRF::parameters_vocabulary())
#> character(0)
```
