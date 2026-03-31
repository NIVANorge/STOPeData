# Generate Protocol ID —-

Generates a standardised protocol identifier based on protocol type,
name, sequence number, and campaign. The function is fully vectorised
and can handle multiple protocols simultaneously.

## Usage

``` r
generate_protocol_id(
  protocol_type,
  protocol_name,
  sequence_number = 1,
  campaign_name = ""
)
```

## Arguments

- protocol_type:

  Character vector. The category of protocol (e.g., "Sampling Protocol",
  "Analytical Protocol"). Must match one of the predefined categories or
  will default to "X".

- protocol_name:

  Character vector. The specific name/method within the protocol
  category. Will be abbreviated and cleaned for ID generation.

- sequence_number:

  Numeric vector. Sequential number for protocols within the same
  category. Defaults to 1. Will be zero-padded to 2 digits.

- campaign_name:

  Character vector. Name of the campaign/study. Will be abbreviated to
  first 10 alphanumeric characters. Defaults to "".

## Value

Character vector of generated protocol identifiers, one per input
element, formatted as TypeCode + SequenceNumber + AbbreviatedName +
CampaignAbbrev. Where: - TypeCode: S (Sampling), F (Fractionation), E
(Extraction), A (Analytical), X (Unknown) - SequenceNumber: Zero-padded
2-digit number - AbbreviatedName: Up to 15 alphanumeric characters
(optional) - CampaignAbbrev: Up to 10 alphanumeric characters (optional)

## Details

The function handles edge cases gracefully: - NULL or empty values
result in appropriate defaults - Invalid protocol types default to "X" -
Names are cleaned of special characters and spaces - Empty components
are omitted from the final ID

## Examples

``` r
generate_protocol_id("Sampling Protocol", "Water grab sampling", sequence_number = 1)
#> [1] "S01_WaterGrabSampli"
generate_protocol_id("Analytical Protocol", "ICP-MS", sequence_number = 2, campaign_name = "NorthSea2022")
#> [1] "A02_Icpms_NorthSea20"
```
