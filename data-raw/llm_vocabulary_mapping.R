# LLM vocabulary mapping table
#
# Source: Manually curated by the STOPeData package maintainer.
#
# Purpose: Maps free-text values extracted by an LLM from scientific papers
# to controlled vocabulary terms used in eDataDRF. The table contains regular
# expression patterns (column `pattern`) and their corresponding standardised
# output values (column `value`) grouped by field (column `field`).
#
# Fields covered:
#   parameter_type, compartment, compartment_sub, geographic_feature
#
# The file lives in inst/extdata/clean/ so it can be accessed at runtime
# via system.file() without being lazy-loaded into the package namespace.
#
# To update: edit inst/extdata/clean/llm_vocabulary_mapping.csv directly.
# No R transformation is needed — the CSV is the authoritative source.
