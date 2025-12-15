#' @title Base Data Table R6 Generator
#'
#' @description
#' Abstract base class for all eData table types providing standardised structure,
#' validation, and metadata management. This [R6][R6::R6Class] class bundles:
#' * A tibble containing the actual data
#' * A schema defining column names, types, and mandatory status
#' * Validation methods to ensure data integrity
#' * Active bindings for querying metadata
#'
#' Child classes must define in their private section:
#' * `schema`: A [tibble][tibble::tibble] with columns (`column_name`, `data_type`, `mandatory`, `description`)
#' * `schema_version`: A version string following semantic versioning (e.g., `"1.0.0"`)
#'
#' @importFrom R6 R6Class
#' @importFrom tibble tibble as_tibble tribble
#' @importFrom purrr map2 set_names
#' @importFrom dplyr filter bind_rows pull
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # This is an abstract base class - use child classes instead
#' # See CampaignData_Table_R6_Generator for concrete implementation
#' }
Data_Table_R6_Generator <- R6Class(
  "Data_Table_R6_Generator",

  # Must be defined in child classes ----
  private = list(
    #' @field schema (`tbl_df`)\cr
    #' Tibble defining table structure with columns: column_name, data_type, mandatory, description.
    #' Must be defined in child classes.
    schema = NULL,

    #' @field schema_version (`character(1)`)\cr
    #' Semantic version string (e.g., `"1.0.0"`).
    #' Must be defined in child classes.
    schema_version = NULL,

    # Build empty tibble from schema ----
    #' @description
    #' Build an empty tibble with appropriate column types based on the schema.
    #' This is an internal method called during initialization.
    #'
    #' @return (`tbl_df`)\cr
    #' An empty tibble with columns defined by the schema.
    build_empty_tibble = function() {
      schema <- private$schema

      # Create appropriately-typed empty columns
      cols <- map2(schema$column_name, schema$data_type, function(name, type) {
        switch(
          type,
          "character" = character(),
          "Date" = as.Date(character()),
          "numeric" = numeric(),
          "integer" = integer(),
          "logical" = logical(),
          stop("Unknown data type: ", type)
        )
      })

      set_names(cols, schema$column_name) |>
        as_tibble()
    }
  ),

  # Available to all child classes ----
  public = list(
    #' @field data (`tbl_df`)\cr
    #' The actual data tibble. Can be modified directly but prefer using `$add_rows()`
    #' for automatic validation.
    data = NULL,

    # Build empty tibble on creation ----
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' Initializes an empty data tibble based on the schema defined in the child class.
    #' This method is called automatically when using `$new()`.
    #'
    #' Note that this object is typically constructed via derived classes,
    #' e.g., [CampaignData_Table_R6_Generator].
    initialize = function() {
      if (is.null(private$schema)) {
        stop("Schema must be defined in child class")
      }
      if (is.null(private$schema_version)) {
        stop("Schema version must be defined in child class")
      }

      self$data <- private$build_empty_tibble()
    },

    # Add rows with validation ----
    #' @description
    #' Add rows to the data tibble with validation.
    #' Validates that all mandatory columns are present and that column types match
    #' the schema. Returns self invisibly to allow method chaining.
    #'
    #' @param new_rows (`tbl_df`)\cr
    #'   A tibble containing the rows to add. Must include all mandatory columns
    #'   and match the schema's column types.
    #'
    #' @return Self, invisibly.
    add_rows = function(new_rows) {
      # Check: All mandatory columns present?
      mandatory <- self$mandatory_fields
      missing <- setdiff(mandatory, names(new_rows))
      if (length(missing) > 0) {
        stop("Missing mandatory columns: ", paste(missing, collapse = ", "))
      }

      # Check: Column types match?
      for (col in names(new_rows)) {
        if (col %in% names(self$data)) {
          expected_type <- class(self$data[[col]])[1]
          actual_type <- class(new_rows[[col]])[1]

          if (expected_type != actual_type) {
            stop(paste0(
              "Type mismatch for '",
              col,
              "': expected ",
              expected_type,
              ", got ",
              actual_type
            ))
          }
        }
      }

      # All good - add rows
      self$data <- bind_rows(self$data, new_rows)
      invisible(self)
    },

    # Validate structure matches schema ----
    #' @description
    #' Validate that the current data structure matches the schema.
    #' Checks that column names are correct and in the right order, and that
    #' column types match expectations. Stops with an error if validation fails.
    #'
    #' @return (`logical(1)`)\cr
    #' Returns `TRUE` if validation passes. Stops with error if validation fails.
    validate_structure = function() {
      schema <- private$schema

      # Check column names
      if (!identical(names(self$data), schema$column_name)) {
        stop("Column names don't match schema")
      }

      # Check data types
      for (i in seq_along(schema$column_name)) {
        col_name <- schema$column_name[i]
        expected_type <- schema$data_type[i]
        actual_type <- class(self$data[[col_name]])[1]

        if (actual_type != expected_type) {
          stop(paste0(
            "Type mismatch for '",
            col_name,
            "': expected ",
            expected_type,
            ", got ",
            actual_type
          ))
        }
      }

      TRUE
    }
  ),

  # Computed properties ----
  active = list(
    #' @field metadata (`tbl_df`)\cr
    #' Returns the schema as a tibble. Read-only.
    metadata = function() {
      private$schema
    },

    #' @field version (`character(1)`)\cr
    #' Returns the schema version string. Read-only.
    version = function() {
      private$schema_version
    },

    #' @field mandatory_fields (`character()`)\cr
    #' Returns a character vector of mandatory column names. Read-only.
    mandatory_fields = function() {
      private$schema |>
        filter(mandatory == TRUE) |>
        pull(column_name)
    },

    #' @field optional_fields (`character()`)\cr
    #' Returns a character vector of optional column names. Read-only.
    optional_fields = function() {
      private$schema |>
        filter(mandatory == FALSE) |>
        pull(column_name)
    },

    #' @field n_rows (`integer(1)`)\cr
    #' Returns the number of rows in the data tibble. Read-only.
    n_rows = function() {
      nrow(self$data)
    }
  )
)


#' @title Campaign Data Table R6 Generator
#'
#' @description
#' [R6][R6::R6Class] class for managing campaign metadata in the eData format.
#' Campaigns represent sampling projects or studies with metadata about timing,
#' organization, data quality evaluation, and confidentiality.
#'
#' This class inherits from [Data_Table_R6_Generator] and provides:
#' * Standardised column structure for campaign data
#' * Validation of mandatory fields
#' * Type checking for all columns
#' * Metadata access via active bindings
#'
#' Mandatory fields: `CAMPAIGN_NAME_SHORT`, `CAMPAIGN_NAME`, `CAMPAIGN_START_DATE`,
#' `ORGANISATION`, `ENTERED_BY`, `ENTERED_DATE`.
#'
#' @section Schema Version:
#' Current version: 1.0.0
#'
#' @section Column Definitions:
#' \describe{
#'   \item{`CAMPAIGN_NAME_SHORT`}{`character`, mandatory. Short campaign identifier.}
#'   \item{`CAMPAIGN_NAME`}{`character`, mandatory. Full campaign name.}
#'   \item{`CAMPAIGN_START_DATE`}{`Date`, mandatory. Campaign start date.}
#'   \item{`CAMPAIGN_END_DATE`}{`Date`, optional. Campaign end date.}
#'   \item{`RELIABILITY_SCORE`}{`character`, optional. Data reliability score.}
#'   \item{`RELIABILITY_EVAL_SYS`}{`character`, optional. Reliability evaluation system.}
#'   \item{`CONFIDENTIALITY_EXPIRY_DATE`}{`Date`, optional. When data becomes public.}
#'   \item{`ORGANISATION`}{`character`, mandatory. Responsible organisation.}
#'   \item{`ENTERED_BY`}{`character`, mandatory. Data entry person.}
#'   \item{`ENTERED_DATE`}{`Date`, mandatory. Data entry date.}
#'   \item{`CAMPAIGN_COMMENT`}{`character`, optional. Additional comments.}
#' }
#'
#' @section Inherited Methods:
#' See [Data_Table_R6_Generator] for details on:
#' * `$initialize()`
#' * `$add_rows(new_rows)`
#' * `$validate_structure()`
#'
#' @section Inherited Active Bindings:
#' See [Data_Table_R6_Generator] for details on:
#' * `$metadata`
#' * `$version`
#' * `$mandatory_fields`
#' * `$optional_fields`
#' * `$n_rows`
#'
#' @export
CampaignData_Table_R6_Generator <- R6Class(
  "CampaignData_Table_R6_Generator",
  inherit = Data_Table_R6_Generator,

  private = list(
    schema_version = "1.0.0",

    # This function gets called during initialize(), not at class definition
    get_schema = function() {
      get_campaign_schema()$schema
    }
  ),

  public = list(
    initialize = function() {
      private$schema <- private$get_schema()
      super$initialize()
    }
  )
)
# ---- Reference Data Table R6 Generator ----

#' @title Reference Data Table R6 Generator
#' @description R6 class for managing reference metadata
#' @export
ReferenceData_Table_R6_Generator <- R6Class(
  "ReferenceData_Table_R6_Generator",
  inherit = Data_Table_R6_Generator,

  private = list(
    schema_version = "1.0.0",

    # This function gets called during initialize(), not at class definition
    get_schema = function() {
      get_reference_schema()$schema
    }
  ),

  public = list(
    initialize = function() {
      private$schema <- private$get_schema()
      super$initialize()
    }
  )
)

# ---- Sites Data Table R6 Generator ----

#' @title Sites Data Table R6 Generator
#' @description R6 class for managing site metadata
#' @export
SitesData_Table_R6_Generator <- R6Class(
  "SitesData_Table_R6_Generator",
  inherit = Data_Table_R6_Generator,

  private = list(
    schema_version = "1.0.0",

    # This function gets called during initialize(), not at class definition
    get_schema = function() {
      get_sites_schema()$schema
    }
  ),

  public = list(
    initialize = function() {
      private$schema <- private$get_schema()
      super$initialize()
    }
  )
)

# ---- Biota Data Table R6 Generator ----

#' @title Biota Data Table R6 Generator
#' @description R6 class for managing biota metadata
#' @export
BiotaData_Table_R6_Generator <- R6Class(
  "BiotaData_Table_R6_Generator",
  inherit = Data_Table_R6_Generator,

  private = list(
    schema_version = "1.0.0",

    # This function gets called during initialize(), not at class definition
    get_schema = function() {
      get_biota_schema()$schema
    }
  ),

  public = list(
    initialize = function() {
      private$schema <- private$get_schema()
      super$initialize()
    }
  )
)

# ---- Samples Data Table R6 Generator ----

#' @title Samples Data Table R6 Generator
#' @description R6 class for managing sample metadata
#' @export
SamplesData_Table_R6_Generator <- R6Class(
  "SamplesData_Table_R6_Generator",
  inherit = Data_Table_R6_Generator,

  private = list(
    schema_version = "1.0.0",

    # This function gets called during initialize(), not at class definition
    get_schema = function() {
      get_samples_schema()$schema
    }
  ),

  public = list(
    initialize = function() {
      private$schema <- private$get_schema()
      super$initialize()
    }
  )
)

# ---- Parameters Data Table R6 Generator ----

#' @title Parameters Data Table R6 Generator
#' @description R6 class for managing parameter metadata
#' @export
ParametersData_Table_R6_Generator <- R6Class(
  "ParametersData_Table_R6_Generator",
  inherit = Data_Table_R6_Generator,

  private = list(
    schema_version = "1.0.0",

    # This function gets called during initialize(), not at class definition
    get_schema = function() {
      get_parameters_schema()$schema
    }
  ),

  public = list(
    initialize = function() {
      private$schema <- private$get_schema()
      super$initialize()
    }
  )
)

# ---- Methods Data Table R6 Generator ----

#' @title Methods Data Table R6 Generator
#' @description R6 class for managing methods metadata
#' @export
MethodsData_Table_R6_Generator <- R6Class(
  "MethodsData_Table_R6_Generator",
  inherit = Data_Table_R6_Generator,

  private = list(
    schema_version = "1.0.0",

    # This function gets called during initialize(), not at class definition
    get_schema = function() {
      get_methods_schema()$schema
    }
  ),

  public = list(
    initialize = function() {
      private$schema <- private$get_schema()
      super$initialize()
    }
  )
)

# ---- Compartments Data Table R6 Generator ----

#' @title Compartments Data Table R6 Generator
#' @description R6 class for managing compartment metadata
#' @export
CompartmentsData_Table_R6_Generator <- R6Class(
  "CompartmentsData_Table_R6_Generator",
  inherit = Data_Table_R6_Generator,

  private = list(
    schema_version = "1.0.0",

    # This function gets called during initialize(), not at class definition
    get_schema = function() {
      get_compartments_schema()$schema
    }
  ),

  public = list(
    initialize = function() {
      private$schema <- private$get_schema()
      super$initialize()
    }
  )
)

# ---- Measurements Data Table R6 Generator ----

#' @title Measurements Data Table R6 Generator
#' @description R6 class for managing measurement data
#' @export
MeasurementsData_Table_R6_Generator <- R6Class(
  "MeasurementsData_Table_R6_Generator",
  inherit = Data_Table_R6_Generator,

  private = list(
    schema_version = "1.0.0",

    # This function gets called during initialize(), not at class definition
    get_schema = function() {
      get_measurements_schema()$schema
    }
  ),

  public = list(
    initialize = function() {
      private$schema <- private$get_schema()
      super$initialize()
    }
  )
)

# ============================================================================
# CREED R6 Generators (add to fct_R6.R)
# ============================================================================

# ---- CREED Scores Data Table R6 Generator ----

#' @title CREED Scores Data Table R6 Generator
#' @description R6 class for managing CREED scores metadata
#' @export
CREEDScoresData_Table_R6_Generator <- R6Class(
  "CREEDScoresData_Table_R6_Generator",
  inherit = Data_Table_R6_Generator,

  private = list(
    schema_version = "1.0.0",

    get_schema = function() {
      get_creed_scores_schema()$schema
    }
  ),

  public = list(
    initialize = function() {
      private$schema <- private$get_schema()
      super$initialize()
    }
  )
)

# ---- CREED Data Table R6 Generator ----

#' @title CREED Data Table R6 Generator
#' @description R6 class for managing CREED criterion data (internal structure)
#' @export
CREEDData_Table_R6_Generator <- R6Class(
  "CREEDData_Table_R6_Generator",
  inherit = Data_Table_R6_Generator,

  private = list(
    schema_version = "1.0.0",

    get_schema = function() {
      get_creed_data_schema()$schema
    }
  ),

  public = list(
    initialize = function() {
      private$schema <- private$get_schema()
      super$initialize()
    }
  )
)
