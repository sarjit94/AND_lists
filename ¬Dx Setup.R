# LOAD LIBRARIES / SETUP R ----------------------------------------------------

rm(list=ls())
graphics.off()

library(tidyverse)



# CREATE FUNCTIONS ------------------------------------------------------------

# Create a function to remove subcodes (eg B22 if B2 exists in list)
func_remove_subcodes <- function(df, code_column) {
  # Create a vector of unique codes
  codes <- unique(df[[code_column]])
  # Define a function to determine if a code is a subcode of any other
  is_subcode <- function(code, codes) {
    # Check if there are any codes that are a substring of the current code
    any(sapply(codes, function(x) grepl(paste0("^", x), code) && x != code))
  }
  # Filter out subcodes
  filtered_codes <- codes[!sapply(codes, is_subcode, codes = codes)]
  # Filter the original dataframe to keep only rows with the filtered codes
  df_filtered <- df %>% 
    filter(get(code_column) %in% filtered_codes)
  return(df_filtered)
}


# Function to remove subcodes if higher level code exists
func_remove_subcodes2 <- function(codes) {
  # Initialize an empty vector to store the final result
  final_codes <- character(0)
  # Sort the codes to ensure higher-level codes appear first
  sorted_codes <- sort(codes)
  # Loop through each code
  for (code in sorted_codes) {
    # Flag to check if a higher-level code exists
    higher_level_exists <- FALSE
    # Iterate through existing final codes
    for (final_code in final_codes) {
      # Check if the current code is a subcode of any existing code
      if (startsWith(code, final_code)) {
        # A higher-level code exists, set the flag and break the loop
        higher_level_exists <- TRUE
        break
      }
    }
    # If no higher-level code exists, add the code to the final result
    if (!higher_level_exists) {
      final_codes <- c(final_codes, code)
    }
  }
  return(final_codes)
}



# IMPORT COVNERSION DATABASES  ------------------------------------------------
# IMPORT COVNERSION DATABASES  ------------------------------------------------

# As importing, create new variables with periods removed (easier to work with)
# Import NHS TRUD read conversion tables
trud_icd10 <- 
  readr::read_delim("Ref/NHS TRUD/read crossmaps/icd10.v3", delim="|", col_names = FALSE) %>%
  transmute(
    read = X1,
    icd10 = X2,
    read_new = str_remove_all(X1, "\\.")
  )

trud_opcs4 <- 
  readr::read_delim("Ref/NHS TRUD/read crossmaps/Opcs4.v3", delim="|", col_names = FALSE) %>%
  transmute(
    read = X1,
    opcs4 = X2,
    read_new = str_remove_all(X1, "\\.")
  )

trud_snomed <- c(
  "Ref/NHS TRUD/read to snomed/PBCLReadSNOMEDmap20180401.txt", 
  "Ref/NHS TRUD/read to snomed/PBCLExtended20180401.txt") %>% 
  map_df(~readr::read_tsv(.)) %>%
  transmute(
    v2_term = V2_TERM,
    v2_read_code = V2_READ_CODE,
    snomed = SNOMED_CONCEPT_ID,
    read_new = str_remove_all(V2_READ_CODE, "\\.")
  )

# Import the CPRD gold and aurum conversion tables 
cprd_gold <- readr::read_tsv("Ref/CPRD/medical.txt")
cprd_aurum <- readr::read_tsv("Ref/CPRD/CPRDAurumMedical.txt") %>%
  mutate(
    read_new = str_trunc(CleansedReadCode, 5, side = "right", ellipsis = ""),
    read_new = str_remove_all(read_new, "\\.")
  )
