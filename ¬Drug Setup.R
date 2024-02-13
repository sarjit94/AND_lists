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

# As importing, create new variables with periods removed (easier to work with)

# Import BioBank read conversion table
biobank_bnf_read <- 
  readxl::read_excel("Ref/BioBank/biobank.xlsx", sheet = "read_v2_drugs_bnf") %>%
  mutate(
    readcode_new = str_remove_all(read_code, "\\."),
    bnfcode_new = str_remove_all(bnf_code, "\\."))

# Import BioBank OPCS conversion table
biobank_opcs_read <- 
  readxl::read_excel("Ref/BioBank/biobank.xlsx", sheet = "read_ctv3_opcs4") %>%
  mutate(
    readcode_new = str_remove_all(read_code, "\\."),
    opcs4_new = str_remove_all(opcs4_code, "\\."))

# Import BioBank's read to drug name table
biobank_read_lookup <- 
  readxl::read_excel("Ref/BioBank/biobank.xlsx", sheet = "read_v2_drugs_lkp") %>%
  mutate(read_new = str_remove_all(read_code, "\\."))

# Import the CPRD gold and aurum conversion tables 
cprd_gold <- readr::read_tsv("Ref/CPRD/product.txt") 
cprd_aurum <- readr::read_tsv("Ref/CPRD/CPRDAurumProduct.txt")

# Import SNOMED to read conversions
ndm_read_snomed <-
  c(
    "Ref/NHS Data Migration/Not Clinically Assured/rcmap_uk_20200401000001.txt",
    "Ref/NHS Data Migration/Not Clinically Assured/rcsctmap_enhanced_uk_20200401000001.txt",
    "Ref/NHS Data Migration/Not Clinically Assured/rcsctmap_uk_20200401000001.txt",
    "Ref/NHS Data Migration/Not Clinically Assured/rctermsctmap_uk_20200401000001.txt"
  ) %>%
  map_df( ~ read_tsv(.)) %>%
  select(ReadCode, ConceptId) %>%
  distinct(ConceptId, .keep_all = T) %>%
  mutate(readcode_new = str_remove_all(ReadCode, "\\."))

