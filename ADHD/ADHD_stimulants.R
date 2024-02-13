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



# IMPORT COVNERSION DATABASES  ------------------------------------------------

# As importing, create new variables with periods removed (easier to work with)

# Import BioBank read conversion table
biobank_bnf_read <- 
  readxl::read_excel("Ref/BioBank/biobank.xlsx", sheet = "read_v2_drugs_bnf") %>%
  mutate(
    readcode_new = str_remove_all(read_code, "\\."),
    bnfcode_new = str_remove_all(bnf_code, "\\."))

# Import BioBank's read to drug name table
biobank_read_lookup <- 
  readxl::read_excel("Ref/BioBank/biobank.xlsx", sheet = "read_v2_drugs_lkp") %>%
  mutate(read_new = str_remove_all(read_code, "\\."))


# CREATE A BNF CODE LIST --------------------------------------------------

# Create a list of read codes that reference BNF chapter 0404
# Remove subcodes
# Pull out the read codes
adhd_0404 <- biobank_bnf_read %>%
  filter(str_starts(bnfcode_new, "0403")) %>%
  func_remove_subcodes("readcode_new") %>%
  pull(readcode_new)


# Convert read codes to read code and term
biobank_read_lookup %>%
  filter(read_new %in% adhd_0404) %>%
  select(read_new, term_description) %>%
  print(n=999)


# Check how many codes should have been converted to terms
adhd_0404 %>%
  str_starts("X", negate=T) %>%
  sum()
