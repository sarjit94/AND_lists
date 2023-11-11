##### LOAD LIBRARIES / SETUP R ----------------------------------------------
# Clear R 
rm(list=ls())
graphics.off()

library(tidyverse)


# CREATE FUNCTIONS --------------------------------------------------------
# Create a function to remove subcodes (eg B22 if B2 exists in list)
remove_subcodes <- function(df, code_column) {
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


##### IMPORT KNOWN DATABASES  -----------------------------------------------
# Location of the code list to check
# As importing, create new variables with periods removed (easier to work with)
cl_checking <- readxl::read_excel("HTN/HTN Rx codes.xlsx") %>%
  pull(Code) %>%
  str_remove_all(., "\\.")

# Import BioBank conversion table
# As importing, create new variables with periods removed (easier to work with)
biobank <- 
  readxl::read_excel("BioBank/biobank.xlsx", sheet = "read_v2_drugs_bnf") %>%
  mutate(
    readcode_new = str_remove_all(read_code, "\\."),
    bnfcode_new = str_remove_all(bnf_code, "\\."))

# Import BioBank's Read code lookup table
read_lookup <- 
  readxl::read_excel("BioBank/biobank.xlsx", sheet = "read_v2_lkp") %>%
  mutate(
    read_code = str_remove_all(read_code, "\\."))


# FIRST VALIDATION LIST ---------------------------------------------------
# Import the testing code list and pull out the BNF code column
# Then split that column based on the presence of slashes
# Then remove empty values (.[. !=""])
res24_beta_blockers <-
  read_csv("HTN/Validation List/res24-beta_blockers.csv") %>%
  pull(bnfcode) %>%
  str_split(., pattern = "/", simplify = TRUE) %>%
  .[. != ""]


# Cross check the list imported againt the biobank BNF code list
# Then remove Read subcodes and pull out these values
# Then check if these codes don't exist in the cl_checking vector
temp <- biobank %>%
  filter(bnfcode_new %in% res24_beta_blockers) %>%
  remove_subcodes("readcode_new") %>%
  pull(readcode_new) %>%
  .[!(. %in% cl_checking)]



