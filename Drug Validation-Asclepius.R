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



# IMPORT KNOWN DATABASES  -----------------------------------------------------

# Location of the code list to check
# As importing, create new variables with periods removed (easier to work with)
validation_codelist <- 
  readxl::read_excel("HTN/HTN Rx codes.xlsx") %>%
  pull(Code) %>%
  str_remove_all(., "\\.")

# Import BioBank read conversion table
# As importing, create new variables with periods removed (easier to work with)
biobank_bnf_read <- 
  readxl::read_excel("BioBank/biobank.xlsx", sheet = "read_v2_drugs_bnf") %>%
  mutate(
    readcode_new = str_remove_all(read_code, "\\."),
    bnfcode_new = str_remove_all(bnf_code, "\\."))

# Import BioBank OPCS conversion table
# As importing, create new variables with periods removed (easier to work with)
biobank_opcs_read <- 
  readxl::read_excel("BioBank/biobank.xlsx", sheet = "read_ctv3_opcs4") %>%
  mutate(
    readcode_new = str_remove_all(read_code, "\\."),
    opcs4_new = str_remove_all(opcs4_code, "\\."))



# IMPORT VALIDATION CODE LISTS ------------------------------------------------

# Import the testing code lists and pull out the BNF code column
# Then split that column based on the presence of slashes
# Then remove empty values (.[. !=""])

res_6 <-
  c("HTN/Drug Validation List/res6-ace_inhibitors.csv",
    "HTN/Drug Validation List/res6-ace-inhibitors.csv",
    "HTN/Drug Validation List/res6-angiotensin-ii-receptor-antagonists.csv") %>%
  map_dfr(read_csv) %>%
  pull(BNF_code) %>%
  .[. != ""] %>%
  as.character()


# Remove slashes from these code lists
res_9 <-
  c("HTN/Drug Validation List/res9-ace_inhibitors.csv",
    "HTN/Drug Validation List/res9-beta_blockers.csv",
    "HTN/Drug Validation List/res9-ca_channel_blockers.csv",
    "HTN/Drug Validation List/res9-diuretics_thiazide.csv",
    "HTN/Drug Validation List/res9-loop_diuretics.csv") %>%
  map_dfr(read_csv) %>%
  pull(bnfcode) %>%
  str_split(., pattern = "/", simplify = TRUE) %>%
  .[. != ""]


# Remove slashes from these code lists
res_24 <-
  c("HTN/Drug Validation List/res24-beta_blockers.csv",
    "HTN/Drug Validation List/res24-ca_channel_blockers.csv",
    "HTN/Drug Validation List/res24-diuretics.csv",
    "HTN/Drug Validation List/res24-other_antihypertensives.csv") %>%
  map_dfr(read_csv) %>%
  pull(bnfcode) %>%
  str_split(., pattern = "/", simplify = TRUE) %>%
  .[. != ""] 




# ALTER THE CODE LISTS



# CHECK CODELIST ----------------------------------------------------------

# Cross check the list imported against the biobank BNF code list
# Then remove Read subcodes and pull out these values
# Then check if these codes don't exist in the validation_codelist vector

missing_codes <- biobank_bnf_read %>%
  filter(bnfcode_new %in% c(
    res_6,
    res_9,
    res_24
    )) %>% 
  func_remove_subcodes("readcode_new") %>%
  pull(readcode_new) %>%
  .[!(. %in% validation_codelist)]

list(missing_codes)





