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

# Location of the code list to check
codelist_checking <- readxl::read_excel("HTN/HTN Rx codes.xlsx") %>%
  pull("Stata Code") %>%
  str_remove_all(., "\\.") %>%
  .[. != ""]

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

# VALIDATION LISTS ------------------------------------------------------------

# Import the testing code lists and pull out the BNF code column
# Then split that column based on the presence of slashes
# Then remove empty values (.[. !=""])
# Ensure each BNF code is 8 characters long (add leading zero)

res_24_bnf <- 
  c(
    "HTN/Drug Validation List/res24-beta_blockers.csv",
    "HTN/Drug Validation List/res24-ca_channel_blockers.csv",
    "HTN/Drug Validation List/res24-diuretics.csv",
    "HTN/Drug Validation List/res24-other_antihypertensives.csv") %>% 
  map_df(~read_csv(.)) %>% 
  pull(bnfcode) %>%
  str_split(., pattern = "/", simplify = TRUE) %>%
  .[. != ""]


# Import RES-56 and RES-72 and pull out the product code column
# Then use the CPRD list to get BNF chapters
# There are more BNF chapters than product codes as some drugs have >1 BNF chapter

res_prod_code <-
  c("HTN/Drug Validation List/res56-antihypertensive-drugs.csv",
    "HTN/Drug Validation List/res72-antihypertensives.csv") %>% 
  map_df(~read_csv(.)) %>% 
  pull(code) 

res_bnf <- cprd_gold %>%
  filter(prodcode %in% res_prod_code) %>%
  pull(bnfchapter) %>%
  str_split(., pattern = "/", simplify = TRUE) %>%
  .[. != ""] 


# Import LSHTM codelist 874 and 2188
# Then use the CPRD list to get BNF chapters
# There are more BNF chapters than product codes as some drugs have >1 BNF chapter

lshtm_prod_code <-
  c(
    "HTN/Drug Validation List/LSHTM_874_Therapy_codelist_antihypertensives.txt",
    "HTN/Drug Validation List/LSTHM_2188_antihypertensives_gold_jul18.txt") %>% 
  map_df(~read_csv(.)) %>%
  pull(prodcode)

lshtm_bnf <- cprd_gold %>%
  filter(prodcode %in% lshtm_prod_code) %>%
  pull(bnfchapter) %>%
  str_split(., pattern = "/", simplify = TRUE) %>%
  .[. != ""] 

# CREATE A BNF CODE LIST --------------------------------------------------

# Create a overall BNF list to check against
# Keep only distinct BNF chapters
# Remove BNF chapter 00000000 (i.e. keep only BNF chapters >1)
# Remove BNF chapters starting with 11 (eye drops)
# Convert this to a vector and add leading zeros
codelist_validation <-
  data.frame(
    bnf_chap = as.numeric(c(
      res_24_bnf,
      res_bnf,
      lshtm_bnf))) %>%
  distinct(bnf_chap) %>%
  filter(bnf_chap >1) %>%
  filter(!str_starts(bnf_chap, "11")) %>%
  pull(bnf_chap) %>%
  formatC(., width = 8, format = "d", flag = "0")


# CHECK CODELIST ----------------------------------------------------------

# Cross check the list imported against the biobank BNF code list
# Then filter out Read subcodes and pull out the remaining codes
# Then check if these codes don't exist in the codelist_checking vector

missing_codes <-
  biobank_bnf_read %>%
  filter(bnfcode_new %in% codelist_validation) %>%
  func_remove_subcodes("readcode_new") %>%
  pull(readcode_new) %>%
  .[!(. %in% codelist_checking)]

missing_codes

# Decode missing codes 
biobank_read_lookup %>%
  filter(read_new %in% missing_codes) %>%
  select(read_new, term_description) %>%
  print(n=999) 

#Count how many translated read V2 codes to expect back
missing_codes %>%
  str_starts("X", negate=T) %>%
  sum()
