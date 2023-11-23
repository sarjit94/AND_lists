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
codelist_checking <- readxl::read_excel("HTN/HTN Dx codes.xlsx") %>%
  pull(Code) %>%
  str_remove_all(., "\\.")

# Import NHS TRUD read conversion tables
trud_icd10 <- 
  readr::read_delim("NHS TRUD/read crossmaps/icd10.v3", delim="|", col_names = FALSE) %>%
  transmute(
    read = X1,
    icd10 = X2,
    read_new = str_remove_all(X1, "\\.")
  )

trud_opcs4 <- 
  readr::read_delim("NHS TRUD/read crossmaps/Opcs4.v3", delim="|", col_names = FALSE) %>%
  transmute(
    read = X1,
    opcs4 = X2,
    read_new = str_remove_all(X1, "\\.")
  )


temp <- readr::read_tsv(c(
  #"NHS TRUD/read to snomed/PBCLReadSNOMEDmap20180401.txt"
  "NHS TRUD/read to snomed/PBCLExtended20180401.txt")) 

# Import the CPRD gold and aurum conversion tables 
cprd_gold <- readr::read_tsv("CPRD/medical.txt") 
cprd_aurum <- readr::read_tsv("CPRD/CPRDAurumMedical.txt")



# VALIDATION LISTS ------------------------------------------------------------

# Import the RES-1 and RES-30 lists and pull out the code
# Truncate the codes to 5 length ()

  c(
    "HTN/Dx Validation List/RES_1_hypertension.csv",
    "HTN/Dx Validation List/res30-hypertension-read.csv"
    ) %>%
  map_df(~read_csv(.)) %>%
  pull(code) %>%
  str_trunc(5, side="right") %>%
  str_remove_all("\\.")
  
  
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



nchar(cprd_gold$readcode) %>%
  table()

