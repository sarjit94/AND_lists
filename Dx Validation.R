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


trud_snomed <- c(
  "NHS TRUD/read to snomed/PBCLReadSNOMEDmap20180401.txt", 
  "NHS TRUD/read to snomed/PBCLExtended20180401.txt") %>% 
  map_df(~readr::read_tsv(.)) %>%
  transmute(
    v2_term = V2_TERM,
    v2_read_code = V2_READ_CODE,
    snomed = SNOMED_CONCEPT_ID,
    read_new = str_remove_all(V2_READ_CODE, "\\.")
  )
 

# Import the CPRD gold and aurum conversion tables 
cprd_gold <- readr::read_tsv("CPRD/medical.txt") 
cprd_aurum <- readr::read_tsv("CPRD/CPRDAurumMedical.txt")



# VALIDATION LISTS ------------------------------------------------------------

# Import the RES-1 and RES-30 lists and pull out the code
# Truncate the codes to 5 length (removes bytes 6 and 7)
# Remove periods from end

res_1_30 <- c(
    "HTN/Dx Validation List/RES_1_hypertension.csv",
    "HTN/Dx Validation List/res30-hypertension-read.csv"
    ) %>%
  map_df(~read_csv(.)) %>%
  pull(code) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")


res_qof <- read_csv("HTN/Dx Validation List/qof_hypertension.csv") %>%
  pull(code) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")


res_180 <- read_csv("HTN/Dx Validation List/res180_hypertension.csv") %>%
  filter(coding_system == "Read") %>%
  pull(code) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")


lshtm_484 <- read_tsv("HTN/Dx Validation List/LSHTM_484_Clinical_codelist_Read_hypertension.txt") %>%
  pull(readcode) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")


lshtm_1031 <- read_csv("HTN/Dx Validation List/LSHTM_1031_hypertension_codes.txt") %>%
  pull(medcode) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")


icd_list_preeclampsia <-  trud_icd10 %>%
    filter(str_starts(icd10, "O10|O11|O13|O14|O15|O16")) %>%
    pull(read_new)


icd_list_htn <-  trud_icd10 %>%
  filter(str_starts(icd10, "I10|I11|I12|I13|I15|I16|I1A")) %>%
  pull(read_new)

# CREATE A BNF CODE LIST --------------------------------------------------

# Create a overall read list to check against
# Keep only distinct read codes
codelist_validation <- data.frame(
    read = c(
      res_1_30,
      res_qof,
      res_180,
      lshtm_484,
      lshtm_1031,
      icd_list_preeclampsia,
      icd_list_htn
      )) %>%
  distinct(read) %>%
  func_remove_subcodes("read") %>%
  pull(read)
  
# CHECK CODELIST ----------------------------------------------------------

# Cross check the lists imported vs Dx codelist

missing_codes <- codelist_validation %>%
  .[!(. %in% codelist_checking)]

missing_codes

# Decode missing codes 
cprd_aurum %>%
  filter(OriginalReadCode %in% missing_codes) %>%
  select(OriginalReadCode, Term) %>%
  print(n=999)

#Count how many translated read V2 codes to expect back
missing_codes %>%
str_starts("X", negate=T) %>%
sum()
