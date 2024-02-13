# IMPORT CHECKING LIST  ------------------------------------------------
# As importing, create new variables with periods removed (easier to work with)
codelist_checking <-
  readxl::read_excel(
    "Asthma/Asthma_2024.02.10 S2 file_read codes for drugs used in LTC definitions_medications.xlsx",
    sheet = 2
  ) %>%
  pull(readcode) %>%
  str_remove_all(., "\\.")

# VALIDATION LISTS ------------------------------------------------------------

logesh <- read_csv("Asthma/Rx Validation List/Asthma Rx - Logesh.csv") %>%
  pull(code) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")

lshtm_338 <- cprd_gold %>%
  filter(prodcode %in% as.list(
    readr::read_tsv("Asthma/Rx Validation List/lshtm_338_Asthma_therapy.txt") %>%
      pull(prodcode)
  )) %>%
  pull(bnfchapter) %>%
  str_split(., pattern = "/", simplify = TRUE) %>%
  .[. != ""]

lshtm_2210 <- cprd_aurum %>%
  filter(ProdCodeId %in% as.list(
    read_csv("Asthma/Rx Validation List/lshtm_2210_asthma_prodcodes_aurum_mar20.txt") %>%
      pull(prodcodeid)
  )) %>%
  filter(!is.na(BNFChapter)) %>%
  pull(BNFChapter) %>%
  .[. != ""]

lshtm_2211 <- 
cprd_gold %>%
  filter(prodcode %in% as.list(
    read_csv("Asthma/Rx Validation List/lshtm_2211_asthma_prodcodes_gold_jul19.txt") %>%
      pull(prodcode)
  )) %>%
  pull(bnfchapter) %>%
  str_split(., pattern = "/", simplify = TRUE) %>%
  .[. != ""]

# CREATE A BNF CODE LIST --------------------------------------------------

# Create a overall BNF list to check against
# Keep only distinct BNF chapters
# Remove BNF chapter 00000000 (i.e. keep only BNF chapters >1)
# Remove BNF chapters starting with 11 (eye drops)
# Convert this to a vector and add leading zeros
bnf_list <-
  data.frame(
    bnf_chap = as.numeric(c(
      lshtm_338,
      lshtm_2210,
      lshtm_2211))) %>%
  distinct(bnf_chap) %>%
  filter(bnf_chap >1) %>%
  filter(!str_starts(bnf_chap, "11")) %>%
  pull(bnf_chap) %>%
  formatC(., width = 8, format = "d", flag = "0")



# Create a overall read list to check against
# Keep only distinct read codes
codelist_validation <- data.frame(
  read = c(
    bnf_list,
    logesh
  )) %>%
  distinct(read) %>%
  func_remove_subcodes("read") %>%
  pull(read)


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


# Decode the codelist for checking
biobank_read_lookup %>%
  filter(read_new %in% func_remove_subcodes2(codelist_checking)) %>%
  select(read_new, term_description) %>%
  print(n=999)


