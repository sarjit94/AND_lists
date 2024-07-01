# IMPORT CHECKING LIST  ------------------------------------------------
# As importing, create new variables with periods removed (easier to work with)
codelist_checking <- read_csv("Epilepsy/Epilepsy Rx 2024.04.08.csv", col_names = FALSE) %>%
  pull(X1) %>%
  str_remove_all(., "\\.")

# VALIDATION LISTS ------------------------------------------------------------

res_9 <- read_csv("Epilepsy/Rx Validation List/man9 - epilepsy_drugs.csv") %>%
  pull(BNF_code)

res_55 <- read_csv("Epilepsy/Rx Validation List/res55-antiepileptic.csv") %>%
  pull(code) %>%
  str_remove_all(., "\\.")


# CREATE A BNF CODE LIST --------------------------------------------------

# Create a overall BNF list to check against
# Keep only distinct BNF chapters
# Remove BNF chapter 00000000 (i.e. keep only BNF chapters >1)
# Remove BNF chapters starting with 11 (eye drops)
# Convert this to a vector and add leading zeros
bnf_list <-
  data.frame(
    bnf_chap = as.numeric(c(
      res_9))) %>%
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
    res_55
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
