# IMPORT CHECKING CODELIST   ------------------------------------------------

# As importing, create new variables with periods removed (easier to work with)
codelist_checking <-
  read_csv("Epilepsy/Epilepsy Dx 2024.04.08.csv") %>%
  pull('Code') %>%
  str_trunc(5, side = "right", ellipsis = "") %>%
  str_remove_all(., "\\.")

# VALIDATION LISTS ------------------------------------------------------------

# Import the RES-1 and RES-30 lists and pull out the code
# Truncate the codes to 5 length (removes bytes 6 and 7)
# Remove periods from end

icd_list_epilepsy <-  trud_icd10 %>%
  filter(str_starts(icd10, "G40|G41")) %>%
  pull(read_new)

mum_predict_epilespy <-
  read_csv("Epilepsy/Dx Validation List/MP - Epilepsy_MM.csv") %>%
  pull(Code) %>%
  str_trunc(5, side = "right", ellipsis = "") %>%
  str_remove_all("\\.")

mum_predict_epilespy2 <- c(
  "Epilepsy/Dx Validation List/MP - Epilepsy_birm_cam_CPRD_AURUM.csv",
  "Epilepsy/Dx Validation List/MP - Epilepsy_birm_cam_CPRD_GOLD.csv"
) %>%
  map_df( ~ read_csv(.)) %>%
  pull(READ_CODE) %>%
  na.omit() %>%
  str_trunc(5, side = "right", ellipsis = "") %>%
  str_remove_all("\\.")

mum_predict_epilespy3 <-
  read_csv("Epilepsy/Dx Validation List/MP - Epilepsy_birm_cam_ICD10.csv") %>%
  pull(READ_CODE) %>%
  str_trunc(5, side = "right", ellipsis = "") %>%
  str_remove_all("\\.")

lshtm_864 <-
  read_csv("Epilepsy/Dx Validation List/lshtm_864_Clinical_codelist_Read_epilepsy.txt") %>%
  pull(readcode) %>%
  str_trunc(5, side = "right", ellipsis = "") %>%
  str_remove_all("\\.")

res <- c(
  "Epilepsy/Dx Validation List/man1 - epilepsy.csv",
  "Epilepsy/Dx Validation List/res21-epilepsy.csv", 
  "Epilepsy/Dx Validation List/res26-epilepsy.csv",
  "Epilepsy/Dx Validation List/res55-epilepsy.csv"
) %>%
  map_df( ~ read_csv(.)) %>%
  pull(code) %>%
  str_trunc(5, side = "right", ellipsis = "") %>%
  str_remove_all("\\.")



# CREATE A BNF CODE LIST --------------------------------------------------

# Create a overall read list to check against
# Keep only distinct read codes
codelist_validation <- data.frame(
  read = c(
    icd_list_epilepsy,
    mum_predict_epilespy,
    mum_predict_epilespy2,
    mum_predict_epilespy3,
    lshtm_864, 
    res
  )
) %>%
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
  arrange(OriginalReadCode) %>%
  print(n = 999) %>%
  print(n=999)


#Count how many translated read V2 codes to expect back
missing_codes %>%
  str_starts("X", negate = T) %>%
  sum()

# Decode the codelist for checking
cprd_aurum %>%
  filter(read_new %in% func_remove_subcodes2(codelist_checking)) %>%
  select(read_new, Term) 
