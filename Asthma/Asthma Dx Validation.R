# IMPORT CHECKING CODELIST   ------------------------------------------------

# As importing, create new variables with periods removed (easier to work with)
codelist_checking <- readxl::read_excel(
  "Asthma/Asthma_2024.02.10 S1 file_read codes for included LTC_diagnosis.xlsx",
  sheet = 2) %>%
  pull('Read Code') %>%
  str_remove_all(., "\\.")

# VALIDATION LISTS ------------------------------------------------------------

# Import the RES-1 and RES-30 lists and pull out the code
# Truncate the codes to 5 length (removes bytes 6 and 7)
# Remove periods from end

icd_list_asthma <-  trud_icd10 %>%
  filter(str_starts(icd10, "J45|J46")) %>%
  pull(read_new)

mum_predict_asthma <- read_csv("Asthma/Dx Validation List/MP - AsthmaLongList2018.csv") %>%
  pull(Code) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")

lshtm_337 <- cprd_gold %>%
  filter(medcode %in% as.list(
    read_tsv("Asthma/Dx Validation List/LSTHM_337_Asthma_diagnosis.txt") %>%
      pull(medcode)
  )) %>%
  pull(readcode) %>%
  str_trunc(5, side = "right", ellipsis = "") %>%
  str_remove_all("\\.")

lshtm_479 <- cprd_gold %>%
  filter(medcode %in% as.list(
    read_csv("Asthma/Dx Validation List/LSTHM_479_asthma_codes_specific.txt") %>%
      pull(medcode)
  )) %>%
  pull(readcode) %>%
  str_trunc(5, side = "right", ellipsis = "") %>%
  str_remove_all("\\.")

lshtm_492 <- cprd_gold %>%
  filter(medcode %in% as.list(
    read_csv("Asthma/Dx Validation List/LSTHM_492_asthma_codes_nonspec.txt") %>%
      pull(medcode)
  )) %>%
  pull(readcode) %>%
  str_trunc(5, side = "right", ellipsis = "") %>%
  str_remove_all("\\.")

lshtm_586 <- read_csv("Asthma/Dx Validation List/LSTHM_856_Clinical_codelist_Read_asthma.txt") %>%
  pull(readcode) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")

lshtm_2209 <- read_csv("Asthma/Dx Validation List/LSTHM_2209_asthma_gold_jul19.txt") %>%
  pull(readcode) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")

lshtm_2578 <- read_csv("Asthma/Dx Validation List/LSTHM_2578_asthma_gp.txt") %>%
  pull(Readcodes) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")

res_25 <- 
  c(
  "Asthma/Dx Validation List/asthma_clinicals.csv",
  "Asthma/Dx Validation List/res25-p1asthma.csv"
) %>%
  map_df(~read_csv(.)) %>%
  pull(code) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")

res_26 <- 
  read_csv("Asthma/Dx Validation List/res26-asthma.csv") %>%
  pull(code) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")

logesh <- read_csv("Asthma/Dx Validation List/Asthma Dx - Logesh.csv", col_names=F) %>%
  pull(X1) %>%
  str_trunc(5, side="right", ellipsis="") %>%
  str_remove_all("\\.")
  
# CREATE A BNF CODE LIST --------------------------------------------------

# Create a overall read list to check against
# Keep only distinct read codes
codelist_validation <- data.frame(
    read = c(
      icd_list_asthma,
      mum_predict_asthma,
      #lshtm_337,
      lshtm_492,
      lshtm_586,
      lshtm_2209,
      lshtm_2578,
      res_25,
      res_26,
      logesh
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
  arrange(OriginalReadCode) %>%
  print(n=999)


#Count how many translated read V2 codes to expect back
missing_codes %>%
str_starts("X", negate=T) %>%
sum()
