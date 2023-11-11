##### LOAD LIBRARIES / SETUP R ------------------------------------------------
# Clear R 
rm(list=ls())
graphics.off()

library(tidyverse)


##### SETUP OPTIONS -----------------------------------------------------------
# Location of the codelist to check
cl_checking <- readxl::read_excel("HTN/HTN Rx codes.xlsx")

# Import BioBank conversion table  
biobank <- readxl::read_excel("BioBank/biobank.xlsx", sheet = "read_v2_drugs_bnf")
biobank$bnfcode_new <- str_remove_all(biobank$bnf_code, "\\.")


# First validation set
res24_beta_blockers <- read_csv("HTN/Validation List/res24-beta_blockers.csv")

# Remove quotation marks
res24_beta_blockers$bnfcode <- str_remove_all(res24_beta_blockers$bnfcode, '"')

list_res24_beta_blockers <- str_split(res24_beta_blockers$bnfcode, pattern = "/", simplify = TRUE) %>% 
  as.list() %>%
  .[. != ""]



