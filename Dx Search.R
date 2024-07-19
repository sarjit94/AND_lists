# CREATE FUNCTIONS ------------------------------------------------------------
func_diag_search <- function(code, byte) {
  cprd_aurum %>%
    filter(str_starts(OriginalReadCode, code)) %>%
    filter(str_length(OriginalReadCode) <= byte ) %>%
    arrange(str_length(OriginalReadCode)) %>%
    select(OriginalReadCode, Term) %>%
    view()
}

func_diag_tally <- function(code, byte) {
  cprd_aurum %>%
    filter(str_starts(OriginalReadCode, code)) %>%
    group_by(str_sub(OriginalReadCode, end=byte)) %>%
    tally() %>%
    view()
}

# DIAGNOSES CODES ---------------------------------------------------------

# Function to search for codes based on length
func_diag_search(
  code = "B",
  byte = 5
)

# Tally number of subcodes 
func_diag_tally(
  code = "E2",
  byte = 3L
)

## SECTION TO DEAL WITH TERM CODES
# # Find terms from codes - END
# cprd_aurum %>%
#   filter(str_length(CleansedReadCode)>5) %>%
#   filter(str_ends(CleansedReadCode, "15")) %>%
#   select(CleansedReadCode, Term) %>%
#   filter(str_length(CleansedReadCode)<=8) %>%
#   arrange(CleansedReadCode) %>%
#   print(n=999)
# 
# # Find terms from codes - START
# cprd_aurum %>%
#   filter(str_length(CleansedReadCode)>5) %>%
#   filter(str_starts(CleansedReadCode, "TK\\.\\.\\.")) %>%
#   select(CleansedReadCode, Term) %>%
#   filter(str_length(CleansedReadCode)<=8) %>%
#   arrange(CleansedReadCode) %>%
#   print(n=999)

# Find terms from codes
cprd_aurum %>%
  filter(str_starts(OriginalReadCode, "E2E")) %>%
  select(OriginalReadCode, Term) %>%
  filter(str_length(OriginalReadCode)<=4) %>%
  arrange(OriginalReadCode) %>%
  print(n=999)

# Find codes from terms
cprd_aurum %>%
  filter(str_detect(Term, regex("adhd|hyperact|attent|hyperkinet", ignore_case = TRUE))) %>%
  select(OriginalReadCode, Term) %>%
  filter(str_length(OriginalReadCode)<5) %>%
  arrange(OriginalReadCode) %>%
  print(n=999) 

# Find codes from terms
codelist_checking <- cprd_aurum %>%
  filter(str_detect(Term, regex("adhd|add|hyperact|Attent|hyperkinet", ignore_case = TRUE))) %>%
  select(OriginalReadCode, Term) %>%
  # filter(str_length(OriginalReadCode)<9) %>%
  arrange(OriginalReadCode) %>%
  pull(OriginalReadCode)

# Decode the codelist for checking
cprd_aurum %>%
  filter(read_new %in% func_remove_subcodes2(codelist_checking)) %>%
  select(read_new, Term) %>%
  arrange(read_new) %>%
  print(n=999) 

  write.csv("dx.csv")
