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
  filter(str_starts(OriginalReadCode, "E2E|ZS9|9Ng|901|Eu90|8BPT|1P00.00|Eu9y700|6A61.")) %>%
  select(OriginalReadCode, Term) %>%
  filter(str_length(OriginalReadCode)<=8) %>%
  arrange(OriginalReadCode) %>%
  print(n=999)

# Find codes from terms
cprd_aurum %>%
  filter(str_detect(Term, "piat|endence|ddicti")) %>%
  select(OriginalReadCode, Term) %>%
  filter(str_length(OriginalReadCode)<5) %>%
  arrange(OriginalReadCode) %>%
  write.csv("dx.csv")

# Decode the codelist for checking
cprd_aurum %>%
  filter(read_new %in% func_remove_subcodes2(codelist_checking)) %>%
  select(read_new, Term) %>%
  print(n=999)
