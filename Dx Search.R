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
  code = "Eu80",
  byte = 5
)

# Tally number of subcodes 
func_diag_tally(
  code = "E2",
  byte = 3L
)

# Find terms from codes
cprd_aurum %>%
  filter(str_starts(OriginalReadCode, "E2")) %>%
  select(OriginalReadCode, Term)

# Find codes from terms
cprd_aurum %>%
  filter(str_detect(Term, "lepsy")) %>%
  select(OriginalReadCode, Term)

# Decode the codelist for checking
cprd_aurum %>%
  filter(read_new %in% func_remove_subcodes2(codelist_checking)) %>%
  select(read_new, Term) %>%
  print(n=999)
