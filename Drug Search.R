# FUNCTIONS -----------------------------------------------
# A function to search for codes based on start and length 
func_drug_search <- function(code, byte) {
  biobank_read_lookup %>%
    filter(str_starts(read_new, code)) %>%
    filter(str_length(read_new) <= byte ) %>%
    arrange(str_length(read_new)) %>%
    view()
}

# A function to tally subcodes
func_drug_tally <- function(code, byte) {
  biobank_read_lookup %>%
    filter(str_starts(read_new,code)) %>%
    group_by(str_sub(read_new, end=byte)) %>%
    tally() %>%
    view()
} 

# DRUG CODES  -----------------------------------------------

# Function to search for codes based on length
func_drug_search(
  code = "dn", 
  byte = 3
)

# Tally number of subcodes 
func_drug_tally(
  code = "d2",
  byte = 3L
)

# Find terms from codes
biobank_read_lookup %>%
  filter(str_starts(read_new, "9h3"))

# Find codes from terms
biobank_read_lookup %>%
  filter(str_detect(term_description, "CLOBAZAM"))

# Decode the codelist for checking
biobank_read_lookup %>%
  filter(read_new %in% func_remove_subcodes2(codelist_checking)) %>%
  select(read_new, term_description) %>%
  print(n=999)
