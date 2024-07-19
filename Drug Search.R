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
  code = "djc", 
  byte = 5
)

# Tally number of subcodes 
func_drug_tally(
  code = "d2",
  byte = 3L
)

# Find terms from codes
biobank_read_lookup %>%
  filter(str_starts(read_new, "dw|db|dz|dc1")) %>%
  filter(str_length(read_new) <= 5) %>%
  arrange(read_new) %>%
  select(read_code, term_description) %>% 
  print(n=999) 

# Find codes from terms
biobank_read_lookup %>%
  filter(str_detect(term_description, "METHAD")) %>%
  print(n=999)


codelist_checking <- biobank_read_lookup %>%
  filter(str_starts(read_new, "dw|db|dz|dc1")) %>%
  # filter(str_length(read_new) <= 5) %>%
  arrange(read_new) %>%
  select(read_new, term_description) %>% 
  pull(read_new)

# Decode the codelist for checking
biobank_read_lookup %>%
  filter(read_new %in% func_remove_subcodes2(codelist_checking)) %>%
  select(read_new, term_description) %>%
  print(n=999) 
  
  
  
biobank_read_lookup %>%
  filter(str_starts(read_new, "dw|db|dz|dc1")) %>%
  # filter(str_length(read_new) <= 5) %>%
  select(read_code, term_description) %>%
  arrange(read_code) %>%

  write_csv("export.csv")

