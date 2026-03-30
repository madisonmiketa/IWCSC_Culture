# Find keywords/phrases and return the full sentence from IWC SC reports and papers
# Single column only
# IWC SC Reports 2019 to present

library(pdfsearch)
library(dplyr)
library(stringr)

# folder where PDFs are housed
pdf_dir <- "C:\\Users\\mmiketa\\OneDrive - The Humane Society of the United States\\Desktop1\\New Folder"
pdf_dir <- "C:\\Users\\mmiketa\\OneDrive - The Humane Society of the United States\\Desktop1\\Papers"

pdf_dir <- "C:\\Users\\mmiketa\\OneDrive - The Humane Society of the United States\\Desktop1\\test"


# -----------------------------
# 1. KEYWORD LIST
# -----------------------------

# list of keywords we want to be found
# with boundaries for words that have exclusions
keywords_list <- c(
  "social learning","socially learned",
  
  # Use word boundary for "culture"
  "\\bculture\\b",
  
  "animal culture","dialect",
  "song","migration","vocal repertoire","foraging tactic","foraging strategy",
  "site fidelity","philopatry","maternal philopatry","traditional feeding grounds",
  "specialisation","specialization","sympatric population","social unit","social network",
  "matrilineal","clan","acoustic clan","coda","behavioural plasticity","behavioral plasticity",
  "innovation","tool-use","depredation","maternal care","behaviour","behavioural response",
  "matriarch", "experienced individual","informed individual",
  
  # Word boundary for "knowledge"
  "\\bknowledge\\b",
  
  "knowledgable",
  "knowledgable individual","leadership","transmission","historic range",
  "behaviour","behavior","behavioural change","behavioral change",
  "recovery","geographic variation","foraging tradition",
  "vocal", "communication", "maternally directed", "tool use", "sponge", "sponger",
  "sponging", "conching", "behaviour diversity", "behavior diversity", "behavioural diversity", 
  "behavioral diversity", "observational learning"
)

# -----------------------------
# 2. EXCLUSION PHRASES
# -----------------------------

# these are the phrases we want to exclude from our keyword search
exclusion_phrases <- c(
  # Knowledge exclusions
  "knowledge gap",
  "knowledge gaps",
  "acknowledge",
  "current knowledge",
  
  # Culture exclusions
  "aquaculture",
  "agriculture"
)

# -----------------------------
# 3. SEARCH PDFs
# -----------------------------

# results from our keyword search
search_results <- keyword_directory(
  directory = pdf_dir,
  keyword = keywords_list,
  full_names = TRUE,
  ignore_case = TRUE,
  convert_sentence = TRUE
)

# -----------------------------
# 4. REMOVE EXCLUDED PHRASES
# -----------------------------

# Combine exclusions into one expression using | for 'or'
exclusion_pattern <- str_c(exclusion_phrases, collapse = "|")

# remove the rows with our excluded phrase
search_results <- search_results %>%
  filter(!str_detect(line_text, regex(exclusion_pattern, ignore_case = TRUE)))

# Remove token_text column if present
# search_results <- search_results[, !names(search_results) %in% "token_text"]

# Convert everything to character
search_results[] <- lapply(search_results, as.character)

# -----------------------------
# 5. EXPORT
# -----------------------------
write.csv(
  search_results,
  "C:\\Users\\mmiketa\\OneDrive - The Humane Society of the United States\\Desktop1\\Output\\Papers2024.csv",
  row.names = FALSE
)

nrow(search_results)

search_results_unique <- unique(search_results)

write.csv(
  search_results_unique,
  "C:\\Users\\mmiketa\\OneDrive - The Humane Society of the United States\\Desktop1\\Output\\Papers2024_nodups.csv",
  row.names = FALSE
)

nrow(search_results_unique)

