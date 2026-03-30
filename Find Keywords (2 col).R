# Find keywords/phrases and return the full sentence from IWC SC reports and papers
# Two columns only
# IWC SC report years 2018 and older

library(pdftools)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# remove front pages for 2 col docs
pdf_dir <- "FILE PATH"

# ============================== #
# 1. KEYWORDS
# ============================== #
keywords_list <- c(
  "social learning","socially learned",
  "\\bculture\\b",
  "animal culture","dialect",
  "song","migration","vocal repertoire","foraging tactic","foraging strategy",
  "site fidelity","philopatry","maternal philopatry","traditional feeding grounds",
  "specialisation","specialization","sympatric population","social unit","social network",
  "matrilineal","clan","acoustic clan","coda","behavioural plasticity","behavioral plasticity",
  "innovation","tool-use","depredation","maternal care","behaviour","behavioural response",
  "matriarch","experienced individual","informed individual",
  "\\bknowledge\\b",
  "knowledgable",
  "knowledgable individual","leadership","transmission","historic range",
  "behavior","behavioural change","behavioral change",
  "recovery","geographic variation","foraging tradition",
  "vocal","communication","maternally directed","tool use","sponge","sponger",
  "sponging","conching","behaviour diversity","behavior diversity",
  "behavioural diversity","behavioral diversity","observational learning", "cultural diffusion")

# ============================== #
# 2. EXCLUSIONS
# ============================== #
exclusion_phrases <- c(
  "knowledge gap","knowledge gaps",
  "acknowledge","current knowledge",
  "aquaculture","agriculture")

keyword_pattern <- str_c(keywords_list, collapse="|")
exclusion_pattern <- str_c(exclusion_phrases, collapse="|")

# ============================== #
# 3. COLUMN-AWARE TEXT EXTRACTION
# ============================== #
extract_column_sentences <- function(file){
  pages <- pdf_data(file)
  map_df(seq_along(pages), function(p){
    df <- pages[[p]]
    # skip blank pages
    if (nrow(df) == 0) return(NULL)
    mid_x <- median(df$x)
    df <- df %>%
      mutate(column = ifelse(x < mid_x, "left", "right"))
    # rebuild lines
    lines <- df %>%
      arrange(column, y, x) %>%
      group_by(column, y) %>%
      summarise(text_line = str_c(text, collapse=" "), .groups="drop")
    # combine lines within column
    column_text <- lines %>%
      group_by(column) %>%
      summarise(
        text = str_c(text_line, collapse=" "),
        .groups="drop")
    # split into sentences
    sentences <- column_text %>%
      mutate(sentence = str_split(text, "(?<=[.!?])\\s+")) %>%
      unnest(sentence)
    sentences %>%
      mutate(
        file = basename(file),
        page = p)})}

# ============================== #
# 4. READ PDFs
# ============================== #
files <- list.files(pdf_dir, pattern="\\.pdf$", full.names=TRUE)

# sentences_df <- map_df(files, extract_column_sentences)

# print file name
sentences_df <- map_df(files, function(f) {
  print(f)
  extract_column_sentences(f)})
View(sentences_df)

# ============================== #
# 5. SEARCH SENTENCES
# ============================== #
search_results <- sentences_df %>%
  filter(str_detect(sentence, regex(keyword_pattern, ignore_case = TRUE))) %>%
  filter(!str_detect(sentence, regex(exclusion_pattern, ignore_case = TRUE))) %>%
  mutate(
    keywords_found = str_extract_all(
      sentence,
      regex(keyword_pattern, ignore_case = TRUE)),
    keywords_found = sapply(keywords_found, function(x) paste(unique(x), collapse = "; "))) %>%
  select(file, page, column, sentence, keywords_found)

# ============================== #
# 6. EXPORT
# ============================== #
write.csv(search_results, "FILE PATH")
