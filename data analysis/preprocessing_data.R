# Data Preprocessing

#-------------------------------------------------------------------------------
# STEP 1: PUT ALL FILES INTO A LIST
#-------------------------------------------------------------------------------


#Load metadata------------------------------------------------------------------
metadata <-read.csv("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/metadata/llm_itemset_metadata.csv")

#Load model_output_data---------------------------------------------------------

# Define the directory path
model_data_raw_dir <- "/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/model_data_raw/outdated"

# Get list of CSV files in the folder
file_list <- list.files(model_data_raw_dir, pattern = "\\.csv$", full.names = TRUE)

# Helper to create safe R variable names
clean_var_name <- function(file_path) {
  base <- tools::file_path_sans_ext(basename(file_path))  # Remove .csv
  name <- gsub("-", "_", base)                            # Replace dashes with _
  make.names(name)                                        # Make syntactically valid name
}

# Loop through and assign each file as a dataframe
for (file in file_list) {
  var_name <- clean_var_name(file)
  assign(var_name, read.csv(file))
}

#Make a list containing all otf them

model_data_list <- mget(clean_var_name(file_list))
# Save the list to an .rds file
saveRDS(model_data_list, file = "model_data_list.rds")



####

library(purrr)
library(stringr)
library(tools)


model_data_raw_dir <- "/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/data analysis/preliminary/model_data_raw/"   # ← your folder

# 1 ── grab only files that respect “…_(experimental|control).csv”
file_list <- list.files(model_data_raw_dir,
                        pattern   = "_(experimental|control)\\.csv$",
                        full.names = TRUE,
                        recursive  = TRUE)

model_data_list <- list()

for (f in file_list) {
  stem      <- file_path_sans_ext(basename(f))     # e.g. "gpt-4o_control"
  condition <- if (str_ends(stem, "_control")) "control" else "experimental"
  
  # model id = part before the final suffix, turn dashes into "_" so you can use $
  model_id  <- gsub("-", "_", sub("_(experimental|control)$", "", stem))
  
  if (is.null(model_data_list[[model_id]]))
    model_data_list[[model_id]] <- list()
  
  ## keep the data-frame under its EXACT stem name (incl. suffix) ------------
  model_data_list[[model_id]][[stem]] <- read.csv(f)
}

saveRDS(model_data_list, "model_data_list.rds")


#-------------------------------------------------------------------------------
# STEP 2: VALIDITY & PROPER RESPONSE FORMAT 
#-------------------------------------------------------------------------------

model_data_list <- readRDS("model_data_list.rds")

# 1: valid response format

library(stringr)
library(dplyr)

# Function to add valid_format column to a single data frame
add_valid_format <- function(df) {
  if (!("predicted_word" %in% names(df))) return(df)
  df %>%
    mutate(valid_format = !is.na(predicted_word) & str_detect(str_trim(predicted_word), "^[^\\s]+$"))
}

# Recursively apply the function to all data frames in your list
model_data_list <- lapply(model_data_list, function(model_entry) {
  lapply(model_entry, function(df) {
    add_valid_format(df)
  })
})


## A table showing valid responses 


library(dplyr)
library(purrr)
library(stringr)

## 1 ── make sure every data-frame has valid_format ────────────────────────────
add_valid <- function(df) {
  if (!"valid_format" %in% names(df)) {
    df <- df %>% 
      mutate(valid_format = !is.na(predicted_word) &
               str_detect(str_trim(predicted_word), "^[^\\s]+$"))
  }
  df
}

model_data_list <- map(model_data_list, ~map(.x, add_valid))

## 2 ── helper to turn a data-frame into “valid/total” text ────────────────────
ratio <- function(df) {
  total  <- nrow(df)
  valid  <- sum(df$valid_format, na.rm = TRUE)
  sprintf("%d/%d", valid, total)
}

## 3 ── build the summary table ───────────────────────────────────────────────
summary_tbl <- map_dfr(names(model_data_list), function(model_name) {
  entry <- model_data_list[[model_name]]
  
  # find the two data-frames by their names’ suffix
  control_idx <- which(endsWith(names(entry), "_control"))
  exper_idx   <- which(endsWith(names(entry), "_experimental"))
  
  tibble(
    model        = model_name,
    experimental = if (length(exper_idx) == 1) ratio(entry[[exper_idx]]) else NA_character_,
    control      = if (length(control_idx) == 1) ratio(entry[[control_idx]]) else NA_character_
  )
})

print(summary_tbl)

#### Visually inspect all the invalid responses

model_data <- model_data_list

library(dplyr)
library(purrr)
library(stringr)
library(tibble)

# Helper function to extract invalid predicted_word entries
extract_invalid_responses <- function(df, model, condition) {
  if (!"predicted_word" %in% names(df)) return(NULL)
  
  df %>%
    mutate(row = row_number(),
           correct = as.character(correct)) %>%  # ensure consistent type
    filter(!is.na(predicted_word),
           str_detect(str_trim(predicted_word), "\\s")) %>%
    mutate(model = model,
           condition = condition) %>%
    select(model, condition, row, prompt, predicted_word, expected_output, correct)
}

# Loop through all model-condition combinations
invalid_responses <- map_dfr(names(model_data), function(model_name) {
  model_entry <- model_data[[model_name]]
  
  map2_dfr(model_entry,
           names(model_entry),
           ~extract_invalid_responses(.x, model = model_name, condition = .y))
})

# View the result (first 50 rows)
print(invalid_responses, n = 50)

#These responses could be parsed and checked for validity

#-------------------------------------------------------------------------------
















































# outdated
#-------------------------------------------------------------------------------

# Merge data into longformat


library(dplyr)
library(purrr)
library(stringr)

names(model_data_list)

# -------------------------------
# 1. Load every CSV into its own df
# -------------------------------

# read & assign each CSV
walk(file_list, ~ assign(clean_var_name(.x), read.csv(.x), envir = .GlobalEnv))

# -------------------------------
# 2. Put the model dfs in a list
# -------------------------------
model_dfs <- mget(clean_var_name(file_list))             # list of data-frames

# -------------------------------
# 3. Standardise + stack them long
# -------------------------------
models_long <- imap_dfr(
  model_dfs,
  ~ .x %>%                                                # .x = one dataframe
    select(-prompt, -expected_output) %>%              # keep only per-model vars †
    mutate(model = .y)                                 # .y = list name ⇒ model id
)
# † If you want `prompt` or `expected_output` you can keep them;
#   they are identical across models so dropping avoids dup columns.

# -------------------------------
# 4. Bring in the metadata
# -------------------------------
full_long <- metadata %>%
  left_join(models_long, by = "trial")                   # adds model rows per trial

# -------------------------------
# 5. (Optional) check
# -------------------------------
glimpse(full_long)
table(full_long$model)                                   # rows per model




#-------------------------------------------------------------------------------







