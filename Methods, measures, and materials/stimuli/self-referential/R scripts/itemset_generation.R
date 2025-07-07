# ============================================================================
# STEP 1 ─ STIMULUS GENERATION  (ground-level pool → vertical blocks)
# ----------------------------------------------------------------------------
# This section corresponds to “Stimulus Generation” in Appendix A1.2.  
# The code constructs the entire self-referential item set from scratch:
#
#   • Enumerates every ordered pair of ordinal words (“first” … “thirtieth”)
#     and instantiates the template  
#       “The n₁-th word after the n₂-th-to-last word in this sentence is ___”.
#
#   • Applies the self-referential indexing formula  
#       target index = totalWords + 1 − (n₂ − n₁ − 1)  
#     retaining only sentences where the index lands on an actual word.
#
#   • Records that target word as the lvl-0 solution, yielding the ground-level
#     pool (286 unique prompt–solution pairs).
#
#   • Builds 13-row lvl-1 *vertical* blocks by stacking lvl-0 prompts whose
#     solutions, read top-to-bottom, spell a self-referential sentence.
#
#   • Rearranges those lvl-1 blocks into 169-row lvl-2 vertical blocks,
#     again keeping only blocks whose combined sentence resolves correctly.
#
#   • Outputs the over-generated item set (with **no** distractors yet) that
#     Step 2 (“Task Setup”) will transform by adding distractors.
# ============================================================================

# ──────────────────────────────────────────────────────────────────────────────
# 0.  SET‑UP
# ──────────────────────────────────────────────────────────────────────────────

setwd("[YOUR_DIRECTORY]")

# Core libraries --------------------------------------------------------------
library(dplyr)     # data wrangling
library(gtools)    # permutations helpers
library(tidyr)     # fill() for propagating values
library(stringr)   # regex & string ops

# Global ordinal lookup -------------------------------------------------------
ordinal_nums <- c(
  "first" = 1,   "second" = 2,  "third" = 3,  "fourth" = 4,  "fifth" = 5,
  "sixth" = 6,   "seventh" = 7, "eighth" = 8, "ninth" = 9,  "tenth" = 10,
  "eleventh" = 11, "twelfth" = 12, "thirteenth" = 13, "fourteenth" = 14,
  "fifteenth" = 15, "sixteenth" = 16, "seventeenth" = 17, "eighteenth" = 18,
  "nineteenth" = 19, "twentieth" = 20, "twenty-first" = 21, "twenty-second" = 22,
  "twenty-third" = 23, "twenty-fourth" = 24, "twenty-fifth" = 25,
  "twenty-sixth" = 26, "twenty-seventh" = 27, "twenty-eighth" = 28,
  "twenty-ninth" = 29, "thirtieth" = 30)
all_ordinals <- names(ordinal_nums)

# ──────────────────────────────────────────────────────────────────────────────
# 1.  LVL0:  GENERATE SELF‑REFERENTIAL SENTENCES
# ──────────────────────────────────────────────────────────────────────────────

# Build template given two ordinal words (n1 and n2)
build_sentence_after <- function(first_word, second_word) {
  paste("The", first_word, "word after the", second_word,
        "to last word in this sentence is")
}

# Analyse a candidate sentence & return list (or NULL if invalid)
analyze_sentence_after <- function(first_word, second_word) {
  sentence <- build_sentence_after(first_word, second_word)
  words    <- strsplit(sentence, " ")[[1]]
  total    <- length(words) + 1               # +1 for the missing answer word
  
  n1 <- ordinal_nums[first_word]
  n2 <- ordinal_nums[second_word]
  
  target_from_end <- n2 - n1 - 1              # how far from sentence end
  target_index    <- total - target_from_end  # convert to forward index
  
  if (target_from_end >= 0 && target_index >= 1 && target_index <= length(words)) {
    sol <- words[target_index]
    list(sentence = paste(sentence, sol),
         solution = sol,
         solution_position = target_index,
         n1 = n1, n2 = n2)
  } else {
    NULL  # violates logic → discard
  }
}

# Exhaustive enumeration of ordered ordinal pairs (n1 < n2)
results  <- list();  k <- 1
for (f in all_ordinals) {
  for (s in all_ordinals) {
    if (ordinal_nums[f] < ordinal_nums[s]) {
      tmp <- analyze_sentence_after(f, s)
      if (!is.null(tmp)) { results[[k]] <- tmp; k <- k + 1 }
    }
  }
}

# Collate & sort by answer word for deterministic ordering
sls_after        <- bind_rows(lapply(results, as.data.frame))
sls_after_sorted <- sls_after[order(sls_after$solution), ]

# ──────────────────────────────────────────────────────────────────────────────
# 2.  LVL1:  BUILD VERTICAL SENTENCES (13× rows)
# ──────────────────────────────────────────────────────────────────────────────

# Split ground‑level pool by word type
fixed_words     <- sls_after_sorted[!sls_after_sorted$solution %in% all_ordinals, ]
numerical_words <- sls_after_sorted[  sls_after_sorted$solution %in% all_ordinals, ]
unique_nums     <- unique(numerical_words$solution)

# All ordered pairs of distinct ordinal words
num_pairs <- permutations(length(unique_nums), 2, v = unique_nums)

# Template for a 13‑word vertical sentence
structure_slots <- c("The", "NUM1", "word", "after", "the", "NUM2",
                     "to", "last", "word", "in", "this", "sentence", "is")

results_list  <- list()      # accumulated lvl1 rows
reuse_tracker <- list()      # cyclic prompt reuse per word

for (i in 1:nrow(num_pairs)) {
  pair <- num_pairs[i, ]
  slots <- structure_slots
  slots[slots == "NUM1"] <- pair[1]
  slots[slots == "NUM2"] <- pair[2]
  if (!all(slots %in% sls_after$solution)) next  # ensure all words exist
  
  prompts <- character(13)
  ok <- TRUE
  for (j in seq_along(slots)) {
    w <- slots[j]
    pool <- sls_after %>% filter(solution == w)
    if (nrow(pool) == 0) { ok <- FALSE; break }
    
    # initialise / rotate reuse list
    if (is.null(reuse_tracker[[w]]) || length(reuse_tracker[[w]]) == 0) {
      reuse_tracker[[w]] <- sample(pool$sentence)
    }
    prompts[j] <- reuse_tracker[[w]][1]
    reuse_tracker[[w]] <- reuse_tracker[[w]][-1]
  }
  if (ok) {
    results_list[[length(results_list)+1]] <- tibble(
      vertical_id = length(results_list)+1,
      word_slot   = slots,
      solution_word = slots,
      sentence_prompt = prompts,
      vertical_sentence_combined = paste(slots, collapse = " ")
    )
  }
}

final_table <- bind_rows(results_list)
cat("Built", length(unique(final_table$vertical_id)), "vertical sentences × 13 rows each.\n")

table(final_table$sentence_prompt)  # quick freq diagnostic

# ──────────────────────────────────────────────────────────────────────────────
# 3.  EXTRACT LVL1 SOLUTIONS (self‑referential resolution)
# ──────────────────────────────────────────────────────────────────────────────

uniq_sentences <- unique(na.omit(final_table$vertical_sentence_combined))
extracted <- character(length(uniq_sentences))

for (i in seq_along(uniq_sentences)) {
  sent  <- uniq_sentences[i]
  words <- str_split(sent, " ")[[1]]
  ord1  <- str_replace_all(tolower(words[2]), "[^a-z-]", "")
  ord2  <- str_replace_all(tolower(words[6]), "[^a-z-]", "")
  n1 <- ordinal_nums[ord1];   n2 <- ordinal_nums[ord2]
  if (is.na(n1) || is.na(n2)) { extracted[i] <- NA; next }
  idx_from_end <- n2 - n1 - 1
  idx          <- 14 - idx_from_end  # total words incl. blank = 14
  extracted[i] <- if (idx>=1 && idx<=length(words)) words[idx] else NA
}

lvl2_solutions_df <- data.frame(
  vertical_sentence_combined = uniq_sentences,
  lvl2_solution_word = extracted,
  stringsAsFactors = FALSE)

# Merge lvl1 solution into table, drop redundant column
final_table <- merge(final_table, lvl2_solutions_df,
                     by = "vertical_sentence_combined", all.x = TRUE)
final_table <- final_table[-3]
colnames(final_table) <- c("lvl1_prompt", "lvl1_id", "lvl0_solution",
                           "lvl0_prompt", "lvl1_solution")

# Drop any verticals lacking a lvl1 solution
table(is.na(final_table$lvl1_solution))
final_table1 <- final_table[!is.na(final_table$lvl1_solution), ]

# ──────────────────────────────────────────────────────────────────────────────
# 4.  LVL2:  BUILD VERTICAL (LEVEL-2) SENTENCES (13×13 grid)
# ──────────────────────────────────────────────────────────────────────────────

# 4.1  Prepare block pool -----------------------------------------------------

blocks <- final_table1 %>%               # each lvl1 contains 13 rows
  group_by(lvl1_id) %>% filter(n()==13) %>% ungroup() %>%
  mutate(used = FALSE)                   # usage flag for fair distribution

block_pool <- split(blocks, blocks$lvl1_solution)

# Retain only words with ≥13 available blocks (one full vertical)
block_pool <- block_pool[sapply(block_pool, function(df) nrow(df) >= 13)]

# Convenience lookups ---------------------------------------------------------
ordinal_pool <- block_pool[names(block_pool) %in% all_ordinals]
fixed_pool   <- block_pool[!names(block_pool) %in% all_ordinals]

ordinal_words <- names(ordinal_pool)
num_pairs2    <- permutations(length(ordinal_words), 2, v = ordinal_words)

# Helper: fetch next unused block for a given solution word
get_next_block <- function(word) {
  pool <- block_pool[[word]]
  available <- pool %>% group_by(lvl1_id) %>% filter(!any(used)) %>% ungroup()
  if (nrow(available)==0) available <- pool   # allow reuse once exhausted
  id <- unique(available$lvl1_id)[1]
  blk <- pool %>% filter(lvl1_id == id)
  block_pool[[word]] <<- pool %>%
    mutate(used = ifelse(lvl1_id==id, TRUE, used))
  blk
}

# 4.2  Assemble lvl2 grids ----------------------------------------------------

structure_slots2 <- structure_slots  # same 13‑word template
new_lvl2 <- list()
next_id  <- ifelse("lvl2_id" %in% names(final_table),
                   max(final_table$lvl2_id, na.rm=TRUE)+1, 1)

for (i in 1:nrow(num_pairs2)) {
  words <- structure_slots2
  words[2] <- num_pairs2[i,1]
  words[6] <- num_pairs2[i,2]
  sentence_txt <- paste(words, collapse=" ")
  
  # Fetch a 13‑row block for each word in order
  blocks13 <- lapply(words, get_next_block)
  full_blk <- bind_rows(blocks13)
  full_blk$lvl2_id    <- next_id
  full_blk$lvl2_prompt <- sentence_txt
  new_lvl2[[i]] <- full_blk
  next_id <- next_id + 1
}

# Combine lvl0 + new lvl2 rows -------------------------------------------------
final_output <- bind_rows(final_table1, bind_rows(new_lvl2))
final_output1 <- final_output[!is.na(final_output$lvl2_prompt), ]

table(final_output1$lvl2_prompt)  # quick check uniqueness

# 4.3  Compute lvl2 solutions -------------------------------------------------

uniq_lvl2 <- unique(na.omit(final_output1$lvl2_prompt))
ex_lvl2   <- character(length(uniq_lvl2))
for (i in seq_along(uniq_lvl2)) {
  sent  <- uniq_lvl2[i]
  words <- str_split(sent, " ")[[1]]
  ord1  <- str_replace_all(tolower(words[2]), "[^a-z-]", "")
  ord2  <- str_replace_all(tolower(words[6]), "[^a-z-]", "")
  n1 <- ordinal_nums[ord1];  n2 <- ordinal_nums[ord2]
  if (is.na(n1)|is.na(n2)) { ex_lvl2[i] <- NA; next }
  from_end <- n2 - n1 - 1
  idx      <- 14 - from_end
  ex_lvl2[i] <- if(idx>=1 && idx<=length(words)) words[idx] else NA
}

lvl2_df <- data.frame(lvl2_prompt=uniq_lvl2, lvl2_solution=ex_lvl2, stringsAsFactors=FALSE)

final_output2 <- merge(final_output1, lvl2_df, by="lvl2_prompt", all.x=TRUE)
final_output2 <- final_output2[!is.na(final_output2$lvl2_solution), ]

# ──────────────────────────────────────────────────────────────────────────────
# 5.  FINAL ORGANISATION & EXPORT
# ──────────────────────────────────────────────────────────────────────────────

# Remove helper flag, then create hierarchical IDs and position indices
final_output2 <- final_output2 %>% select(-used)

final_output2$lvl2_id       <- rep(1:(nrow(final_output2)/169), each=169)
final_output2$lvl1_id       <- rep(1:(nrow(final_output2)/13),  each=13)
final_output2$lvl1_position <- rep(1:13,                 nrow(final_output2)/13)
final_output2$lvl2_position <- rep(rep(1:13, each=13), nrow(final_output2)/169)
final_output2$lvl0_id       <- seq_len(nrow(final_output2))

# Re‑order columns for readability
final_output2 <- final_output2 %>%
  select(lvl0_id, lvl1_id, lvl1_position,
         lvl2_id, lvl2_position,
         lvl0_prompt, lvl0_solution,
         lvl1_prompt, lvl1_solution,
         lvl2_prompt, lvl2_solution)

table(final_output2$lvl1_prompt)  # final sanity frequency

# Persist to CSV (without distractors at lvl0 blanks yet)
write.csv(final_output2, "lvl2itemset_without_distractors.csv", row.names = FALSE)

# End of STEP 1 ─ STIMULUS GENERATION ─────────────────────────────────────────
