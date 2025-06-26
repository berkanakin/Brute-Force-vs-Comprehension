# ============================================================================
# STEP 2 ─ TASK SET‑UP  (critical lvl‑1 / lvl‑2 answer rows  + distractors)
# ----------------------------------------------------------------------------
# This section corresponds to “Task Setup” in Appendix A1.2.  The code below
# takes the over‑generated lvl‑2 item set (no distractor rows yet) and
# transforms it into the exact trial order required by the experiment:
#   • Inserts a 14th row in every lvl‑1 block → critical meta‑rule trial.
#   • Fills that row with the lvl‑1 solution word and a ground‑level distractor
#     whose solution ≠ lvl‑1 solution.
#   • Inserts a 14‑row distractor block after each lvl‑2 block → critical
#     meta‑meta‑rule trial (lvl==2).  The inserted block’s solutions differ
#     from both lvl‑1 & lvl‑2 solutions in the surrounding context.
# ============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# 2.0  Load lvl‑2 item set (no distractors yet)
# ─────────────────────────────────────────────────────────────────────────────

setwd("")

itemset <- read.csv(".csv")


# ─────────────────────────────────────────────────────────────────────────────
# 2·1  Insert an all-NA 14th row after every 13-row lvl-1 block
# ─────────────────────────────────────────────────────────────────────────────
empty_row   <- itemset[1, ];  empty_row[] <- NA               # template row
insert_after <- seq(13, nrow(itemset), by = 13)               # after rows 13, 26 …

itemset1 <- itemset
for (i in rev(insert_after)) {                                # reverse to avoid index shift
  itemset1 <- rbind(itemset1[1:i, ], empty_row,
                    itemset1[(i + 1):nrow(itemset1), ])
}

# Flag new rows as lvl == 1 (critical meta-rule) and adjust indices ----------
itemset1$lvl <- 0
itemset1$lvl[is.na(itemset1$lvl1_id)]        <- 1     # new rows → lvl 1
itemset1$lvl1_position[is.na(itemset1$lvl1_position)] <- 14
itemset1$lvl0_id <- seq_len(nrow(itemset1))           # temporary re-count

itemset1 <- itemset1[-1]                              # drop CSV index col
itemset1 <- itemset1[-c(nrow(itemset1), nrow(itemset1)-1), ]  # trim junk tail
itemset1$lvl1_id[is.na(itemset1$lvl1_id)] <- seq_len(nrow(itemset1)/14)

# ─────────────────────────────────────────────────────────────────────────────
# 2·2  Propagate lvl-1 / lvl-2 meta-data downward (tidyr::fill)
# ─────────────────────────────────────────────────────────────────────────────
library(tidyr)
itemset1 <- itemset1 %>%
  fill(lvl1_solution, lvl2_solution, .direction = "down") %>%
  fill(lvl1_prompt,   lvl2_prompt,   .direction = "down") %>%
  fill(lvl2_id,       lvl2_position, .direction = "down")

# ─────────────────────────────────────────────────────────────────────────────
# 2·3  Insert ground-level distractor sentence in every 14th row
# ─────────────────────────────────────────────────────────────────────────────
target_rows <- seq(14, nrow(itemset1), by = 14)  # the new rows we just added

solution_freq   <- sort(table(itemset1$lvl0_solution), decreasing = FALSE)
unique_solutions <- names(solution_freq)

library(dplyr)
candidates <- itemset1 %>%                       # pool of viable lvl-0 rows
  filter(!is.na(lvl0_solution)) %>%
  mutate(row_id = row_number()) %>%
  select(row_id, lvl0_prompt, lvl0_solution)

used_rows      <- integer(0)                     # to avoid re-using same row
solution_reuse <- setNames(rep(0, length(unique_solutions)), unique_solutions)

for (row_idx in target_rows) {
  current_lvl1 <- itemset1$lvl1_solution[row_idx]
  
  valid <- candidates %>%                        # exclude same-solution rows
    filter(lvl0_solution != current_lvl1,
           !row_id %in% used_rows) %>%
    mutate(reuse_count = solution_reuse[lvl0_solution]) %>%
    arrange(reuse_count, lvl0_solution)          # rare solutions first
  
  if (nrow(valid) == 0) next                     # safety check
  
  sel <- valid[1, ]
  itemset1$lvl0_prompt[row_idx]   <- sel$lvl0_prompt
  itemset1$lvl0_solution[row_idx] <- sel$lvl0_solution
  
  used_rows <- c(used_rows, sel$row_id)
  solution_reuse[sel$lvl0_solution] <-
    solution_reuse[sel$lvl0_solution] + 1
}

# ─────────────────────────────────────────────────────────────────────────────
# 2·4  Trim final word off each lvl-0 prompt (visual neatness only)
# ─────────────────────────────────────────────────────────────────────────────
itemset1$lvl0_prompt <- gsub("\\\\s+\\\\S+$", "", itemset1$lvl0_prompt)

# ─────────────────────────────────────────────────────────────────────────────
# 2·5  Append empty 14-row placeholders after each lvl-2 block
# ─────────────────────────────────────────────────────────────────────────────
make_empty_rows <- function(template_row) {
  blk <- template_row[rep(1, 14), ];  blk[] <- NA; blk
}

lvl2itemset <- do.call(
  rbind,
  lapply(split(itemset1, itemset1$lvl2_id), function(block) {
    if (nrow(block) == 182)                     # 169 + 13 existing rows
      rbind(block, make_empty_rows(block[1, ]))
    else block
  })
)

lvl2itemset$lvl2_position[is.na(lvl2itemset$lvl2_position)] <- 14
lvl2itemset <- lvl2itemset %>%
  fill(lvl2_solution, lvl2_prompt, lvl2_id, .direction = "down")

lvl2itemset$lvl0_id <- seq_len(nrow(lvl2itemset))
lvl2itemset$lvl[is.na(lvl2itemset$lvl)] <-
  rep(c(rep(0, 13), 2), sum(is.na(lvl2itemset$lvl)) / 14)   # mark lvl 2 rows

# ─────────────────────────────────────────────────────────────────────────────
# 2·6  Replace empty blocks with full lvl-1 distractor blocks
# ─────────────────────────────────────────────────────────────────────────────
set.seed(42)                            # reproducibility
block_size <- 14
key_cols   <- c("lvl1_id","lvl1_position","lvl0_prompt","lvl0_solution",
                "lvl1_prompt","lvl1_solution")

extract_last_word <- function(x) {
  words <- strsplit(trimws(x), "\\\\s+")[[1]]
  tolower(tail(words, 1))
}

# Candidate blocks: exactly 14 rows sharing the same lvl1_id
source_blocks <- lapply(
  split(lvl2itemset, lvl2itemset$lvl1_id),
  function(df) if (nrow(df) == block_size) df
)
source_blocks <- source_blocks[!sapply(source_blocks, is.null)]

for (start_row in seq(1, nrow(lvl2itemset), by = block_size)) {
  # Detect placeholder (all meta columns NA)
  if (all(is.na(lvl2itemset$lvl1_id[start_row:(start_row+13)]))) {
    
    dest_word <- tolower(lvl2itemset$lvl2_solution[start_row + 13])
    repeat {
      cand_id  <- sample(names(source_blocks), 1)
      cand_blk <- source_blocks[[cand_id]]
      
      lvl0_14  <- extract_last_word(cand_blk$lvl0_solution[14])
      lvl1_14  <- extract_last_word(cand_blk$lvl1_solution[14])
      
      # Ensure the new block’s final answers differ from surrounding lvl-2 sol
      if (is.na(dest_word) || !(dest_word %in% c(lvl0_14, lvl1_14))) {
        for (col in key_cols)
          lvl2itemset[start_row:(start_row+13), col] <- cand_blk[[col]]
        break
      }
    }
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# 2·7  Final integrity checks & export
# ─────────────────────────────────────────────────────────────────────────────
write.csv(lvl2itemset, "updated_lvl2itemset_complete_final.csv", row.names = FALSE)

stopifnot(
  !any(lvl2itemset$lvl == 2 &
         lvl2itemset$lvl2_solution == lvl2itemset$lvl1_solution, na.rm = TRUE),
  !any(lvl2itemset$lvl == 2 &
         lvl2itemset$lvl2_solution == lvl2itemset$lvl0_solution, na.rm = TRUE),
  !any(lvl2itemset$lvl == 1 &
         lvl2itemset$lvl1_solution == lvl2itemset$lvl0_solution, na.rm = TRUE)
)

message("✔︎ Task-setup (Step 2) completed with no solution overlaps.")


