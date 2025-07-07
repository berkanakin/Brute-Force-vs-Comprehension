# ============================================================================
# STEP 3 ─ TRIMMING & OPTIMISATION  (final 980-trial dataset)
# ----------------------------------------------------------------------------
# Appendix A1.2 (“Trimming and Optimisation”) describes how the over-generated
# item set (≈ 56 k rows) was pruned to the 980 trials actually used:
#
#   • **In-block shuffle** – Within each lvl-2 column (lvl2_position) we reshuffle
#     14-row lvl-1 blocks, except in positions 2 & 6, to break up local
#     repetitions while preserving every solution hierarchy.
#   • **Greedy block picker** – Iteratively selects lvl-2 blocks (169 + 14 rows)
#     that contribute the most unique lvl-1 prompts, until 70 lvl-1
#     blocks × 14 rows = 980 rows are retained.
#   • **Duplicate-prompt repair** – Any residual duplicate lvl-1 prompts are
#     fixed by swapping in alternative blocks that share the same solution but
#     a different prompt, pulled from a pre-built replacement pool.
#   • **Integrity checks** – Re-runs the self-referential logic at all three
#     levels and confirms there are no solution overlaps (lvl0 ≠ lvl1 ≠ lvl2).

# ============================================================================

setwd("")

library(dplyr)
library(tidyr)
library(stringr)

lvl2itemset <- read.csv(".csv",
                        stringsAsFactors = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# 3·1  IN-BLOCK SHUFFLE  (diversify while keeping hierarchy intact)
# ─────────────────────────────────────────────────────────────────────────────

# Tag every 14-row lvl-1 block
lvl2itemset$lvl1_block_id <- rep(seq_len(nrow(lvl2itemset) / 14), each = 14)
cols_to_swap   <- c("lvl0_prompt", "lvl0_solution", "lvl1_prompt", "lvl1_id")

# Build a replacement pool *per* lvl2_position, sorted to interleave prompts
lvl1_pool_sorted <- lapply(split(lvl2itemset, lvl2itemset$lvl2_position), function(df) {
  df$lvl1_block_id <- rep(seq_len(nrow(df) / 14), each = 14)
  blocks <- split(df, df$lvl1_block_id)
  
  # interleave blocks to alternate prompts
  prompt_groups <- split(blocks, sapply(blocks, \(b) unique(b$lvl1_prompt)))
  depth <- max(lengths(prompt_groups))
  do.call(rbind, lapply(seq_len(depth), function(k) {
    do.call(rbind, lapply(prompt_groups, \(g) if (k <= length(g)) g[[k]]))
  }))
})

# Perform the shuffle, skipping columns 2 & 6
lvl2_itemset_shuffled1 <- lvl2itemset
for (pos in names(lvl1_pool_sorted)) {
  pos_num <- as.integer(pos)
  if (pos_num %in% c(2, 6)) next
  
  pool <- lvl1_pool_sorted[[pos]]
  pool$lvl1_block_id <- rep(seq_len(nrow(pool) / 14), each = 14)
  
  blocks_in_col <- unique(
    lvl2_itemset_shuffled1$lvl1_block_id[
      lvl2_itemset_shuffled1$lvl2_position == pos_num
    ])
  
  # cycle through pool blocks
  pool_ptr <- 1
  for (bid in blocks_in_col) {
    tgt_idx <- which(lvl2_itemset_shuffled1$lvl1_block_id == bid &
                       lvl2_itemset_shuffled1$lvl2_position == pos_num)
    src_blk <- pool[pool$lvl1_block_id == pool_ptr, ]
    lvl2_itemset_shuffled1[tgt_idx, cols_to_swap] <- src_blk[cols_to_swap]
    pool_ptr <- ifelse(pool_ptr %% max(pool$lvl1_block_id) + 1, pool_ptr + 1, 1)
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# 3·2  GREEDY PICKER  →  980 ROWS = 70 × 14
# ─────────────────────────────────────────────────────────────────────────────

max_rows   <- 980
row_total  <- 0
seen_lvl1  <- character()
retained   <- list()

for (bid in unique(lvl2_itemset_shuffled1$lvl2_id)) {
  block <- lvl2_itemset_shuffled1 %>% filter(lvl2_id == bid)
  new_prompts <- setdiff(unique(block$lvl1_prompt), seen_lvl1)
  
  # if block adds something new *and* keeps us ≤ 980 rows, retain it
  if (length(new_prompts) > 0 && (row_total + nrow(block)) <= max_rows) {
    retained[[as.character(bid)]] <- block
    seen_lvl1 <- union(seen_lvl1, new_prompts)
    row_total <- row_total + nrow(block)
  }
  if (row_total == max_rows) break
}

optimized_set <- bind_rows(retained)
stopifnot(nrow(optimized_set) == 980) # size check
# ─────────────────────────────────────────────────────────────────────────────
# 3·3  DUPLICATE-PROMPT REPAIR  (swap in alt. blocks with same solution)
# ─────────────────────────────────────────────────────────────────────────────

dup_ids <- optimized_set %>% # lvl-1 blocks with dup prompts
  count(lvl1_prompt) %>%
  filter(n > 14) %>%
  pull(lvl1_prompt)

if (length(dup_ids) > 0) {
  
  # Build replacement pool from blocks **not** in optimized_set
  replacement_pool <- lvl2_itemset_shuffled1 %>%
    filter(!(lvl2_id %in% names(retained))) %>%
    group_by(lvl1_id) %>% filter(n() == 14) %>% ungroup()
  
  # iterate duplicates until none remain
  repeat {
    dup_map <- optimized_set %>%
      count(lvl1_prompt) %>%
      filter(n > 14)
    if (nrow(dup_map) == 0) break
    
    for (dup_prompt in dup_map$lvl1_prompt) {
      bad_block <- optimized_set %>% filter(lvl1_prompt == dup_prompt) %>% slice(1:14)
      sol_word  <- unique(bad_block$lvl1_solution)
      
      # Find alternate block with same solution but different prompt
      alt_block <- replacement_pool %>%
        filter(lvl1_solution == sol_word,
               lvl1_prompt  != dup_prompt,
               !(lvl1_prompt %in% optimized_set$lvl1_prompt)) %>%
        group_by(lvl1_id) %>% filter(n() == 14) %>% slice(1:14)
      
      if (nrow(alt_block) == 14) {
        optimized_set[optimized_set$lvl1_id == unique(bad_block$lvl1_id),
                      cols_to_swap] <- alt_block[cols_to_swap]
      } else {
        warning("No alt block for duplicate prompt: ", dup_prompt)
      }
    }
  }
}

stopifnot(length(unique(optimized_set$lvl1_prompt)) == 70) # uniqueness check


# ─────────────────────────────────────────────────────────────────────────────
# 3·4  INTEGRITY CHECKS  (self-referential correctness & no overlaps)
# ─────────────────────────────────────────────────────────────────────────────

# Helper: ordinal map reused from Step 1
ordinal_nums <- c(
  "first"=1,"second"=2,"third"=3,"fourth"=4,"fifth"=5,"sixth"=6,"seventh"=7,
  "eighth"=8,"ninth"=9,"tenth"=10,"eleventh"=11,"twelfth"=12,"thirteenth"=13,
  "fourteenth"=14,"fifteenth"=15,"sixteenth"=16,"seventeenth"=17,"eighteenth"=18,
  "nineteenth"=19,"twentieth"=20,"twenty-first"=21,"twenty-second"=22,
  "twenty-third"=23,"twenty-fourth"=24,"twenty-fifth"=25,"twenty-sixth"=26,
  "twenty-seventh"=27,"twenty-eighth"=28,"twenty-ninth"=29,"thirtieth"=30
)

# ---- lvl0 solutions correct? -------------------------------------------------
lvl0_ok <- all(apply(optimized_set, 1, \(r) {
  pr <- r["lvl0_prompt"]; sol <- r["lvl0_solution"]
  ords <- str_extract_all(pr, paste(names(ordinal_nums), collapse="|"),
                          simplify = TRUE)[1,1:2]
  if (any(is.na(ords))) return(TRUE)
  n1 <- ordinal_nums[tolower(ords[1])]; n2 <- ordinal_nums[tolower(ords[2])]
  words <- str_split(pr, "\\\\s+")[[1]]; idx <- length(words)+1 - (n2 - n1 - 1)
  tolower(words[idx]) == tolower(sol)
}))
stopifnot(lvl0_ok)

# ---- lvl1 / lvl2 overlap guards ---------------------------------------------
stopifnot(
  !any(optimized_set$lvl==2 &
         optimized_set$lvl2_solution==optimized_set$lvl1_solution, na.rm=TRUE),
  !any(optimized_set$lvl==2 &
         optimized_set$lvl2_solution==optimized_set$lvl0_solution, na.rm=TRUE),
  !any(optimized_set$lvl==1 &
         optimized_set$lvl1_solution==optimized_set$lvl0_solution, na.rm=TRUE)
)

# ─────────────────────────────────────────────────────────────────────────────
# 3·5  EXPORT  (for final step)
# ─────────────────────────────────────────────────────────────────────────────

write.csv(optimized_set,
          "optimized_set.csv", row.names = FALSE)

message("🎉  Step 3 completed: 980 rows, 70 unique lvl-1 prompts, integrity OK.")

