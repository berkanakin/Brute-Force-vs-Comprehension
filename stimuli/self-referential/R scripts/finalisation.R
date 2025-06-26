# ============================================================================
# STEP 4 ─ FINALISATION  (export unstructured + full metadata)
# ----------------------------------------------------------------------------
# After trimming/optimisation (Step 3) the 980-row dataset is exported in three
# parallel forms:
#
#   • **Structured**   – full trial table (all 19 columns) used by the game.
#   • **Unstructured** – only `trial`, `prompt`, `solution` (for API testing).
#   • **Metadata**     – adds analysis helpers:
#         – `lvl0_id`, `lvl1_id`, `lvl2_id`  (re-numbered, compact, 1-based)
#         – `*_first` flags marking the first occurrence of each prompt
#         – `item_id`  tracks every unique prompt across *all* three levels.
#
# ============================================================================

setwd("")

library(dplyr)
library(tidyr)
library(stringr)

# ---------------------------------------------------------------------------
# 4·1  READ STRUCTURED DATA  (produced at end of Step 3)
# ---------------------------------------------------------------------------
structured <- read.csv("llm_meta-task_final-testset_structured.csv",
                       stringsAsFactors = FALSE)

if (!all(c("trial","prompt","solution") %in% names(structured))) {
  structured <- structured %>%
    mutate(trial   = row_number(),
           prompt  = lvl0_prompt,
           solution= case_when(
             lvl == 0 ~ lvl0_solution,
             lvl == 1 ~ lvl1_solution,
             lvl == 2 ~ lvl2_solution
           )) %>%
    select(trial, prompt, solution, everything())
}

# ---------------------------------------------------------------------------
# 4·2  WRITE UNSTRUCTURED DATASET  (three columns only)
# ---------------------------------------------------------------------------
write.csv(structured[c("trial","prompt","solution")],
          "llm_meta-task_final-testset_unstructured.csv",
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 4·3  BUILD FULL METADATA TABLE
# ---------------------------------------------------------------------------

metadata <- structured

# ---- 4·3·1  Re-number lvl0_id  & flag first appearance --------------------
metadata <- metadata %>%
  mutate(lvl0_id    = match(lvl0_prompt, unique(lvl0_prompt)),
         lvl0_first = !duplicated(lvl0_prompt))

# ---- 4·3·2  Re-number lvl1_id  & lvl1_first  (14-row blocks) -------------
blk_idx      <- seq(1, nrow(metadata), by = 14)
lvl1_prompts <- metadata$lvl1_prompt[blk_idx]

metadata$lvl1_id    <- rep(match(lvl1_prompts, unique(lvl1_prompts)), each = 14)
metadata$lvl1_first <- rep(!duplicated(lvl1_prompts),                 each = 14)

# ---- 4·3·3  Compact lvl2_id  (196-row blocks) ----------------------------
metadata$lvl2_id <- rep(seq_len(nrow(metadata) / 196), each = 196)

# ---- 4·3·4  Trial-block counters (optional helpers) ----------------------
metadata <- metadata %>%
  mutate(trial_block_lvl1 = rep(seq_len(nrow(metadata) / 14),  each = 14),
         trial_block_lvl2 = rep(seq_len(nrow(metadata) / 196), each = 196))

# ---- 4·3·5  item_id  (unique across *all* prompts) ------------------------
metadata <- metadata %>%
  mutate(prompt_key = case_when(
    lvl == 0 ~ lvl0_prompt,
    lvl == 1 ~ lvl1_prompt,
    lvl == 2 ~ lvl2_prompt),
    item_id = match(prompt_key, unique(prompt_key))) %>%
  select(-prompt_key)

# ---------------------------------------------------------------------------
# 4·4  RE-ORDER COLUMNS FOR READABILITY  
# ---------------------------------------------------------------------------
metadata <- metadata %>%
  select(trial,
         lvl, lvl0_id, lvl0_first, lvl1_id, lvl2_id,
         lvl1_position, lvl2_position,
         lvl0_prompt, lvl1_prompt, lvl2_prompt, prompt = prompt,
         lvl0_solution, lvl1_solution, lvl2_solution, solution,
         trial_block_lvl1, trial_block_lvl2, item_id)

write.csv(metadata, "llm_itemset_metadata.csv", row.names = FALSE)

message("🎯  Finalisation complete: exported unstructured & metadata files.")


