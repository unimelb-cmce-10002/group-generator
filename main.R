# main.R — driver script for group generation
library(readr)
library(dplyr)
library(assertr)

# 1) Load helpers --------------------------------------------------------------
source("R/group_generator.R")  
# provides:
# - extract_tutorial_group()
# - group_generator()
# - pass_size_check()

# 2) Load data -----------------------------------------------------------------
roster_file <- "data/roster.csv"
# Canvas People export CSV (after importing sign-ups into sections)
df <- read_csv(roster_file, show_col_types = FALSE)

# 3) Extract tutorial group from `sections` ------------------------------------
# Pull the number inside Tutorial n (G) -> returns integer G
df <- df %>%
    mutate(tutorial_group = extract_tutorial_group(sections))

# Optional: manual fix-ups not yet in the enrollment system
# See README for format of a supplementary csv of changes
df <- apply_overrides(df, path = "config/overrides.csv")  

# 4) Generate groups (max 4s, 3s only as needed) -------------------------------
groups <- group_generator(
    data        = df,
    grouping_var= tutorial_group,
    grp_size    = 4,          # preference: make as many 4s as possible
    seed        = 76,
    name_prefix = NULL,       # we’ll build our own label below
    min_size    = 3
) %>%
    mutate(
        group_name = paste0("Tutorial ", tutorial_group, ", Group ", group_id)
    )

# 5) Verify sizes with assertr -------------------------------------------------
# Returns invisibly TRUE or throws (by default) if a check fails.
# We'll catch the error and turn it into a logical for simple branching.
size_ok <- tryCatch(
    {
        pass_size_check(
            data        = groups,
            grouping_var= tutorial_group,
            group_col   = "group_id",
            min_size    = 3,
            allowed     = c(3, 4),
            error_fun   = assertr::error_stop,
            success_fun = assertr::success_continue
        )
        TRUE
    },
    error = function(e) {
        message("Group size check failed:\n", conditionMessage(e))
        FALSE
    }
)

# 6) Write output --------------------------------------------------------------
# First reduce to only the columns from the original dataframe
orig_cols <- names(df)
upload_df <- 
    groups %>% 
    select(all_of(orig_cols))

if (size_ok){
    message("✅ Groups valid. Saving CSV")
    write_csv(upload_df, "output/assignment_groups.csv")
} else {
    message("❌ Not saving: fix group sizes and re-run.")
}

