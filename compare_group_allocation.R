# compare_group_allocations.R
#
# PURPOSE
#   Compare each student's group before and after run_group_allocation.R
#   ran, so you can see exactly who moved and reach out to any
#   students/groups whose situation actually changed.
#
# INPUT
#   - "group_rosters.csv" -- the original raw Canvas export, from
#     BEFORE the allocation ran.
#   - "group_rosters_assigned.csv" -- the allocation pipeline's final
#     output, from AFTER it ran.
#   Only students present in BOTH files are compared -- anyone removed
#   as inactive before allocation (see run_group_allocation.R) is in
#   the raw file but not the assigned one, and is simply left out here
#   rather than shown as a false "lost their group" change.
#
# OUTPUT
#   - "group_comparison.csv" -- one row per student: their group and
#     full teammate list before and after, and whether it's flagged
#     "Different". Sorted by tutorial_number, then final_group_name.
#   - "final_group_lookup.csv" -- one row per final group: just the
#     tutorial number and group name and its members' user_ids (no names), 
#     so a student can find their own group by searching their user_id. 
#     Sorted by tutorial number and final_group_name. Upload this to Canvas.
#
# WHEN "different" IS FLAGGED
#   A student is flagged "Different" if their final group_name or full
#   set of final teammates doesn't exactly match their initial
#   group_name and teammates -- EXCEPT in these cases, which are not
#   flagged:
#     - the student had no group initially, and still has none now
#       (both "Not grouped" -- nothing to flag, nothing changed)
#     - the student had no group initially, and every one of their new
#       teammates was ALSO groupless initially -- i.e. they've simply
#       landed in a brand-new group formed entirely from previously
#       ungrouped students, not joined an already-existing group's
#       dynamic

library(dplyr)
library(stringr)
library(readr)

# Turns a sorted vector of user_ids into a single readable string for
# the CSV (a real list-column can't be written to a flat CSV file).
# Returns NA for an empty/NULL list (e.g. a "Not grouped" student).
collapse_ids <- function(ids) {
  if (length(ids) == 0) return(NA_character_)
  paste(sort(ids), collapse = ", ")
}

compare_group_allocations <- function(
    raw_path = "group_rosters.csv",
    assigned_path = "group_rosters_assigned.csv",
    comparison_path = "group_comparison.csv",
    lookup_path = "final_group_lookup.csv") {
  
  raw <- read_csv(raw_path, show_col_types = FALSE)
  assigned <- read_csv(assigned_path, show_col_types = FALSE)
  
  # Sort key for "Group Assignment NN" names -- plain text sorting would
  # put "Group Assignment 10" before "Group Assignment 2", which isn't
  # the order anyone scanning the file expects; sort by the actual
  # number instead
  group_number <- function(group_name) {
    as.integer(str_match(group_name, "Group Assignment\\s+(\\d+)")[, 2])
  }
  
  # Extract tutorial number the same way run_group_allocation.R does
  tut_match <- str_match(raw$sections, "Tutorial\\s+\\d+\\s*\\((\\d+)\\)")
  raw <- raw |> mutate(tutorial_number = as.integer(tut_match[, 2]))
  
  # Initial team members: every user_id sharing a group_name in the raw
  # roster (including the student themselves), sorted, as a list-column
  initial_members <- raw |>
    filter(!is.na(group_name)) |>
    group_by(group_name) |>
    summarise(initial_team_ids = list(sort(user_id)), .groups = "drop")
  
  raw <- raw |>
    left_join(initial_members, by = "group_name") |>
    rename(initial_group_name = group_name)
  
  # Final team members: the same idea, from the assigned roster
  final_members <- assigned |>
    filter(!is.na(group_name)) |>
    group_by(group_name) |>
    summarise(final_team_ids = list(sort(user_id)), .groups = "drop")
  
  assigned <- assigned |>
    left_join(final_members, by = "group_name") |>
    rename(final_group_name = group_name)
  
  # Only compare students present in both files
  combined <- raw |>
    select(name, user_id, tutorial_number, initial_group_name, initial_team_ids) |>
    inner_join(
      assigned |> select(user_id, final_group_name, final_team_ids, group_issues),
      by = "user_id"
    )
  
  # Lookup: for any given user_id, were they originally groupless?
  # (used by the "all new teammates were also groupless" exception)
  groupless_lookup <- setNames(is.na(combined$initial_group_name), as.character(combined$user_id))
  
  different <- vapply(seq_len(nrow(combined)), function(i) {
    
    init_name <- combined$initial_group_name[i]
    final_name <- combined$final_group_name[i]
    
    if (is.na(init_name) && is.na(final_name)) {
      return(FALSE)  # ungrouped before and after -- nothing changed
    }
    
    if (is.na(init_name)) {
      # originally groupless -- only flag if the final group ISN'T
      # made entirely of other originally-groupless students
      final_ids <- combined$final_team_ids[[i]]
      all_groupless <- all(groupless_lookup[as.character(final_ids)])
      return(!all_groupless)
    }
    
    # had a real group before -- flag if the name OR the exact
    # teammate set changed at all
    init_ids <- combined$initial_team_ids[[i]]
    final_ids <- combined$final_team_ids[[i]]
    !identical(init_name, final_name) || !identical(init_ids, final_ids)
    
  }, logical(1))
  
  comparison <- combined |>
    mutate(
      initial_team_members = vapply(initial_team_ids, collapse_ids, character(1)),
      final_team_members = vapply(final_team_ids, collapse_ids, character(1)),
      different = ifelse(different, "Different", NA_character_),
      .sort_key = group_number(final_group_name)
    ) |>
    arrange(tutorial_number, .sort_key) |>
    select(
      student_name = name,
      user_id,
      tutorial_number,
      initial_group_name,
      initial_team_members,
      final_group_name,
      final_team_members,
      final_group_issues = group_issues,
      different
    )
  
  write_csv(comparison, comparison_path)
  
  message(sprintf(
    "Wrote %d student rows to '%s' (%d flagged as Different).",
    nrow(comparison), comparison_path,
    sum(comparison$different == "Different", na.rm = TRUE)
  ))
  
  # Second file: one row per final group, members only, no names
  lookup <- assigned |>
    filter(!is.na(final_group_name)) |>
    distinct(tutorial_number, final_group_name, final_team_ids) |>
    mutate(final_group_members = vapply(final_team_ids, collapse_ids, character(1))) |>
    select(tutorial_number, final_group_name, final_group_members) |>
    arrange(tutorial_number, group_number(final_group_name))
  
  write_csv(lookup, lookup_path)
  
  message(sprintf("Wrote %d groups to '%s' for student lookup.", nrow(lookup), lookup_path))
  
  invisible(list(comparison = comparison, lookup = lookup))
}

if (sys.nframe() == 0) {
  compare_group_allocations()
}