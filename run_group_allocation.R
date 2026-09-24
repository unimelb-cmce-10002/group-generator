# run_group_allocation.R
#
# PURPOSE
#   Take the raw Canvas export (group_rosters.csv) all the way through to
#   a final, randomized 3-4 person group allocation, in one script. No
#   manual CSV round-tripping through Canvas or Excel needed in between.
#
# WORKFLOW
#   1. prepare_group_data() reads group_rosters.csv, removes any inactive
#      groupless students (per inactive_students.csv -- see INPUT), then
#      works out each student's tutorial number. If anyone is missing
#      one (Canvas doesn't let staff set this directly), it stops and
#      lists exactly who, with ready-to-paste lines to fill them in.
#   2. Fill those in by hand, then call finish_group_allocation(group_data)
#      to continue. Can be called again if anything's still missing.
#   3. finish_group_allocation() checks within each group if all members
#      are from the same tutorial number. Any group that isn't gets
#      dissolved and recorded, then the full random allocation below
#      (RULES) runs on what's left.
#   4. run_group_allocation() chains steps 1-3 together, and only pauses
#      if it actually has to -- so once tutorial numbers are complete,
#      one call does everything.
#
# RULES (allocation, once every student has a tutorial number and every
# group is confined to one tutorial)
#   1. Students never move between tutorials.
#   2. Existing groups of 4 are protected.
#   3. Existing groups of 1 or 2 (pairs and singles) can be merged
#      together with each other, or with an existing group of 3, while
#      staying below the max of 4 -- worked out exactly (see Step 2:
#      SOLVE AND PACK below), not by greedily merging whatever fits
#      first, so a locally convenient merge never accidentally strands
#      a different group that needed the same students.
#   4. Existing groups of 3 and 4 are never dissolved.
#   5. Existing groups of 1 and 2 may be folded into another group.
#   6. Group Assignment numbers are reusable once a group disappears.
#   7. Every plan is checked for feasibility BEFORE anything is moved --
#      if no combination of existing groups + groupless students can
#      place everyone into valid 3-4 groups, nothing is guessed at;
#      Step 3 (RESCUE) is tried next, then anyone still left is flagged.
#   8. New groups use any number from 1-200 that is not currently active.
#
# REPRODUCIBILITY
#   All randomness goes through safe_shuffle()/safe_sample_n(), seeded by
#   `seed` (default 123). Re-running with the same input file and the same
#   seed always produces the exact same group assignments. Pass a
#   different seed (or NULL, to let R's own random state decide) if you
#   deliberately want a different shuffle.
#
# INPUT 
#   - "group_rosters.csv", exported from Canvas -> People -> Group
#     Assignment. One row per student, a 'sections' column listing
#     tutorial enrolments as free text (e.g. "Tutorial 1 (21)"), and a
#     'group_name' column showing which group the student self-enrolled
#     into (NA if they haven't).
#   - "inactive_students.csv" (optional -- skipped with a message if not
#     found). Columns: Student, Section, Student Id. Any student whose
#     Student Id matches group_rosters.csv's user_id, AND who has no
#     group_name yet, is removed before anything else runs. A match who
#     DOES have a group_name is left alone (they've likely re-enrolled).
#
# OUTPUT
#   -  "group_rosters_cleaned.csv" - the data once every student has a
#      tutorial number and every cross-tutorial group has been
#      dissolved. For your records.
#   -  "problem_groups.csv" - only written if at least one group spans
#      more than one tutorial. Lists every member of every such group
#      (all flagged "To be dissolved") for future reference -- these
#      groups are fully dissolved, and all their members are reallocated
#      as if they'd never self-enrolled into a group at all.
#   -  "group_rosters_assigned.csv" - final group allocations along with a 
#      column describing any groups with issues.
#   -  "group_rosters_for_canvas_import.csv" - final group allocations with 
#      only the columns in original Canvas export. "Not grouped" and
#      "<3 members" students are excluded, so it's always safe to import
#      into Canvas as-is.
#   -  A message specifying if any student is "Not grouped" or in a group with
#      "<3 members", so you can follow these up manually.
#
# USAGE
#   Simplest case (tutorial numbers already complete):
#     source("run_group_allocation.R")
#     run_group_allocation()
#
#   If some students are missing a tutorial number, run_group_allocation()
#   will stop and print exactly what to do -- fill those in directly in
#   your console, then call finish_group_allocation(group_data), which
#   can be called again as many times as needed if anything's still missing.

library(dplyr)
library(stringr)
library(readr)

MIN_GROUP_SIZE   <- 3
MAX_GROUP_SIZE   <- 4
MAX_GROUP_NUMBER <- 200


# Extracts group number (NNN) from group_name (Group Assignment NNN)
extract_group_number <- function(group_name) {
  as.integer(str_match(group_name, "Group Assignment\\s+(\\d+)")[, 2])
}


# Given n ungrouped students in a tutorial, work out how many
# groups of 3 and 4 to form, using the fewest total groups possible,
# or return that no valid split exists at all
split_into_groups <- function(n) {
  if (n < MIN_GROUP_SIZE) return(NULL)
  k_min <- ceiling(n / MAX_GROUP_SIZE)
  k_max <- floor(n / MIN_GROUP_SIZE)
  if (k_min > k_max) return(NULL)
  k <- k_min
  c(
    fours = n - MIN_GROUP_SIZE * k,
    threes = MAX_GROUP_SIZE * k - n
  )
}

# Randomly reorder items in a list (students or group names).
# If only 1 item in list, return it as-is since no shuffling possible.
# Otherwise, shuffle the entire list.
safe_shuffle <- function(x) {
  if (length(x) <= 1) return(x)
  sample(x)
}

# Randomly pick n students/groups from a list without replacement 
# (so pick a subset)
# E.g., pick 2 donor groups out of 5 available in a tutorial
safe_sample_n <- function(x, n) {
  if (length(x) < n) {
    stop(
      "safe_sample_n: cannot draw ",
      n,
      " from a pool of ",
      length(x)
    )
  }
  if (length(x) == n) return(x)
  sample(x, n)
}


# ============================================================
# PART 1: READ THE RAW EXPORT, EXTRACT TUTORIAL NUMBERS, AND
# CHECK IF ANY ARE MISSING BEFORE ANYTHING ELSE PROCEEDS
# ============================================================

# Reads inactive_students.csv and removes any student from df who:
#   - appears in it (matched: df's user_id == inactive file's Student Id)
#   - AND currently has no group_name (NA) -- the usual sign they
#     haven't actually been active this semester
# A matching student who DOES have a group_name is left as is, since
# having enrolled into a group suggests they're probably still active.
remove_inactive_students <- function(df, inactive_path = "inactive_students.csv") {
  
  if (!file.exists(inactive_path)) {
    message(sprintf("No '%s' found -- skipping inactive-student removal.", inactive_path))
    return(df)
  }
  
  inactive <- read_csv(inactive_path, show_col_types = FALSE)
  
  # Match on ID, as text, so numeric-formatting differences between the
  # two files (e.g. stored as integer vs double) can't cause a silent miss
  inactive_ids <- as.character(inactive[["Student Id"]])
  is_inactive <- as.character(df$user_id) %in% inactive_ids
  
  to_remove <- is_inactive & is.na(df$group_name)
  kept_anyway <- is_inactive & !is.na(df$group_name)
  
  if (any(to_remove)) {
    message(sprintf(
      "Removed %d inactive, groupless student(s) found in '%s'.",
      sum(to_remove), inactive_path
    ))
  }
  if (any(kept_anyway)) {
    message(sprintf(
      "%d student(s) are listed in '%s' but already have a group_name -- left as is.",
      sum(kept_anyway), inactive_path
    ))
  }
  
  df[!to_remove, ]
}


prepare_group_data <- function(input_path = "group_rosters.csv",
                               inactive_path = "inactive_students.csv") {
  
  # Read in input CSV
  df <- read_csv(
    input_path,
    show_col_types = FALSE
  )
  
  # Remove inactive, groupless students BEFORE anything else -- so they
  # never trigger a missing-tutorial-number prompt for someone who's
  # about to be dropped anyway
  df <- remove_inactive_students(df, inactive_path)
  
  # Extract tutorial number from sections variable
  tut_match <- str_match(
    df$sections,
    "Tutorial\\s+\\d+\\s*\\((\\d+)\\)"
  )
  
  # Add tutorial_number as a variable
  df <- df |>
    mutate(
      tutorial_number = as.integer(tut_match[, 2])
    )
  
  # Save this working copy to your console as group_data, so you can
  # patch it by hand and pass it straight to finish_group_allocation()
  # without re-reading or re-extracting anything
  assign("group_data", df, envir = .GlobalEnv)
  
  # Find any student still missing a tutorial number
  missing <- which(is.na(df$tutorial_number))
  
  # If at least one student has NA tutorial number, stop here and print
  # exactly who, plus ready-to-run lines to fill each one in by hand
  # (Canvas doesn't let staff set this directly)
  if (length(missing) > 0) {
    cat(sprintf(
      "\n%d student(s) are missing a tutorial number. Canvas doesn't let\n",
      length(missing)
    ))
    cat("staff set this directly, so fill each one in by hand below, then\n")
    cat("call finish_group_allocation(group_data) to continue:\n\n")
    for (i in missing) {
      group_label <- if (is.na(df$group_name[i])) "NA" else df$group_name[i]
      cat(sprintf(
        "  group_data$tutorial_number[%d] <- <Enter tut num here for>   
           # %s, currently in group: %s\n",
        i, df$name[i], group_label
      ))
    }
    cat("\n")
    return(invisible(df))
  }
  
  # Once we have tutorial numbers for everyone, proceed without issues
  message("Every student already has a tutorial number -- ready to continue with finish_group_allocation(group_data).")
  invisible(df)
}


# ============================================================
# PART 2: DISSOLVE CROSS-TUTORIAL GROUPS, THEN RUN THE FULL
# RANDOMIZED ALLOCATION ON WHAT'S LEFT
# ============================================================

finish_group_allocation <- function(
    df,
    problem_groups_path = "problem_groups.csv",
    cleaned_path = "group_rosters_cleaned.csv",
    output_path = "group_rosters_assigned.csv",
    canvas_import_path = "group_rosters_for_canvas_import.csv",
    seed = 123, allow_self_enrolled_donor = FALSE) {
  
  # Guard: refuse to proceed until every tutorial number is filled in.
  # Same message as prepare_group_data() -- lets you call this again as
  # many times as needed while you're still filling things in
  missing <- which(is.na(df$tutorial_number))
  if (length(missing) > 0) {
    cat(sprintf("\n%d student(s) still missing a tutorial number:\n\n", length(missing)))
    for (i in missing) {
      group_label <- if (is.na(df$group_name[i])) "NA" else df$group_name[i]
      cat(sprintf(
        "  group_data$tutorial_number[%d] <- <Enter tut num here for>   
           # %s, currently in group: %s\n",
        i, df$name[i], group_label
      ))
    }
    cat("\nFill these in, then call finish_group_allocation(group_data) again.\n\n")
    return(invisible(df))
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  # Check within each group if all members are from the same tutorial
  # number. Only consider students who actually self-enrolled
  # (non-missing group_name)
  group_status <- df |>
    filter(!is.na(group_name)) |>
    group_by(group_name) |>
    summarise(spans_multiple = n_distinct(tutorial_number) > 1, .groups = "drop")
  
  dissolve_names <- group_status |> filter(spans_multiple) |> pull(group_name)
  
  # Only do anything (and only write a record) if at least one group
  # actually spans more than one tutorial
  if (length(dissolve_names) > 0) {
    
    # Save a CSV of problem_groups that only keeps groups "To be dissolved" --
    # a record of cross-tutorial groups for future reference
    problem_output <- df |>
      filter(group_name %in% dissolve_names) |>
      mutate(problem_groups = "To be dissolved") |>
      select(name, user_id, group_name, tutorial_number, problem_groups) |>
      arrange(group_name, tutorial_number)
    
    write_csv(problem_output, problem_groups_path)
    
    message(sprintf(
      "Wrote %d students across %d cross-tutorial group(s) to '%s' (dissolved, for your records).",
      nrow(problem_output), n_distinct(problem_output$group_name), problem_groups_path
    ))
    
    # Remove the existing group_name of every "To be dissolved" group's
    # members entirely, so all of them -- not just the ones from a
    # minority tutorial -- become groupless and get randomly reallocated
    # within their own tutorial, exactly as if they'd never self-enrolled
    # into a group at all
    df$group_name[df$group_name %in% dissolve_names] <- NA_character_
  }
  
  # This is the version of the data once every student has a tutorial
  # number and every cross-tutorial group has been dissolved -- write
  # it out for your records, regardless of whether any dissolving
  # actually happened this run
  write_csv(df, cleaned_path)
  message(sprintf("Wrote the cleaned roster to '%s'.", cleaned_path))
  
  # Hand off to the full randomized allocation below
  allocate_groups_within_budget(
    df,
    output_path = output_path,
    canvas_import_path = canvas_import_path,
    seed = seed,
    allow_self_enrolled_donor = allow_self_enrolled_donor
  )
}


# ============================================================
# PART 3: RANDOM ALLOCATION (per RULES above)
# Takes an in-memory data frame (already has tutorial_number, already
# has cross-tutorial groups dissolved) rather than reading a CSV.
# ============================================================

allocate_groups_within_budget <- function(
    df,
    output_path = "group_rosters_assigned.csv",
    canvas_import_path = "group_rosters_for_canvas_import.csv",
    seed = 123,
    allow_self_enrolled_donor = FALSE) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # df should already have tutorial_number by this point (Part 1 does
  # this) -- stop with a clear message if it doesn't, rather than
  # failing on a confusing error further down
  if (!"tutorial_number" %in% names(df)) {
    stop("allocate_groups_within_budget() expects df to already have a ",
         "'tutorial_number' column -- call prepare_group_data() (and, if ",
         "needed, finish_group_allocation()) first rather than calling ",
         "this directly on a raw roster.")
  }
  
  # Create a new column to log group issues
  df$group_issues <- NA_character_
  
  # Flag to show if student was self-enrolled or not. Taken AFTER
  # cross-tutorial groups were dissolved above, so a dissolved student is
  # correctly treated as never having self-enrolled at all from here on
  df$was_self_enrolled <- !is.na(df$group_name)
  
  # Log the original size of each pre-existing group 
  # (used below to decide who's protected from being donated)
  orig_sizes <- df |>
    filter(was_self_enrolled) |>
    count(
      group_name,
      name = "orig_size"
    )
  
  df <- df |>
    left_join(
      orig_sizes,
      by = "group_name"
    )
  
  # New column denoting if a student is eligible to be donated to other groups
  # Eligible donors are those not self-enrolled or self-enrolled alone only
  df$eligible_donor <-
    !df$was_self_enrolled |
    (df$was_self_enrolled & df$orig_size == 1)
  
  
  # ============================================================
  # CREATE A NEW GROUP NAME
  #
  # IMPORTANT:
  # Do NOT maintain a separate "available_numbers" list.
  #
  # Instead, look at df RIGHT NOW and find a number that is
  # genuinely not being used by any active group.
  #
  # This automatically makes a number reusable as soon as its
  # previous group disappears due to being dissolved.
  # ============================================================
  
  draw_new_group_name <- function() {
    
    # Pull out active group numbers
    active_numbers <- extract_group_number(
      df$group_name
    )
    
    # Remove NA group numbers (from students not self-enrolled in a group)
    active_numbers <- active_numbers[
      !is.na(active_numbers)
    ]
    
    # Returns group numbers that are available to use between 
    # 1 and MAX_GROUP_NUMBER (200 in this case)
    available <- setdiff(
      seq_len(MAX_GROUP_NUMBER),
      active_numbers
    )
    
    # If all MAX_GROUP_NUMBER (200) numbers are taken, code stops with error
    if (length(available) == 0) {
      stop(
        "There are already ",
        MAX_GROUP_NUMBER,
        " active Group Assignment numbers. ",
        "This means the current allocation genuinely requires ",
        "more than ",
        MAX_GROUP_NUMBER,
        " groups."
      )
    }
    
    # If group numbers are available, randomly pick an available number
    # and return "Group Assignment <that number>"
    paste(
      "Group Assignment",
      sample(available, 1)
    )
  }
  
  
  # Get the full sorted list of tutorial numbers in the data 
  # (will be used to loop over the tutorials)
  tutorials <- sort(
    unique(df$tutorial_number)
  )
  
  # ===============================================================
  #### MAIN LOOP BELOW ####
  # Each iteration happens within each tutorial, as per Rule 1
  # ===============================================================
  
  for (tut in tutorials) {
    
    # ============================================================
    # STEP 1:
    # SETUP
    # ============================================================
    
    # For current tutorial, get row numbers of students
    # (Used to ensure students in other tutorials are not touched)
    idx_tut <- which(
      df$tutorial_number == tut
    )
    
    # Given a group name, count how many students in that tutorial currently
    # have that name (Used to check how full a group is at any given moment)
    group_size <- function(gname) {
      
      sum(
        df$group_name[idx_tut] == gname,
        na.rm = TRUE
      )
    }
    
    
    # ============================================================
    # STEP 2:
    # SOLVE AND PACK
    #
    # (Works out an EXACT plan for every student who isn't already in
    # a full (4-person) group, rather than merging things greedily one
    # at a time. A greedy "merge whatever fits first" approach can make
    # a locally efficient choice -- e.g. pairing two existing 2-person
    # groups into a perfect 4 -- that turns out to be globally wrong
    # once you know exactly how many groupless students are actually
    # available. The search below is small enough (the number of
    # existing 1s, 2s, and 3s in one tutorial is never large) to just
    # try every reasonable combination and find one that leaves nobody
    # stranded, whenever such a combination exists at all.
    #
    # The building blocks:
    #   - each existing group of 2 is an ATOMIC pair -- both members
    #     always stay together, in whichever group they end up in
    #   - each existing group of 1, plus every groupless pool student,
    #     is a fully flexible "single" -- no partner to keep together
    #   - each existing group of 3 is already valid on its own, and
    #     can optionally take exactly one single to become a 4
    #   - existing groups of 4 are protected and don't enter this at
    #     all)
    # ============================================================
    
    # Get all existing group names in the tutorial, and their sizes
    existing_names <- unique(
      df$group_name[idx_tut]
    )
    existing_names <- existing_names[
      !is.na(existing_names)
    ]
    existing_sizes <- vapply(
      existing_names,
      group_size,
      integer(1)
    )
    
    two_unit_names <- existing_names[existing_sizes == 2]
    one_unit_names <- existing_names[existing_sizes == 1]
    three_names <- existing_names[existing_sizes == 3]
    
    # Every groupless student in this tutorial, plus every member of an
    # existing 1-person group (nobody to keep them paired with, so
    # they're just as flexible as a pool student), shuffled together
    pool <- idx_tut[
      is.na(df$group_name[idx_tut])
    ]
    one_unit_members <- idx_tut[
      df$group_name[idx_tut] %in% one_unit_names
    ]
    singles <- safe_shuffle(
      c(pool, one_unit_members)
    )
    
    m <- length(two_unit_names)
    s <- length(singles)
    n_fixed3 <- length(three_names)
    
    # Search for a feasible plan: how many of the existing 3-groups to
    # top up to 4 (n_topped), and how to place the m atomic pairs and
    # the remaining singles into groups of 3-4, using every reasonable
    # combination of:
    #   p = groups formed from (1 pair + 1 single)          = 3
    #   q = groups formed from (2 pairs)                     = 4
    #   r = groups formed from (1 pair + 2 singles)           = 4
    #   a = brand-new groups formed from 3 singles            = 3
    #   b = brand-new groups formed from 4 singles            = 4
    # Returns NULL if no combination places everyone.
    # Among every feasible combination, keep the one that minimizes the
    # total number of groups formed -- i.e. prefer merging more pairs
    # together (higher q) and forming fewer brand-new groups (lower
    # a + b) -- rather than just taking the first one found, so the
    # 200-group budget keeps getting the same benefit it always did.
    plan <- NULL
    best_score <- Inf
    
    for (n_topped in 0:n_fixed3) {
      
      s2 <- s - n_topped
      if (s2 < 0) next
      
      for (p in 0:m) {
        
        for (q in 0:((m - p) %/% 2)) {
          
          r <- m - p - 2 * q
          if (r < 0) next
          
          singles_left <- s2 - p - 2 * r
          if (singles_left < 0) next
          
          for (a in 0:(singles_left %/% 3)) {
            
            rem <- singles_left - 3 * a
            
            if (rem %% 4 == 0) {
              
              b <- rem / 4
              
              # fewer surviving pair-groups (m - q) and fewer brand-new
              # groups (a + b) is better; the first valid 'a' found is
              # already the smallest for this (n_topped, p, q), which
              # already minimizes a + b for this branch -- so one check
              # here is enough, no need to keep trying larger a
              score <- (m - q) + (a + b)
              
              if (score < best_score) {
                best_score <- score
                plan <- list(
                  n_topped = n_topped,
                  p = p,
                  q = q,
                  r = r,
                  a = a,
                  b = b
                )
              }
              break
            }
          }
        }
      }
    }
    
    # If a feasible plan was found, carry it out. Existing groups keep
    # their own name wherever possible (no new number spent); only the
    # brand-new groups (a, b) draw a fresh number.
    if (!is.null(plan)) {
      
      two_unit_names_shuffled <- safe_shuffle(two_unit_names)
      three_names_shuffled <- safe_shuffle(three_names)
      
      next_pair <- 1
      next_single <- 1
      
      # Top up n_topped existing 3-groups to 4, one single each
      for (i in seq_len(plan$n_topped)) {
        g <- three_names_shuffled[i]
        df$group_name[singles[next_single]] <- g
        next_single <- next_single + 1
      }
      
      # p: pair + 1 single -> group of 3 (keeps the pair's own name)
      if (plan$p > 0) {
        for (i in seq_len(plan$p)) {
          g <- two_unit_names_shuffled[next_pair]
          next_pair <- next_pair + 1
          df$group_name[singles[next_single]] <- g
          next_single <- next_single + 1
        }
      }
      
      # q: pair + pair -> group of 4 (keeps the first pair's name)
      if (plan$q > 0) {
        for (i in seq_len(plan$q)) {
          g1 <- two_unit_names_shuffled[next_pair]
          next_pair <- next_pair + 1
          g2 <- two_unit_names_shuffled[next_pair]
          next_pair <- next_pair + 1
          donor_idx <- idx_tut[
            df$group_name[idx_tut] == g2
          ]
          df$group_name[donor_idx] <- g1
        }
      }
      
      # r: pair + 2 singles -> group of 4 (keeps the pair's own name)
      if (plan$r > 0) {
        for (i in seq_len(plan$r)) {
          g <- two_unit_names_shuffled[next_pair]
          next_pair <- next_pair + 1
          df$group_name[singles[next_single]] <- g
          next_single <- next_single + 1
          df$group_name[singles[next_single]] <- g
          next_single <- next_single + 1
        }
      }
      
      # a: brand-new groups of 3 singles
      if (plan$a > 0) {
        for (i in seq_len(plan$a)) {
          take <- singles[next_single:(next_single + 2)]
          next_single <- next_single + 3
          df$group_name[take] <- draw_new_group_name()
        }
      }
      
      # b: brand-new groups of 4 singles
      if (plan$b > 0) {
        for (i in seq_len(plan$b)) {
          take <- singles[next_single:(next_single + 3)]
          next_single <- next_single + 4
          df$group_name[take] <- draw_new_group_name()
        }
      }
      
      pool <- integer(0)  # everyone placed
      
    } else {
      
      # No feasible plan using existing groups + pool alone. Leave
      # everything untouched here -- Step 3 (RESCUE) still gets a
      # chance to help by borrowing from an already-full group, and
      # anything still left after that is flagged, further down.
    }
    
    
    # ==========================================================
    # STEP 3:
    # RESCUE 1-2 LEFTOVER STUDENTS
    #
    # (If Step 2 couldn't find any fully feasible plan and 1-2
    # students are left stranded, rescue them by pulling out spare
    # members from an existing full group)
    # ==========================================================
    
    # PATCH 2: only three pool sizes cannot split into groups of 3 and 4 --
    # 1, 2 and 5. Everything else needs no donors at all.
    #   pool = 1 -> 2 donors -> 3 students  = one group of 3
    #   pool = 2 -> 1 donor  -> 3 students  = one group of 3
    #   pool = 5 -> 1 donor  -> 6 students  = two groups of 3
    # The original triggered on 1 or 2 only, so a pool of 5 was never
    # rescued. It also gave every rescued student the same group name,
    # which would make a group of 6 in the pool = 5 case -- hence the
    # split below.
    needed_donors <- switch(
      as.character(length(pool)),
      "1" = 2L,
      "2" = 1L,
      "5" = 1L,
      0L
    )
    
    if (length(pool) > 0 && needed_donors > 0) {
      
      names_now <- unique(df$group_name[idx_tut])
      names_now <- names_now[!is.na(names_now)]
      
      full_groups <- names_now[
        vapply(names_now, group_size, integer(1)) == MAX_GROUP_SIZE
      ]
      
      # A member may be moved if they did not choose their group, or chose
      # it alone. With allow_self_enrolled_donor = TRUE, any member of a
      # full group may be moved: the group stays valid at 3, but one
      # student is taken away from people they chose.
      donor_ok <- function(i) {
        if (allow_self_enrolled_donor) TRUE else df$eligible_donor[i]
      }
      
      
      
      # NOTE: `%in%` rather than `==` on the next line. idx_tut includes
      # groupless students whose group_name is NA, and `NA == g` returns NA,
      # which indexes an NA element rather than dropping it. `%in%` returns
      # FALSE for NA, so those rows are excluded properly.
      has_donor <- vapply(
        full_groups,
        function(g) any(vapply(idx_tut[df$group_name[idx_tut] %in% g], #`%in%` not `==`, for the NA reason noted above
                               donor_ok, logical(1))),
        logical(1)
      )
      full_groups <- full_groups[has_donor]
      
      if (length(full_groups) >= needed_donors) {
        
        donor_groups <- safe_sample_n(full_groups, needed_donors)
        donated <- integer(0)
        
        for (g in donor_groups) {
          members <- idx_tut[df$group_name[idx_tut] %in% g]
          eligible_members <- members[vapply(members, donor_ok, logical(1))]
          if (length(eligible_members) == 0) next
          donated <- c(donated, safe_sample_n(eligible_members, 1))
        }
        
        # safe_sample_n() on an empty or all-NA set returns NA, and length(NA)
        # is 1 -- so without this filter the guard below passes with a phantom
        # donor, and one real student ends up alone in a group of 2. This was
        # the cause of the invalid Tutorial 37 group during testing.
        donated <- donated[!is.na(donated)]
        # Only proceed if we actually got every donor we needed. Without them
        # the pool still cannot split into 3s and 4s, and forming groups
        # anyway produces an invalid one.
        if (length(donated) == needed_donors) {
          
          rescued <- safe_shuffle(c(pool, donated))
          sp <- split_into_groups(length(rescued))   # named: fours, threes
          
          nxt <- 1
          for (i in seq_len(sp[["fours"]])) {
            df$group_name[rescued[nxt:(nxt + 3)]] <- draw_new_group_name()
            nxt <- nxt + 4
          }
          for (i in seq_len(sp[["threes"]])) {
            df$group_name[rescued[nxt:(nxt + 2)]] <- draw_new_group_name()
            nxt <- nxt + 3
          }
          
          message(sprintf(
            "Tutorial %s: rescued %d student(s) using %d donor(s).",
            tut, length(pool), needed_donors
          ))
          
          pool <- pool[0]
        }
      }
    }
    
    
    # ==========================================================
    # STEP 4:
    # ANYONE LEFT OVER
    # 
    # (Flag anyone still left groupless as "Not grouped".
    #  This will be shown in the output CSV for manual checking)
    # ==========================================================
    
    if (length(pool) > 0) {
      
      df$group_issues[pool] <-
        "Not grouped"
    }
    
    
    # ==========================================================
    # STEP 5:
    # FLAG ANY GROUP STILL UNDER 3
    #
    # (These students are flagged as being in groups with
    # "<3 members" in the output CSV for manual checking)
    # ==========================================================
    
    # Get distinct group names again (after all above steps done)
    names_final <- unique(
      df$group_name[idx_tut]
    )
    names_final <- names_final[
      !is.na(names_final)
    ]
    
    # Only keep groups that are sized below 3
    short <- names_final[
      vapply(
        names_final,
        group_size,
        integer(1)
      ) < MIN_GROUP_SIZE
    ]
    
    # Only proceed if at least 1 group is under-sized
    if (length(short) > 0) {
      
      # Flag all students in the under-sized group as having "<3 members"
      df$group_issues[
        idx_tut[
          df$group_name[idx_tut] %in% short
        ]
      ] <- "<3 members"
    }
  }
  
  #=============================================================
  # END OF LOOP WITHIN ONE TUTORIAL.
  # SCRIPT GOES BACK TO STEP 1 AND REPEATS FOR THE NEXT TUTORIAL
  #=============================================================
  
  
  # ============================================================
  # OUTPUT
  # ============================================================
  
  # Clean output by keeping columns needed and sorting
  output <- df |>
    select(
      -was_self_enrolled,
      -orig_size,
      -eligible_donor
    ) |>
    arrange(
      tutorial_number,
      group_name
    )
  
  # Write "group_rosters_assigned.csv" to working directory
  # Contains students "Not grouped" or in "<3 members" groups
  # For manual checking
  write_csv(
    output,
    output_path
  )
  
  # Create dataset for Canvas upload
  # Excludes "Not grouped" students (blank group_name isn't a valid
  # Canvas row) AND "<3 members" students, so nothing undersized ever
  # gets uploaded without you actively deciding to. Both are still
  # flagged in group_issues in the full output above, and in the
  # message below, for manual follow-up.
  canvas_ready <- output |>
    filter(
      !is.na(group_name),
      is.na(group_issues)
    ) |>
    select(
      any_of(
        c(
          "name",
          "canvas_user_id",
          "user_id",
          "login_id",
          "sections",
          "group_name",
          "canvas_group_id",
          "group_id"
        )
      )
    ) |> 
    # Replace NAs to blanks to not cause issues on Canvas after importing
    mutate(across(everything(), as.character)) |>
    mutate(across(everything(), ~ tidyr::replace_na(.x, "")))   

  
  # Write CSV for Canvas upload, to working directory
  write_csv(
    canvas_ready,
    canvas_import_path
  )
  
  # Count how many distinct groups in final output (used in final message)
  total_groups <- length(
    unique(
      output$group_name[
        !is.na(output$group_name)
      ]
    )
  )
  
  # Print final message specifying "Not grouped" or "<3 members" cases
  message(
    sprintf(
      paste(
        "Wrote %d rows to '%s' (%d students not grouped,",
        "%d students in groups of <3).",
        "Used %d / %d group numbers.",
        "Wrote %d rows to '%s', ready for Canvas import."
      ),
      nrow(output),
      output_path,
      sum(
        output$group_issues ==
          "Not grouped",
        na.rm = TRUE
      ),
      sum(
        output$group_issues ==
          "<3 members",
        na.rm = TRUE
      ),
      total_groups,
      MAX_GROUP_NUMBER,
      nrow(canvas_ready),
      canvas_import_path
    )
  )
  
  # Return the full output, the Canvas-ready file, and the group count as
  # a named list (e.g. result$full), so they're available without
  # re-reading either CSV. invisible() keeps this from auto-printing.
  invisible(
    list(
      full = output,
      canvas_ready = canvas_ready,
      total_groups = total_groups
    )
  )
}


# ============================================================
# PART 4: RUN EVERYTHING IN ONE CALL
# ============================================================

# Chains Part 1 and Part 2 together, and only pauses if it genuinely has to
# (i.e. some student is missing a tutorial number). Once tutorial numbers
# are complete for the semester, this does the whole pipeline in one go.
run_group_allocation <- function(input_path = "group_rosters.csv",
                                 inactive_path = "inactive_students.csv", ...) {
  df <- prepare_group_data(input_path, inactive_path)
  if (any(is.na(df$tutorial_number))) {
    invisible(df)  # prepare_group_data() already printed what to do
  } else {
    finish_group_allocation(df, ...)
  }
}