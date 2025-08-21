# ===============================================================
# group_assignment_generator.R
# Utilities for extracting tutorial groups, generating assignment
# groups (max 4s, 3s only as needed), and verifying group sizes.
# ===============================================================

# NOTE: This file uses qualified calls (dplyr::, stringr::, assertr::, rlang::)
# so you don't *have* to library() them globally—just ensure the packages
# are installed.

# -------------------------------
# 1) extract_tutorial_group()
# -------------------------------

#' Extract the Tutorial Group Number from a Canvas "sections" field
#'
#' Parses strings that contain a Tutorial in the form `"Tutorial <n> (<g>)"`
#' and returns the `<g>` (the number inside the parentheses) as an integer.
#' Works regardless of order when multiple items are comma-separated (e.g.,
#' `"Lecture A, Tutorial 3 (2)"` or `"Tutorial 1 (4), Lecture B"`).
#'
#' @param sections A character vector (e.g., a column from your roster) that
#'   includes a substring like `"Tutorial 1 (2)"`.
#'
#' @return An integer vector of the same length as `sections`, containing the
#'   tutorial group number (or `NA_integer_` if no tutorial pattern is found).
#'
#' @examples
#' # x <- c("Lecture A, Tutorial 1 (2)", "Tutorial 3 (1), Lecture B", "Lecture only")
#' # extract_tutorial_group(x)
#' # [1] 2 1 NA
extract_tutorial_group <- function(sections) {
    m <- stringr::str_match(
        sections,
        stringr::regex("Tutorial\\s*\\d+\\s*\\((\\d+)\\)", ignore_case = TRUE)
    )
    # m[, 2] is the first capture group (number inside Tutorial(...))
    out <- suppressWarnings(as.integer(m[, 2]))
    out
}

# -------------------------------
# 2) group_generator()
# -------------------------------

#' Generate assignment groups within a stratifying variable, maximizing 4-person groups
#'
#' Randomly shuffles rows within each level of `grouping_var` and partitions each level
#' into groups of size **3 or 4**, with **as many 4-person groups as possible** and 3s
#' used only when required by arithmetic.
#'
#' This enforces feasible 3/4 packings per level. It errors on impossible sizes
#' (e.g., n in {1, 2, 5} for a given level).
#'
#' @param data A data frame (e.g., your roster).
#' @param grouping_var A column used to stratify before forming groups (tidy-eval).
#'   Example: `tutorial_group`.
#' @param grp_size Preferred size in {3, 4}. Default `4`. This indicates preference
#'   only; the algorithm will still ensure the final sizes are all 3 or 4 with as many
#'   4s as possible.
#' @param seed Integer seed for reproducible shuffling. Default `42`.
#' @param name_prefix Optional string; if provided, appends a `group_name` column in the
#'   form `"<name_prefix> Group <id>"`.
#' @param min_size Minimum allowed size for groups. Must be 3 or 4. Default `3`.
#'
#' @return The input data with an added integer column `group_id` (1..K within each
#'   level of `grouping_var`). If `name_prefix` is provided, also returns `group_name`.
#'
#' @examples
#' # df <- data.frame(
#' #   student = paste0("S", 1:23),
#' #   section = rep(c("A","B"), times = c(11,12))
#' # )
#' # out <- group_generator(df, section, grp_size = 4, seed = 123, name_prefix = "Proj")
#' # out |>
#' #   dplyr::count(section, group_id)
group_generator <- function(data, grouping_var, grp_size = 4, seed = 42,
                            name_prefix = NULL, min_size = 3) {
    stopifnot(grp_size %in% c(3, 4), min_size %in% c(3, 4))
    # Ensure preference >= min_size (so "as many 4s as possible" works as intended)
    if (grp_size < min_size) {
        tmp <- grp_size; grp_size <- min_size; min_size <- tmp
    }
    # This generator is specifically for sizes {3,4}
    if (!all(sort(c(grp_size, min_size)) == c(3, 4))) {
        stop("This function currently supports group sizes in {3,4} only.")
    }
    
    set.seed(seed)
    
    # Partition n into as many 4s as possible; use 3s only when required.
    pack_sizes <- function(n) {
        # Impossible to pack n ∈ {1, 2, 5} with only 3s and 4s
        if (n %in% c(1, 2, 5)) {
            stop("Cannot partition n = ", n, " into groups of size 3 or 4.")
        }
        r <- n %% 4
        if (r == 0) {
            rep(4, n / 4)
        } else if (r == 1) {
            # replace two 4s (+1) with three 3s
            c(rep(4, (n - 9) / 4), 3, 3, 3)
        } else if (r == 2) {
            # replace one 4 (+2) with two 3s
            c(rep(4, (n - 6) / 4), 3, 3)
        } else { # r == 3
            # just add one 3
            c(rep(4, (n - 3) / 4), 3)
        }
    }
    
    data %>%
        dplyr::group_by({{ grouping_var }}) %>%
        dplyr::group_modify(function(.x, .key) {
            .x <- dplyr::slice_sample(.x, prop = 1)  # shuffle within stratum
            n <- nrow(.x)
            sizes <- pack_sizes(n)
            .x$group_id <- rep(seq_along(sizes), times = sizes)
            .x
        }) %>%
        dplyr::ungroup() %>%
        { 
            if (!is.null(name_prefix)) {
                dplyr::mutate(., group_name = paste0(name_prefix, " Group ", .data$group_id))
            } else {
                .
            }
        }
}

# -------------------------------
# 3) pass_size_check()
# -------------------------------

#' Verify group sizes using assertr
#'
#' Checks that every `(grouping_var, group_id)` has size \eqn{\ge} `min_size` and,
#' by default, that sizes are only in `{3, 4}`. Uses \pkg{assertr} so you can plug
#' in your preferred `error_fun` / `success_fun` handlers.
#'
#' @param data A data frame that already contains a grouping column (default `"group_id"`).
#' @param grouping_var The stratifying variable used when forming groups (tidy-eval).
#' @param group_col The name of the column that identifies groups. Default `"group_id"`.
#' @param min_size Minimum allowed size (default `3`).
#' @param allowed A numeric vector of allowed sizes. Default `c(3, 4)`. Set `NULL` to skip.
#' @param error_fun Error handler passed to \code{assertr::verify}. Default \code{assertr::error_stop}.
#' @param success_fun Success handler passed to \code{assertr::verify}. Default \code{assertr::success_continue}.
#'
#' @return Invisibly returns `TRUE` on success. If a check fails, the chosen
#'   `error_fun` determines behavior (by default, stop with an error).
#'
#' @examples
#' # out <- group_generator(df, section, grp_size = 4)
#' # pass_size_check(out, section)  # ensures all sizes are in {3,4} and >= 3
pass_size_check <- function(data, grouping_var,
                            group_col   = "group_id",
                            min_size    = 3,
                            allowed     = c(3, 4),
                            error_fun   = assertr::error_stop,
                            success_fun = assertr::success_continue) {
    
    counts <- data %>%
        dplyr::count({{ grouping_var }}, !!rlang::sym(group_col), name = "n")
    
    # ≥ min_size
    counts <- counts %>%
        assertr::verify(n >= min_size, error_fun = error_fun, success_fun = success_fun)
    
    # allowed sizes (optional)
    if (!is.null(allowed)) {
        counts <- counts %>%
            assertr::verify(n %in% allowed, error_fun = error_fun, success_fun = success_fun)
    }
    
    invisible(TRUE)
}
