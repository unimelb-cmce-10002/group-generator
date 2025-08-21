# Group Assignment Generator

This repository contains a reproducible workflow for creating **Canvas-ready assignment groups** from a course roster.

It uses:
- **R + tidyverse** for data wrangling
- **assertr** for validation
- **Canvas roster CSV exports** as the raw input

## Repository Layout

```
project/
├─ config/
│  ├─ overrides.sample.csv   # Example format for manual overrides (tracked)
│  └─ overrides.csv          # Real overrides (ignored by git)
├─ data/
│  └─ roster.csv             # Canvas People export (not tracked)
├─ outputs/
│  └─ assignment_groups.csv  # Generated groups ready for Canvas re-upload
├─ R/
│  └─ group_assignment_generator.R # Core functions
├─ main.R                    # Driver script
└─ README.md                 # This file
```

## Workflow

1. **Export Canvas roster**
   - In Canvas, go to **People → Import → Download Roster**.
   - Save as `data/roster.csv`.

2. **Run the main script**
   ```bash
   Rscript main.R
   ```

3. **Outputs**
   - `outputs/assignment_groups.csv`: CSV with the original roster columns only (Canvas-safe).

## Manual Overrides

Sometimes students switch sections late or need to be excluded.  
Handle these via `config/overrides.csv`.

- Copy the example:
  ```bash
  cp config/overrides.sample.csv config/overrides.csv
  ```
- Edit `overrides.csv` with the following columns:

| column                   | meaning                                                |
|---------------------------|--------------------------------------------------------|
| `login_id`               | Canvas login ID (matches `login_id` in roster)         |
| `tutorial_group_override`| Integer; replace student’s tutorial group (blank = none)|
| `remove`                 | `TRUE/FALSE`; exclude student from group assignment     |
| `comment`                | Free text reason                                       |
| `active`                 | `TRUE/FALSE`; only active overrides are applied         |

⚠️ `config/overrides.csv` is **.gitignored** → never gets committed.

## Core Functions

Located in [`R/group_assignment_generator.R`](R/group_assignment_generator.R):

1. **`extract_tutorial_group(sections)`**
   - Extracts the group number inside `Tutorial n (g)` from a Canvas `sections` column.
   - Example: `"Lecture A, Tutorial 3 (2)" → 2`.

2. **`group_generator(data, grouping_var, grp_size = 4, min_size = 3)`**
   - Shuffles students within each tutorial and partitions them into groups of 3 or 4.
   - Maximizes 4-person groups; uses 3s only if required by arithmetic.
   - Errors if impossible (n in {1, 2, 5}).

3. **`pass_size_check(data, grouping_var, ...)`**
   - Verifies all `(tutorial, group_id)` sizes are valid.
   - Default: all groups ≥ 3 and only sizes 3 or 4.
   - Uses `assertr::verify()` for consistent checks.

## Example Usage

```r
# load helpers
source("R/group_assignment_generator.R")

# read roster
df <- readr::read_csv("data/roster.csv")

# extract tutorial groups
df <- df %>%
  dplyr::mutate(tutorial_group = extract_tutorial_group(sections))

# apply manual overrides
df <- apply_overrides(df)   # if using overrides.csv

# generate groups
groups <- group_generator(df, tutorial_group, grp_size = 4, min_size = 3, seed = 76)

# validate
pass_size_check(groups, tutorial_group)

# save Canvas-ready CSV
upload_df <- groups %>% dplyr::select(dplyr::any_of(names(df)))
readr::write_csv(upload_df, "outputs/assignment_groups.csv")
```

## Safety Notes

- **Never commit student identifiers**: roster CSVs and overrides are git-ignored.
- If sensitive data is accidentally committed, rewrite history using `git filter-repo` with replacement rules (see repo notes).
- Always double-check group sizes with `pass_size_check()` before saving.

## Requirements

- R (≥ 4.1 recommended)
- Packages: `dplyr`, `readr`, `stringr`, `tidyr`, `assertr`, `rlang`

Install once with:
```r
install.packages(c("dplyr","readr","stringr","tidyr","assertr","rlang"))
```

## Authors

Lachlan Deer and Patrick Ferguson @ University of Melbourne  
Adapted and maintained by teaching staff for safe, reproducible group assignment generation.


## License

[MIT License](LICENSE.md)

Please drop us a note if you use this repo in a different course or institution. 
A hat-tip in your course material would also be greatly appreciated