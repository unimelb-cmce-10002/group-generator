# Self-Selected Group Allocation

## Overview

This script starts with a list of students exported from Canvas who have either: (1) self-selected into groups or (2) have chosen to remain ungrouped.

The script then randomly allocates the ungrouped students to either: (1) the existing groups if they have space or (2) new groups.

Overall, the goal is to keep students within their assigned tutorial and attempt to produce valid 3–4 person groups while making as few changes to existing self-selected groups as possible.

## How to "run" this script

The main script is: `run_group_allocation.R`

1) Run the main script within R/RStudio after the required input files (see below) have been placed in the working directory.
2) Run the function `run_group_allocation()` in the Console.
3) If any student has no valid tutorial number, the script will break with a message asking you to manually fill this in.
4) Simply use the guiding code provided in the message to fill in the student's tutorial number. Example:
     `group_data$tutorial_number[274] <- <Enter tut num here for>`   
           `# Doe, Jane, currently in group: Group Assignment 29`
   
   In the above code, 274 is the row number of the student with no tutorial number.
   So, entering the tutorial number and running the first line of the code above overwrites the NA with say 16.
6) After entering all required tutorial numbers manually, run `finish_group_allocation(group_data)` in the Console to continue the script from where it broke.
7) At the end, the output CSVs (see below) will be saved to the working directory.
8) Some messages will also be printed to the Console and can be used for checking if any students have not been grouped or are in an under-sized group.

## Key rules

The allocation process follows these rules:

* Students **never move between tutorials**.
* Existing groups of **4 or 3 students are protected** and are not dissolved.
* Existing groups of **1 or 2 students** can be combined with other students/groups where necessary.
* Existing groups of **1 or 2 students** are treated as units where possible when forming new groups.
* Group sizes must ultimately be between **3 and 4 students**.
* New groups are assigned an available Group Assignment number between **1 and 200**.
* Group Assignment numbers can be reused once the original group using that number has been dissolved.
* The allocation includes checks for situations where a valid 3–4 person allocation cannot be achieved.
* The allocation uses randomisation. A default seed of 123 is used so that the allocation can be reproduced when required. The seed can be changed in the script if a different random allocation is required.

## Input files

The script expects the input files below to be available in the working directory:

### `group_rosters.csv`

The main Canvas roster file containing student and group information. This is exported from Canvas > People > Group Assignment.

This file contains information such as:

* Student identifiers
* Tutorial numbers stored as free text in the sections variable
* Existing group names in the format Group Assignment 1, Group Assignment 2, and so on up to 200.

### `inactive_students.csv`

An optional file containing students who should be excluded from the allocation.

Inactive students are students who have not appeared for the mid-semester test and have not applied for special consideration.

NOTE: An inactive student, as defined above, who has self-enrolled into a group is not treated as "inactive" and is allowed to remain grouped.

## Output files

The script produces several output files for checking, along with a CSV for Canvas upload and grouping completion.

### `problem_groups.csv`

Contains information about groups that contained students from >1 tutorial in the original student data exported from Canvas.

Can be used to verify dissolved groups when students raise concerns about being dissolved from their self-selected group.

If no problem groups exist, an empty CSV is produced (this redundancy in the script can be patched in the future).

### `group_rosters_cleaned.csv`

List of students after removing cross-tutorial groups and ensuring all students have a valid tutorial number.

Serves as the starting point for group allocations.

### `group_rosters_assigned.csv`

Contains the final group allocations. 

The final column of this CSV also notes if any students are "Not grouped" yet, or are in a group with "<3 members". 

This CSV can be used to diagnose why these students are not in valid groups within their own tutorial by using `group_by(tutorial_number)`.

This also means that this CSV can be used to check edge cases and improve the script.

### `group_rosters_for_canvas_import.csv`

Contains the final allocation in a format intended for importing the groups back into Canvas.

IMPORTANT: 

- Any student "Not grouped" is not present in this Canvas-ready CSV output, and will have to be manually grouped on Canvas.
- Any student(s) left in groups of <3 after the allocation above has run are not present in this Canvas-ready CSV output. So, they will remain in their original invalid groups and need manual grouping to fix.
