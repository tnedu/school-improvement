library(haven)
library(janitor)
library(lubridate)
library(magrittr)
library(openxlsx)
library(rlang)
library(tidyverse)

# Functions ----

strip_attributes <- function(x) {
  if(is.data.frame(x)) {
    attributes(x)$spec <- NULL
  }
  return(x)
}

# Input ----

input_paths <-
  list.files(
    "data/input",
    full.names = T,
    pattern = "student-level-file"
  ) %>%
  keep(~ !str_detect(.x, "2015|2016"))

vars_to_drop <- c(
  "system_name",
  "school_name",
  "subject",
  "performance_level",
  "proficiency_level",
  "last_name",
  "first_name",
  "bhn_group",
  # Potentially use teacher data for mediating variables.
  "teacher_of_record_tln"
)

student_input <-
  map(
    2019:2017,
    ~ str_c("data/input/", .x, "-student-level-file.csv") %>%
      read_csv() %>%
      strip_attributes() %>%
      select(-one_of(vars_to_drop)) %>%
      distinct()
  ) %>%
  set_names(nm = str_c("sy", 2019:2017))

# student_input <-
#   input_paths %>%
#   map_if(
#     ~ is.character(.x) & str_detect(.x, ".csv$"),
#     read_csv
#   ) %>%
#   map_if(
#     ~ is.character(.x) & str_detect(.x, ".dta$"),
#     read_dta
#   ) %>%
#   map(
#     ~ .x %>%
#       strip_attributes()
#   ) %>%
#   set_names(nm = str_c("sy", str_trunc(input_paths, 4, "right", "")))

# Explore student input ----

map(student_input, names)

map(student_input, ~ nrow(.x) == nrow(distinct(.x)))

# Identify composite keys. In all data sets, all or almost all observations
# are unique by system, school, subject, and student ID.

student_input %>%
  map_at(
    "sy2019",
    ~ .x %>%
      summarize(
        n = n(),
        n_key = n_distinct(
          system,
          school,
          original_subject,
          state_student_id
        )
      )
  ) %>%
  map_at(
    "sy2018",
    ~ .x %>%
      summarize(
        n = n(),
        n_key = n_distinct(
          system,
          school,
          original_subject,
          scale_score,
          state_student_id,
          grade,
          race
        )
      )
  ) %>%
  map_at(
    "sy2017",
    ~ .x %>%
      summarize(
        n = n(),
        n_key = n_distinct(
          system,
          school,
          original_subject,
          scale_score,
          state_student_id,
          race
        )
      )
  )

map(
  student_input,
  ~ summarize(.x, n_schools = n_distinct(system, school))
)

student_input %>%
  map(~ count(.x, test, original_subject)) %>%
  walk(View)

student_input %>%
  keep(~ "semester" %in% names(.x)) %>%
  map(~ count(.x, semester))

student_input %>%
  map_if(
    ~ "original_proficiency_level" %in% names(.x),
    ~ rename(.x, original_performance_level = original_proficiency_level)
  ) %>%
  map(~ count(.x, original_performance_level))

map(student_input, ~ summary(.x$scale_score))

map(student_input, ~ count(.x, enrolled, tested, valid_test))

map(student_input, ~ count(.x, grade))

# Gender is missing for 2017 and 2018.

count(student_input[["sy2019"]], gender)

student_input %>%
  map_at(
    "sy2019",
    ~ rename(.x, race = reported_race)
  ) %>%
  map(~ count(.x, race))

# Prepare student input for analysis ----

student <-
  student_input %>%
  map_at(
    "sy2019",
    ~ rename(.x, race = reported_race)
  ) %>%
  map_at(
    "sy2017",
    ~ rename(.x, original_performance_level = original_proficiency_level)
  ) %>%
  reduce(bind_rows) %>%
  filter(
    valid_test == 1,
    grade %in% 3:12,
    !is.na(race),
    race != "Unknown"
  ) %>%
  mutate_at(
    vars(test),
    funs(
      case_when(
        str_detect(., "Alt") ~ "Alt",
        . == "Achievement" ~ "TNReady",
        T ~ .
      )
    )
  ) %>%
  mutate_at(
    vars(original_performance_level),
    funs(
      case_when(
        str_detect(., "Advanced") ~ "Mastered",
        str_detect(., "Below Basic") ~ "Below",
        str_detect(., "Basic") ~ "Approaching",
        str_detect(., "Proficient") ~ "On Track",
        T ~ .
      )
    )
  ) %>%
  mutate_at(
    vars(race),
    funs(
      case_when(
        str_detect(., "American Indian") ~ "N",
        str_detect(., "Asian") ~ "A",
        str_detect(., "Black") ~ "B",
        str_detect(., "Hispanic") ~ "H",
        str_detect(., "Hawaiian") ~ "P",
        str_detect(., "White") ~ "W"
      )
    )
  )

# Explore descriptive statistics and variation of model variables ----

# Variation in scale scores at the bottom of the achievement distribution
# (floor effects where growth equals 0) and at the top (ceiling effects), by
# treatment/comparison group
