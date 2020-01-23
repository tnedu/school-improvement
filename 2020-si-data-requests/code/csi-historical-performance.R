# Historical Performance of Priority or CSI Schools
# Josh Carson
# Requested by Andrea Thorsbakken
# Last updated 2020-01-23

# Provide historical outcome data on Priority or CSI schools for a Strategic
# Plan report.

library(magrittr)
library(openxlsx)
library(tidyverse)

# Functions ----

strip_attributes <- function(df) {
  attributes(df)$spec <- NULL
  return(df)
}

# Input ----

accountability <-
  list(
    sy2019 = "2019_final_accountability_files/school_accountability_file.csv",
    sy2018 = "2018_final_accountability_files/2018_school_accountability_file.csv",
    sy2017 = "2017_final_accountability_files/school_numeric_2017_JW_10242017.csv"
  ) %>%
  map(
    ~ str_c("N:/ORP_accountability/data/", .x) %>%
      read_csv() %>%
      strip_attributes()
  )

schools_priority_2018 <- read.xlsx(
  "N:/ORP_accountability/data/2018_final_accountability_files/school_designations_file.xlsx",
  sheet = "Priority"
)

schools_priority_2015 <-
  read.xlsx(
    "N:/ORP_accountability/projects/2014/2014_Final_school_accountability/2015_priority_list_withSR.xlsx",
    startRow = 2
  ) %>%
  janitor::clean_names() %>%
  filter(!is.na(school_number)) %>%
  mutate_at(vars(district_number), as.numeric)

schools_priority_2015_minus_exits <-
  read_csv("N:/ORP_accountability/projects/2015/2015_school_coding/Output/priority_schools_not_exiting_ap.csv") %>%
  filter(
    priority == 1,
    priority_exit == 0,
    designation_ineligible_priority == 0
  )

schools_priority_2012 <-
  read.xlsx("N:/ORP_accountability/data/2012/raw/school_designations_simple2012.xlsx") %>%
  janitor::clean_names() %>%
  filter(status == "Priority")

# The file below contains historical information about Priority schools, most
# notably the former district numbers, former school numbers, and intervention
# histories of ASD schools. This file was copied from here:
# N:\ORP_accountability\projects\Josh\_archive\school_lists\schools_priority_180703_jc.xlsx

schools_priority_historical <- read.xlsx(
  "data/schools-priority-with-id-histories.xlsx",
  sheet = "schools"
)

# Explore input ----

count(schools_priority_2012, district_number, sort = T)

# Shelby County ID crosswalk ----

# Shelby County's district number changed.

sort(unique(schools_priority_2012$district_number))
sort(unique(schools_priority_2015$district_number))
sort(unique(schools_priority_2018$system))

# School numbers changed, too.

list(schools_priority_2012, schools_priority_2015, schools_priority_2018) %>%
  map_at(3, ~ rename(.x, district_number = system, school_number = school)) %>%
  map(
    ~ .x %>%
      filter(district_number %in% c(791, 792)) %>%
      extract2("school_number") %>%
      summary()
  )

schools_priority_2018 %>% filter(system == 792, school < 2000)

# Confirm that each new school number equals 2000 plus the old number. (This
# update will be necessary for matching 2017-2019 accountability data with
# 2012 schools.)

crosswalk_shelby <-
  list(schools_priority_2012, schools_priority_2015, schools_priority_2018) %>%
  map_at(
    1:2,
    ~ .x %>%
      select(
        system = district_number,
        school = school_number,
        school_name = school
      )
  ) %>%
  map_at(1, ~ mutate_at(.x, vars(school), funs(. + 2000))) %>%
  imap(
    ~ .x %>%
      filter(system %in% c(791, 792)) %>%
      transmute(system = 792, school, school_name, element = .y)
  ) %>%
  reduce(full_join, by = c("system", "school")) %>%
  rename(
    school_name_2012 = school_name.x,
    school_name_2015 = school_name.y,
    school_name_2018 = school_name
  ) %>%
  select(-starts_with("element")) %>%
  arrange(school_name_2012, school_name_2015, school_name_2018)

# Longitudinal Priority list comparison ----

# This exploratory analysis will help update the Venn diagram in the report if
# needed. It will also help determine if grouping schools by number of times
# identified would be helpful.

# schools_priority <-
#   schools_priority_2018 %>%
#   select(system, system_name, school, school_name) %>%
#   full_join(
#     schools_priority_2015 %>%
#       select(
#         system = district_number,
#         system_name = district,
#         school = school_number,
#         school_name = school
#       ),
#     by = c("system", "school"),
#     suffix = c("", "_2015")
#   ) %>%
#   full_join(
#     schools_priority_2012 %>%
#       select(
#         system = district_number,
#         system_name = district,
#         school = school_number,
#         school_name = school
#       ),
#     by = c("system", "school"),
#     suffix = c("", "_2012")
#   )

schools_priority <-
  schools_priority_2018 %>%
  transmute(
    system,
    school,
    year = "identified_2018"
  ) %>%
  bind_rows(
    schools_priority_2015 %>%
      transmute(
        system = district_number,
        school = school_number,
        year = "identified_2015"
      )
  ) %>%
  bind_rows(
    schools_priority_2012 %>%
      transmute(
        system = if_else(district_number == 791, 792, district_number),
        school = if_else(
          district_number == 791 & school_number < 8000,
          school_number + 2000,
          school_number
        ),
        year = "identified_2012"
      )
  ) %>%
  mutate(value = T) %>%
  spread(year, value) %>%
  mutate(
    when_identified = case_when(
      identified_2012 & identified_2015 & identified_2018 ~ "2012, 2015, and 2018",
      identified_2012 & identified_2015 ~ "2012 and 2015",
      identified_2012 & identified_2018 ~ "2012 and 2018",
      identified_2015 & identified_2018 ~ "2015 and 2018",
      identified_2012 ~ "2012 only",
      identified_2015 ~ "2015 only",
      identified_2018 ~ "2018 only"
    )
  )

# These school counts differ from those in the report for a few reasons:

# 1) 9 schools classified as "2018 only" should be classified as "2012, 2015,
#    and 2018" because these schools changed district and school numbers upon
#    conversion to the ASD. 4 schools classified as "2015 and 2018" should be
#    classified as "2012, 2015, and 2018" for the same reason.
# 2) The same reason applies for 4 schools that should be re-classified from
#    "2018 only" to "2015 and 2018."

# 2015-2018 to 2012-2015-2018 (4): changes to ID numbers
# 2018 to 2012-2015-2018 (9): changes to ID numbers
# 2018 to 2015-2018 (4): changes to ID numbers

schools_priority %>%
  group_by(when_identified) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

check <-
  schools_priority %>%
  full_join(
    schools_priority_historical %>%
      select(system, school, id_2012, id_2015, exited) %>%
      mutate_at(vars(id_2012, id_2015), funs(if_else(is.na(.), 0, .))) %>%
      mutate(
        when_identified_2 = case_when(
          pmin(id_2012, id_2015) == 1 ~ "2012, 2015, and 2018",
          id_2012 == 1 ~ "2012 and 2018",
          id_2015 == 1 ~ "2015 and 2018",
          T ~ "2018 only"
        )
      ),
    by = c("system", "school")
  )

count(check, when_identified, when_identified_2)

View(
  check %>%
    filter(
      when_identified != when_identified_2
      # Add more conditions here to deal with NA.
      # Does schools_priority_historical actually list 2018 Priority schools?
    )
)
