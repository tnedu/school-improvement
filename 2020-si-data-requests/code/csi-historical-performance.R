# Historical Performance of Priority or CSI Schools
# Josh Carson
# Requested by Andrea Thorsbakken
# Last updated 2020-01-24

# Provide historical outcome data on Priority or CSI schools for a Strategic
# Plan report.

library(magrittr)
library(openxlsx)
library(tidyverse)

connection_eis <-
  DBI::dbConnect(
    RJDBC::JDBC(
      "oracle.jdbc.OracleDriver",
      classPath = Sys.getenv("jar_path")
    ),
    Sys.getenv("eis_connection_string"),
    "EIS_MGR", Sys.getenv("eis_password")
  )

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
    sy2017 = "2017_final_accountability_files/school_base_2017_for_accountability.csv",
    tvaas_2017 = "2017_tvaas/2017 School Composites.xlsx"
  ) %>%
  map(~ str_c("N:/ORP_accountability/data/", .x)) %>%
  map_at(1:3, read_csv) %>%
  map_at(4, read.xlsx) %>%
  map(
    ~ .x %>%
      janitor::clean_names() %>%
      strip_attributes()
  )

schools <-
  DBI::dbGetQuery(
    connection_eis,
    "SELECT s.*, d.district_name
    FROM school s JOIN district d ON s.district_no = d.district_no"
  ) %>%
  rename_all(tolower)

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

n_distinct(schools$district_no, schools$school_no)

# Over 80% of 2012 Priority schools were in Memphis (district 791).

count(schools_priority_2012, district_number, sort = T)

# Does schools_priority_historical (created July 2018) include 2018 Priority
# schools? No, only half of 2018 Priority schools are included in the
# historical list. For the 41 schools not included, we do not have former
# district or school numbers (where applicable).

schools_priority_historical %>%
  transmute(
    system,
    school,
    in_historical = T
  ) %>%
  full_join(
    schools_priority_2018 %>%
      transmute(
        system,
        school,
        in_2018 = T
      )
  ) %>%
  count(in_historical, in_2018)

# The 2017 numeric file has a very different layout from the 2018 and 2019
# accountability files.

count(accountability$sy2017, year)
count(accountability$sy2017, subject)
count(accountability$sy2017, subgroup)
count(accountability$sy2017, grade)
View(count(accountability$sy2017, subject, grade))

summary(accountability$sy2017$valid_tests)
mean(accountability$sy2017$valid_tests < 30, na.rm = T)

accountability$sy2017 %>% summarize(n_schools = n_distinct(system, school))

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
      # left_join(
      #   crosswalk_id,
      #   by = c("system" = "system_old", "school" = "school_old"),
      #   suffix = c("", "_y")
      # )
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

# Success Rate ----

eoc_english <- c("English I", "English II", "English III")

eoc_math <- c(
  "Algebra I",
  "Algebra II",
  "Geometry",
  "Integrated Math I",
  "Integrated Math II",
  "Integrated Math III"
)

eoc_science <- c("Biology I", "Chemistry")

success_rate <-
  accountability[c("sy2019", "sy2018", "sy2017")] %>%
  map_at(
    c("sy2019", "sy2018"),
    ~ .x %>%
      filter(indicator == "Achievement", subgroup == "All Students") %>%
      transmute(
        system,
        school,
        test_year = 2019,
        n_otm = round(n_count * metric / 100),
        n_valid_tests = n_count
      )
  ) %>%
  map_at("sy2018", ~ mutate(.x, test_year = 2018)) %>%
  map_at(
    "sy2017",
    ~ .x %>%
      filter(
        # Ignore 2016 data for now due to testing misadministrations that year.
        year == 2017,
        subject %in% c(
          "ELA", "Math", "Science",
          eoc_english, eoc_math, eoc_science
        ),
        subgroup == "All Students",
        !grade %in% c("All Grades", "Missing Grade")
      ) %>%
      mutate(
        grade_below_9 = !is.na(as.numeric(grade)) & as.numeric(grade) < 9,
        n_otm = n_on_track + n_mastered
      ) %>%
      mutate_at(
        vars(subject),
        funs(
          case_when(
            . %in% eoc_english & grade_below_9 ~ "ELA",
            . %in% eoc_math & grade_below_9 ~ "Math",
            . %in% eoc_science & grade_below_9 ~ "Science",
            T ~ .
          )
        )
      ) %>%
      group_by(system, school, year, subject) %>%
      summarize_at(vars(n_otm, valid_tests), sum, na.rm = T) %>%
      filter(valid_tests >= 30) %>%
      summarize_at(vars(n_otm, valid_tests), sum, na.rm = T) %>%
      ungroup() %>%
      rename(test_year = year, n_valid_tests = valid_tests)
  ) %>%
  reduce(bind_rows) %>%
  mutate(success_rate = round(100 * n_otm / n_valid_tests, 1)) %>%
  arrange(system, school, test_year) %>%
  left_join(
    schools_priority_historical %>%
      select(
        system,
        school,
        system_alt = system_old,
        school_alt = school_old
      ),
    by = c("system", "school")
  ) %>%
  left_join(
    schools_priority_historical %>%
      select(
        system_alt_2 = system,
        school_alt_2 = school,
        system_alt = system_old,
        school_alt = school_old
      ),
    by = c("system" = "system_alt", "school" = "school_alt")
  ) %>%
  left_join(
    schools_priority_2018 %>%
      transmute(
        system,
        school,
        identified_2018 = T
      ),
    by = c("system", "school")
  ) %>%
  left_join(
    schools_priority_2018 %>%
      transmute(
        system_alt = system,
        school_alt = school,
        identified_2018_alt = T
      ),
    by = c("system_alt", "school_alt")
  ) %>%
  left_join(
    schools_priority_2018 %>%
      transmute(
        system_alt_2 = system,
        school_alt_2 = school,
        identified_2018_alt_2 = T
      ),
    by = c("system_alt_2", "school_alt_2")
  ) %>%
  left_join(
    schools_priority_2015 %>%
      transmute(
        system = district_number,
        school = school_number,
        identified_2015 = T
      ),
    by = c("system", "school")
  ) %>%
  left_join(
    schools_priority_2015 %>%
      transmute(
        system_alt = district_number,
        school_alt = school_number,
        identified_2015_alt = T
      ),
    by = c("system_alt", "school_alt")
  ) %>%
  left_join(
    schools_priority_2015 %>%
      transmute(
        system_alt_2 = district_number,
        school_alt_2 = school_number,
        identified_2015_alt_2 = T
      ),
    by = c("system_alt_2", "school_alt_2")
  ) %>%
  left_join(
    schools_priority_2012 %>%
      transmute(
        system = if_else(
          district_number == 791,
          792,
          district_number
        ),
        school = if_else(
          district_number == 791 & school_number < 2000,
          school_number + 2000,
          school_number
        ),
        identified_2012 = T
      ),
    by = c("system", "school")
  ) %>%
  left_join(
    schools_priority_2012 %>%
      transmute(
        system_alt = if_else(
          district_number == 791,
          792,
          district_number
        ),
        school_alt = if_else(
          district_number == 791 & school_number < 2000,
          school_number + 2000,
          school_number
        ),
        identified_2012_alt = T
      ),
    by = c("system_alt", "school_alt")
  ) %>%
  left_join(
    schools_priority_2012 %>%
      transmute(
        system_alt_2 = if_else(
          district_number == 791,
          792,
          district_number
        ),
        school_alt_2 = if_else(
          district_number == 791 & school_number < 2000,
          school_number + 2000,
          school_number
        ),
        identified_2012_alt_2 = T
      ),
    by = c("system_alt_2", "school_alt_2")
  ) %>%
  mutate(
    identified_2012 = identified_2012 | identified_2012_alt | identified_2012_alt_2,
    identified_2015 = identified_2015 | identified_2015_alt | identified_2015_alt_2,
    identified_2018 = identified_2018 | identified_2018_alt | identified_2018_alt_2
  ) %>%
  left_join(
    schools %>%
      select(
        system = district_no,
        system_name = district_name,
        school = school_no,
        school_name
      ),
    by = c("system", "school")
  ) %>%
  select(
    system, system_name, school, school_name, test_year,
    identified_2012, identified_2015, identified_2018,
    n_otm, n_valid_tests, success_rate
  ) %>%
  arrange(system_name, school_name, test_year)

success_rate_summary <-
  success_rate %>%
  gather(
    identification, identified,
    identified_2012, identified_2015, identified_2018
  ) %>%
  group_by(system, school) %>%
  mutate(
    never_identified = mean(is.na(identified)) == 1,
    identification = if_else(
      never_identified,
      "Never Priority",
      str_c(str_replace(identification, "identified_", ""), " Priority")
    )
  ) %>%
  ungroup() %>%
  distinct(system, school, test_year, identification, .keep_all = T) %>%
  arrange(system, school, test_year, identification) %>%
  filter(identification == "Never Priority" | !is.na(identified)) %>%
  # Any school identified multiple times is counted once for each
  # year in which the school was identified.
  group_by(test_year, identification) %>%
  summarize(
    n_schools = n_distinct(system, school),
    n_otm = sum(n_otm, na.rm = T),
    n_valid_tests = sum(n_valid_tests)
  ) %>%
  ungroup() %>%
  mutate(success_rate = round(100 * n_otm / n_valid_tests, 1))

# TVAAS ----

tvaas <-
  accountability[c("sy2019", "sy2018", "tvaas_2017")] %>%
  map_at(
    c("sy2019", "sy2018"),
    ~ .x %>%
      filter(indicator == "Growth", subgroup == "All Students") %>%
      transmute(
        system,
        school,
        test_year = 2019,
        tvaas = metric
      )
  ) %>%
  map_at("sy2018", ~ mutate(.x, test_year = 2018)) %>%
  map_at(
    "tvaas_2017",
    ~ .x %>%
      transmute(
        system = as.numeric(district_number),
        school = as.numeric(school_number),
        test_year = 2017,
        tvaas = school_wide_composite
      )
  ) %>%
  reduce(bind_rows) %>%
  arrange(system, school, test_year) %>%
  left_join(
    schools_priority_historical %>%
      select(
        system,
        school,
        system_alt = system_old,
        school_alt = school_old
      ),
    by = c("system", "school")
  ) %>%
  left_join(
    schools_priority_historical %>%
      select(
        system_alt_2 = system,
        school_alt_2 = school,
        system_alt = system_old,
        school_alt = school_old
      ),
    by = c("system" = "system_alt", "school" = "school_alt")
  ) %>%
  left_join(
    schools_priority_2018 %>%
      transmute(
        system,
        school,
        identified_2018 = T
      ),
    by = c("system", "school")
  ) %>%
  left_join(
    schools_priority_2018 %>%
      transmute(
        system_alt = system,
        school_alt = school,
        identified_2018_alt = T
      ),
    by = c("system_alt", "school_alt")
  ) %>%
  left_join(
    schools_priority_2018 %>%
      transmute(
        system_alt_2 = system,
        school_alt_2 = school,
        identified_2018_alt_2 = T
      ),
    by = c("system_alt_2", "school_alt_2")
  ) %>%
  left_join(
    schools_priority_2015 %>%
      transmute(
        system = district_number,
        school = school_number,
        identified_2015 = T
      ),
    by = c("system", "school")
  ) %>%
  left_join(
    schools_priority_2015 %>%
      transmute(
        system_alt = district_number,
        school_alt = school_number,
        identified_2015_alt = T
      ),
    by = c("system_alt", "school_alt")
  ) %>%
  left_join(
    schools_priority_2015 %>%
      transmute(
        system_alt_2 = district_number,
        school_alt_2 = school_number,
        identified_2015_alt_2 = T
      ),
    by = c("system_alt_2", "school_alt_2")
  ) %>%
  left_join(
    schools_priority_2012 %>%
      transmute(
        system = if_else(
          district_number == 791,
          792,
          district_number
        ),
        school = if_else(
          district_number == 791 & school_number < 2000,
          school_number + 2000,
          school_number
        ),
        identified_2012 = T
      ),
    by = c("system", "school")
  ) %>%
  left_join(
    schools_priority_2012 %>%
      transmute(
        system_alt = if_else(
          district_number == 791,
          792,
          district_number
        ),
        school_alt = if_else(
          district_number == 791 & school_number < 2000,
          school_number + 2000,
          school_number
        ),
        identified_2012_alt = T
      ),
    by = c("system_alt", "school_alt")
  ) %>%
  left_join(
    schools_priority_2012 %>%
      transmute(
        system_alt_2 = if_else(
          district_number == 791,
          792,
          district_number
        ),
        school_alt_2 = if_else(
          district_number == 791 & school_number < 2000,
          school_number + 2000,
          school_number
        ),
        identified_2012_alt_2 = T
      ),
    by = c("system_alt_2", "school_alt_2")
  ) %>%
  mutate(
    identified_2012 = identified_2012 | identified_2012_alt | identified_2012_alt_2,
    identified_2015 = identified_2015 | identified_2015_alt | identified_2015_alt_2,
    identified_2018 = identified_2018 | identified_2018_alt | identified_2018_alt_2
  ) %>%
  left_join(
    schools %>%
      select(
        system = district_no,
        system_name = district_name,
        school = school_no,
        school_name
      ),
    by = c("system", "school")
  ) %>%
  select(
    system, system_name, school, school_name, test_year,
    identified_2012, identified_2015, identified_2018,
    tvaas
  ) %>%
  arrange(system_name, school_name, test_year)

tvaas_summary <-
  tvaas %>%
  gather(
    identification, identified,
    identified_2012, identified_2015, identified_2018
  ) %>%
  group_by(system, school) %>%
  mutate(
    never_identified = mean(is.na(identified)) == 1,
    identification = if_else(
      never_identified,
      "Never Priority",
      str_c(str_replace(identification, "identified_", ""), " Priority")
    )
  ) %>%
  ungroup() %>%
  distinct(system, school, test_year, identification, .keep_all = T) %>%
  arrange(system, school, test_year, identification) %>%
  filter(identification == "Never Priority" | !is.na(identified)) %>%
  # Any school identified multiple times is counted once for each
  # year in which the school was identified.
  group_by(test_year, identification) %>%
  summarize(
    n_schools_4_or_5 = sum(!is.na(tvaas) & tvaas > 3),
    n_schools = n()
  ) %>%
  ungroup() %>%
  mutate(pct_schools_4_or_5 = round(100 * n_schools_4_or_5 / n_schools, 1))

# Output ----

write.xlsx(
  list(
    success_rate = success_rate,
    sr_summary = success_rate_summary,
    tvaas = tvaas,
    tvaas_summary = tvaas_summary
  ),
  "data/priority-historical-performance.xlsx",
  colWidths = "auto",
  overwrite = T
)
