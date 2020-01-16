library(DBI)
library(RJDBC)
library(lubridate)
library(magrittr)
library(openxlsx)
library(sida)
library(tidyverse)

connection_eis <- connect()

# Import CSI schools ----

schools_csi <-
  read.xlsx(
    "C:/Users/CA20397/SharePoint/School Improvement - Documents/School Lists/school-designations-2018.xlsx",
    sheet = "Comprehensive Support"
  ) %>%
  filter(
    active == "Yes",
    !(system == 600 & school == 110), # Northfield Academy (adult high school)
    !(system == 792 & school == 8275) # The Excel Center (adult high school)
  ) %>%
  transmute(
    district = system,
    district_name = system_name,
    school,
    school_name
  ) %>%
  bind_rows(
    tibble(
      district = 792,
      district_name = "Shelby County",
      school = 2245,
      school_name = "Geeter School"
    )
  ) %>%
  left_join(
    dbGetQuery(
      connection_eis,
      "select school_bu_id as bu_id, district_no, school_no
      from school
      where operational_status = 'A'"
    ),
    by = c("district" = "DISTRICT_NO", "school" = "SCHOOL_NO")
  ) %>%
  rename_all(tolower) %>%
  arrange(district_name, school_name)

school_ids <- str_c("(", glue::glue_collapse(schools_csi$bu_id, sep = ", "), ")")

# Query school calendars ----

calendar <-
  dbGetQuery(
    connection_eis,
    str_replace(read_file("code/calendar.sql"), "SCHOOL IDS HERE", school_ids)
  ) %>%
  rename_all(tolower) %>%
  mutate_at(vars(id_date), as_date) %>%
  group_by(district, school, school_year, ipn) %>%
  mutate(id_number = min_rank(id_date)) %>%
  ungroup()

# Summarize to the IPN and month levels before filling missing dates below.

# calendar_ipn <-
#   calendar %>%
#   group_by(district, school, school_year, ipn) %>%
#   mutate(n_id_total = n_distinct(id_date)) %>%
#   filter(id_date <= now()) %>%
#   group_by(n_id_total, add = T) %>%
#   summarize(n_id_to_date = n_distinct(id_date)) %>%
#   ungroup()

calendar_month <-
  calendar %>%
  mutate(calendar_month = month(id_date)) %>%
  group_by(district, school, school_year, ipn, calendar_month) %>%
  summarize(
    # n_id_in_month = n_distinct(id_number),
    last_id_number = max(id_number)
  ) %>%
  mutate(id_month = min_rank(last_id_number)) %>%
  ungroup() %>%
  select(district, school, school_year, ipn, calendar_month, id_month, everything()) %>%
  arrange(district, school, school_year, ipn, id_month)

# Fill missing dates and instructional day numbers.

calendar_all_dates <-
  tibble(
    school_year = 2019,
    id_date = seq(
      from = calendar %>% filter(school_year == 2019) %>% extract2("id_date") %>% min(),
      to = calendar %>% filter(school_year == 2019) %>% extract2("id_date") %>% max(),
      by = 1
    )
  ) %>%
  bind_rows(
    tibble(
      school_year = 2020,
      id_date = seq(
        from = calendar %>% filter(school_year == 2020) %>% extract2("id_date") %>% min(),
        to = calendar %>% filter(school_year == 2020) %>% extract2("id_date") %>% max(),
        by = 1
      )
    )
  )

calendar <-
  calendar %>%
  group_by(district, school, school_year, ipn) %>%
  nest() %>%
  mutate(
    data = map2(
      data, school_year,
      ~ .x %>%
        full_join(
          calendar_all_dates %>% filter(school_year == .y) %>% select(id_date),
          by = c("id_date")
        ) %>%
        arrange(id_date) %>%
        fill(id_number) %>%
        mutate_at(vars(id_number), funs(if_else(is.na(.), 1L, .)))
    )
  ) %>%
  unnest()

month_ends <-
  cross_df(list("year" = 2018:2020, "calendar_month" = 1:12)) %>%
  mutate(
    school_year = if_else(calendar_month %in% 7:12, year + 1L, year),
    day = case_when(
      calendar_month == 2 ~ 28,
      calendar_month %in% c(4, 6, 9, 11) ~ 30,
      T ~ 31
    ),
    last_date_of_month = ymd(str_c(year, calendar_month, day, sep = "-"))
  ) %>%
  select(school_year, everything()) %>%
  arrange(last_date_of_month)

month_levels <-
  c("Aug", "Sep", "Oct", "Nov", "Dec",
    "Jan", "Feb", "Mar", "Apr", "May", "Jun")

month_names <-
  tibble(month_number = 1:12) %>%
  mutate(month_name = month(month_number, label = T))

# Query and import absence and discipline data ----

absence_accountability <-
  read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv") %>%
  filter(indicator == "Chronic Absenteeism", subgroup == "All Students") %>%
  select(-system_name, -school_name) %>%
  rename(district = system) %>%
  right_join(
    schools_csi %>% select(district, school),
    by = c("district", "school")
  )

# Should I include service enrollments?

absence <-
  dbGetQuery(
    connection_eis,
    str_replace(read_file("code/absence.sql"), "SCHOOL IDS HERE", school_ids)
  ) %>%
  rename_all(tolower) %>%
  mutate_at(vars(begin_date, end_date, absence_date), as_date) %>%
  left_join(
    calendar %>% rename(begin_id_number = id_number),
    by = c("district", "school", "school_year", "ipn", "begin_date" = "id_date")
  ) %>%
  left_join(
    calendar %>% rename(end_id_number = id_number),
    by = c("district", "school", "school_year", "ipn", "end_date" = "id_date")
  ) %>%
  left_join(
    calendar %>% rename(absence_id_number = id_number),
    by = c("district", "school", "school_year", "ipn", "absence_date" = "id_date")
  )

discipline <-
  dbGetQuery(
    connection_eis,
    str_replace(read_file("code/discipline.sql"), "SCHOOL IDS HERE", school_ids)
  ) %>%
  rename_all(tolower) %>%
  mutate_at(vars(begin_date, end_date, discipline_begin_date), as_date) %>%
  left_join(
    calendar %>% rename(begin_id_number = id_number),
    by = c("district", "school", "school_year", "ipn", "begin_date" = "id_date")
  ) %>%
  left_join(
    calendar %>% rename(end_id_number = id_number),
    by = c("district", "school", "school_year", "ipn", "end_date" = "id_date")
  ) %>%
  left_join(
    calendar %>% rename(discipline_id_number = id_number),
    by = c("district", "school", "school_year", "ipn", "discipline_begin_date" = "id_date")
  )

# Summarize (student by month, YTD) ----

absence_student_month <-
  map(
    c(8:12, 1:6),
    ~ absence %>%
      # Why does only one school show up in the June 2019 group?
      left_join(
        calendar_month %>% filter(calendar_month == .x),
        by = c("district", "school", "school_year", "ipn")
      ) %>%
      left_join(
        month_ends %>% select(school_year, calendar_month, last_date_of_month),
        by = c("school_year", "calendar_month")
      ) %>%
      filter(
        !is.na(begin_date),
        (is.na(absence_date) | absence_date <= last_date_of_month)
      ) %>%
      mutate(
        n_id_enrolled = if_else(
          is.na(end_id_number),
          last_id_number - begin_id_number + 1,
          pmin(last_id_number, end_id_number) - begin_id_number + 1
        )
      ) %>%
      group_by(
        district, school, school_year, ipn, calendar_month, last_date_of_month,
        state_id, isp_id
      ) %>%
      summarize(
        n_absences = if_else(
          mean(is.na(absence_id_number)) == 1,
          0L,
          n_distinct(absence_id_number)
        ),
        n_id_enrolled = max(n_id_enrolled, na.rm = T),
        last_id_number = max(last_id_number)
      ) %>%
      summarize(
        n_absences = sum(n_absences),
        n_id_enrolled = sum(n_id_enrolled),
        last_id_number = max(last_id_number)
      ) %>%
      ungroup() %>%
      filter(n_id_enrolled / last_id_number >= 0.5) %>%
      mutate(
        n_id_enrolled = pmin(n_id_enrolled, last_id_number),
        pct_absences = round(100 * n_absences / n_id_enrolled, 1),
        chr_absent = pct_absences > 10
      )
  ) %>%
  reduce(bind_rows) %>%
  select(-last_id_number) %>%
  arrange(district, school, school_year, ipn, last_date_of_month, state_id)

discipline_student_month <-
  map(
    c(8:12, 1:6),
    ~ discipline %>%
      left_join(
        month_ends %>%
          filter(calendar_month == .x) %>%
          select(school_year, calendar_month, last_date_of_month),
        by = c("school_year")
      ) %>%
      filter(
        !is.na(begin_date),
        (is.na(discipline_begin_date) | discipline_begin_date <= last_date_of_month)
      ) %>%
      group_by(
        district, school, school_year, ipn, calendar_month, last_date_of_month,
        state_id
      ) %>%
      summarize(
        n_discipline_incidents = if_else(
          mean(is.na(discipline_id_number)) == 1,
          0L,
          n_distinct(discipline_id_number)
        )
      ) %>%
      ungroup()
  ) %>%
  reduce(bind_rows) %>%
  arrange(district, school, school_year, ipn, last_date_of_month, state_id)

# Summarize (school by month, YTD) ----

absence_school_month <-
  absence_student_month %>%
  group_by(district, school, school_year, calendar_month, last_date_of_month) %>%
  summarize(
    n_students_chr_absent = n_distinct(state_id[chr_absent]),
    n_students_in_school = n_distinct(state_id)
  ) %>%
  group_by(school_year, calendar_month) %>%
  mutate(
    pct_students_chr_absent = round(
      100 * n_students_chr_absent / n_students_in_school,
      1
    ),
    school_rank = min_rank(pct_students_chr_absent)
  ) %>%
  ungroup() %>%
  arrange(district, school, school_year, last_date_of_month)

discipline_school_month <-
  discipline_student_month %>%
  group_by(district, school, school_year, calendar_month, last_date_of_month) %>%
  summarize(
    n_students_disciplined = n_distinct(state_id[n_discipline_incidents > 0]),
    n_students_in_school = n_distinct(state_id)
  ) %>%
  group_by(school_year, calendar_month) %>%
  mutate(
    pct_students_disciplined = round(
      100 * n_students_disciplined / n_students_in_school,
      1
    ),
    school_rank = min_rank(pct_students_disciplined)
  ) %>%
  ungroup() %>%
  arrange(district, school, school_year, last_date_of_month)

# Summarize (school, EOY and YTD) ----

absence_school <-
  absence_accountability %>%
  transmute(
    district, school,
    pct_students_chr_absent_2018_eoy = metric_prior,
    pct_students_chr_absent_2019_eoy = metric,
    school_rank_2019_eoy = min_rank(pct_students_chr_absent_2019_eoy),
    amo_2020_eoy = 15/16 * pct_students_chr_absent_2019_eoy
  ) %>%
  left_join(
    absence_school_month %>%
      filter(calendar_month == month_for_ytd_filter) %>%
      select(district, school, school_year, pct_students_chr_absent) %>%
      spread(school_year, pct_students_chr_absent) %>%
      rename(
        pct_students_chr_absent_2019_ytd = `2019`,
        pct_students_chr_absent_2020_ytd = `2020`
      ) %>%
      mutate(
        school_rank_2020_ytd = min_rank(pct_students_chr_absent_2020_ytd),
        amo_2020_on_track = pct_students_chr_absent_2020_ytd <= 15/16 * pct_students_chr_absent_2019_ytd
      ),
    by = c("district", "school")
  ) %>%
  arrange(school)

discipline_school <-
  discipline_school_month %>%
  filter(school_year == 2019) %>%
  group_by(district, school) %>%
  filter(last_date_of_month == max(last_date_of_month)) %>%
  ungroup() %>%
  rename(pct_students_disciplined_2019_eoy = pct_students_disciplined) %>%
  mutate(
    school_rank_2019_eoy = min_rank(pct_students_disciplined_2019_eoy),
    amo_2020_eoy = 15/16 * pct_students_disciplined_2019_eoy
  ) %>%
  select(
    district, school,
    pct_students_disciplined_2019_eoy, school_rank_2019_eoy, amo_2020_eoy
  ) %>%
  left_join(
    discipline_school_month %>%
      filter(calendar_month == month_for_ytd_filter) %>%
      select(district, school, school_year, pct_students_disciplined) %>%
      spread(school_year, pct_students_disciplined) %>%
      rename(
        pct_students_disciplined_2019_ytd = `2019`,
        pct_students_disciplined_2020_ytd = `2020`
      ) %>%
      mutate(
        school_rank_2020_ytd = min_rank(pct_students_disciplined_2020_ytd),
        amo_2020_on_track = pct_students_disciplined_2020_ytd <= 15/16 * pct_students_disciplined_2019_ytd
      ),
    by = c("district", "school")
  ) %>%
  arrange(school)

# Summarize (district by month, YTD) ----

absence_district_month <-
  absence_student_month %>%
  group_by(district, school_year, calendar_month, last_date_of_month) %>%
  summarize(
    n_students_chr_absent = n_distinct(state_id[chr_absent]),
    n_students_in_district = n_distinct(state_id)
  ) %>%
  ungroup() %>%
  mutate(
    pct_students_chr_absent = round(
      100 * n_students_chr_absent / n_students_in_district,
      1
    )
  )

# Add a Measure Name field to be the group variable in district-level line
# graphs.

absence_district_month <-
  absence_district_month %>%
  mutate(measure_name = str_c(school_year, " Actual")) %>%
  bind_rows(
    absence_district_month %>%
      filter(school_year == 2019) %>%
      mutate(
        measure_name = "2020 AMO",
        pct_students_chr_absent = 15/16 * pct_students_chr_absent
      ) %>%
      select(-n_students_chr_absent, -n_students_in_district)
  ) %>%
  select(measure_name, everything()) %>%
  arrange(measure_name, district, school_year, last_date_of_month)

discipline_district_month <-
  discipline_student_month %>%
  group_by(district, school_year, calendar_month, last_date_of_month) %>%
  summarize(
    n_students_disciplined = n_distinct(state_id[n_discipline_incidents > 0]),
    n_students_in_district = n_distinct(state_id)
  ) %>%
  ungroup() %>%
  mutate(
    pct_students_disciplined = round(
      100 * n_students_disciplined / n_students_in_district,
      1
    )
  )

# Add a Measure Name field to be the group variable in district-level line
# graphs.

discipline_district_month <-
  discipline_district_month %>%
  mutate(measure_name = str_c(school_year, " Actual")) %>%
  bind_rows(
    discipline_district_month %>%
      filter(school_year == 2019) %>%
      mutate(
        measure_name = "2020 AMO",
        pct_students_disciplined = 15/16 * pct_students_disciplined
      ) %>%
      select(-n_students_disciplined, -n_students_in_district)
  ) %>%
  select(measure_name, everything()) %>%
  arrange(measure_name, district, school_year, last_date_of_month)

# Graph ----

schools_csi %>%
  left_join(absence_school_month, by = c("district", "school")) %>%
  filter(school_year == 2020, month == 9) %>%
  ggplot(
    aes(
      x = reorder(bu_id, pct_students_chr_absent, max),
      y = pct_students_chr_absent
    )
  ) +
  geom_col()

absence_district %>%
  filter(district == 70) %>%
  mutate(month = factor(month, levels = c(8:12, 1:5), labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May"))) %>%
  ggplot(aes(x = month, y = pct_students_chr_absent)) +
  geom_line(aes(color = measure_name, group = measure_name))

schools_csi %>%
  filter(district == 985) %>%
  left_join(absence_school, by = c("district", "school")) %>%
  gather(column_name, cell_value, pct_students_chr_absent_2018_eoy:amo_2020_on_track) %>%
  ggplot(aes(x = school_name, y = column_name)) +
  geom_text(aes(label = cell_value)) +
  coord_flip()
