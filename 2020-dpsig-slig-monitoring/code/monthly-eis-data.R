library(lubridate)
library(magrittr)
library(openxlsx)
library(sida)
library(tidyverse)

# Calendar data ----

# List school numbers

if(!exists("school_ids")) source("code/schools.R")

# Query school calendars

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
    school_year = 2017,
    id_date = seq(
      # from = calendar %>% filter(school_year == 2019) %>% extract2("id_date") %>% min(),
      from = as_date("20160701"),
      # to = calendar %>% filter(school_year == 2019) %>% extract2("id_date") %>% max(),
      to = as_date("20170630"),
      by = 1
    )
  ) %>%
  bind_rows(
    tibble(
      school_year = 2018,
      id_date = seq(
        # from = calendar %>% filter(school_year == 2020) %>% extract2("id_date") %>% min(),
        from = as_date("20170701"),
        # to = calendar %>% filter(school_year == 2020) %>% extract2("id_date") %>% max(),
        to = as_date("20180630"),
        by = 1
      )
    )
  ) %>%
  bind_rows(
    tibble(
      school_year = 2019,
      id_date = seq(
        # from = calendar %>% filter(school_year == 2019) %>% extract2("id_date") %>% min(),
        from = as_date("20180701"),
        # to = calendar %>% filter(school_year == 2019) %>% extract2("id_date") %>% max(),
        to = as_date("20190630"),
        by = 1
      )
    )
  ) %>%
  bind_rows(
    tibble(
      school_year = 2020,
      id_date = seq(
        # from = calendar %>% filter(school_year == 2020) %>% extract2("id_date") %>% min(),
        from = as_date("20190701"),
        # to = calendar %>% filter(school_year == 2020) %>% extract2("id_date") %>% max(),
        to = as_date("20200630"),
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
  cross_df(list(year = 2016:2020, calendar_month = 1:12)) %>%
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

month_levels <- c(
  "Aug", "Sep", "Oct", "Nov", "Dec",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun"
)

month_names <-
  tibble(month_number = 1:12) %>%
  mutate(month_name = month(month_number, label = T))

# Input ----

# Import chronic absenteeism numbers from accountability files for previous EOY
# measures.

absence_accountability <-
  list(
    sy2019 = "N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv",
    sy2018 = "N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv"
  ) %>%
  imap(
    ~ .x %>%
      read_csv() %>%
      filter(indicator == "Chronic Absenteeism", subgroup == "All Students") %>%
      select(-system_name, -school_name) %>%
      rename(district = system) %>%
      right_join(
        schools_csi %>% select(district, school),
        by = c("district", "school")
      ) %>%
      mutate(year = as.numeric(str_replace(.y, "sy", ""))) %>%
      select(district:designation_ineligible, year, everything())
  )

# Query absences from EIS.

absence <-
  # Should I include service enrollments?
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

# Query disciplinary incidents from EIS.

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

# Query enrollments to calculate mobility rates.

mobility <-
  dbGetQuery(
    connection_eis,
    str_replace(read_file("code/enrollment.sql"), "SCHOOL IDS HERE", school_ids)
  ) %>%
  rename_all(tolower) %>%
  mutate_at(vars(begin_date, end_date), as_date) %>%
  left_join(
    calendar %>% rename(begin_id_number = id_number),
    by = c("district", "school", "school_year", "ipn", "begin_date" = "id_date")
  ) %>%
  left_join(
    calendar %>% rename(end_id_number = id_number),
    by = c("district", "school", "school_year", "ipn", "end_date" = "id_date")
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

mobility_student_month <-
  map(
    c(8:12, 1:6),
    ~ mobility %>%
      left_join(
        calendar_month %>% filter(calendar_month == .x),
        by = c("district", "school", "school_year", "ipn")
      ) %>%
      left_join(
        month_ends %>% select(school_year, calendar_month, last_date_of_month),
        by = c("school_year", "calendar_month")
      ) %>%
      filter(!is.na(begin_date), begin_date <= last_date_of_month) %>%
      mutate(
        end_id_number = case_when(
          is.na(end_date) ~ as.integer(last_id_number),
          !is.na(end_id_number) ~ as.integer(pmin(end_id_number, last_id_number)),
          T ~ as.integer(end_id_number)
        ),
        n_id_enrolled = end_id_number - begin_id_number + 1
      ) %>%
      filter(n_id_enrolled >= 3) %>%
      # mutate_at(
      #   "end_id_number",
      #   funs(case_when(is.na(end_date) ~ as.integer(last_id_number),
      #                  !is.na(.) ~ as.integer(pmin(., last_id_number)),
      #                  T ~ as.integer(.)))
      # ) %>%
      group_by(
        district, school, school_year, ipn, calendar_month, last_date_of_month,
        state_id # , isp_id
      ) %>%
      summarize(
        # n_id_enrolled = max(n_id_enrolled, na.rm = T),
        begin_id_number = min(begin_id_number),
        end_id_number = max(end_id_number),
        last_id_number = max(last_id_number)
      ) %>%
      # summarize(
      #   n_id_enrolled = sum(n_id_enrolled),
      #   last_id_number = max(last_id_number)
      # ) %>%
      ungroup() %>%
      mutate(
        entered = begin_id_number > 1,
        exited = end_id_number < last_id_number
      )
      # mutate(n_id_enrolled = pmin(n_id_enrolled, last_id_number))
  ) %>%
  reduce(bind_rows) %>%
  # select(-last_id_number) %>%
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

mobility_school_month <-
  mobility_student_month %>%
  group_by(district, school, school_year, calendar_month, last_date_of_month) %>%
  summarize(
    n_students_entered = n_distinct(state_id[entered]),
    n_students_exited = n_distinct(state_id[exited]),
    n_students_ever_enrolled = n_distinct(state_id)
  ) %>%
  group_by(school_year, calendar_month) %>%
  mutate(
    pct_students_entered = round(
      100 * n_students_entered / n_students_ever_enrolled,
      1
    ),
    pct_students_exited = round(
      100 * n_students_exited / n_students_ever_enrolled,
      1
    ),
    school_rank_entered = min_rank(pct_students_entered),
    school_rank_exited = min_rank(pct_students_exited)
  ) %>%
  ungroup() %>%
  arrange(district, school, school_year, last_date_of_month)

# Summarize (school, EOY and YTD) ----

absence_school <-
  absence_accountability %>%
  map(~ select(.x, district, school, metric_prior, metric)) %>%
  reduce(
    full_join,
    by = c("district", "school"),
    suffix = c("_2019", "_2018")
  ) %>%
  transmute(
    district, school,
    pct_students_chr_absent_2017_eoy = metric_prior_2018,
    pct_students_chr_absent_2018_eoy = metric_prior_2019,
    pct_students_chr_absent_2019_eoy = metric_2019,
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
  # Keep end-of-year numbers from previous school years.
  filter(school_year != max(school_year)) %>%
  group_by(district, school, school_year) %>%
  filter(last_date_of_month == max(last_date_of_month)) %>%
  ungroup() %>%
  select(district, school, school_year, pct_students_disciplined) %>%
  spread(school_year, pct_students_disciplined) %>%
  rename_at(
    vars(-district, -school),
    funs(str_c("pct_students_disciplined_", ., "_eoy"))
  ) %>%
  # rename(pct_students_disciplined_2019_eoy = pct_students_disciplined) %>%
  mutate(
    school_rank_2019_eoy = min_rank(pct_students_disciplined_2019_eoy),
    amo_2020_eoy = 15/16 * pct_students_disciplined_2019_eoy
  ) %>%
  select(
    district, school,
    starts_with("pct_students_disciplined"), school_rank_2019_eoy, amo_2020_eoy
  ) %>%
  left_join(
    discipline_school_month %>%
      filter(
        # Include YTD measures only for the current and last school years.
        school_year %in% c(max(school_year), max(school_year) - 1),
        calendar_month == month_for_ytd_filter
      ) %>%
      select(district, school, school_year, pct_students_disciplined) %>%
      spread(school_year, pct_students_disciplined) %>%
      rename_at(
        vars(-district, -school),
        funs(str_c("pct_students_disciplined_", ., "_ytd"))
      ) %>%
      # rename(
      #   pct_students_disciplined_2019_ytd = `2019`,
      #   pct_students_disciplined_2020_ytd = `2020`
      # ) %>%
      mutate(
        school_rank_2020_ytd = min_rank(pct_students_disciplined_2020_ytd),
        amo_2020_on_track = pct_students_disciplined_2020_ytd <= 15/16 * pct_students_disciplined_2019_ytd
      ),
    by = c("district", "school")
  ) %>%
  arrange(school)

mobility_school <-
  map2(
    .x = quos(pct_students_entered, pct_students_exited),
    .y = c("entered_", "exited_"),
    ~ mobility_school_month %>%
      # Keep end-of-year numbers from previous school years.
      filter(school_year != max(school_year)) %>%
      group_by(district, school, school_year) %>%
      filter(last_date_of_month == max(last_date_of_month)) %>%
      ungroup() %>%
      select(district, school, school_year, !!.x) %>%
      spread(school_year, !!.x) %>%
      rename_at(
        vars(-district, -school),
        funs(str_c("pct_students_", .y, ., "_eoy"))
      )
  ) %>%
  reduce(full_join, by = c("district", "school")) %>%
  # rename(
  #   pct_students_entered_2019_eoy = pct_students_entered,
  #   pct_students_exited_2019_eoy = pct_students_exited
  # ) %>%
  mutate(
    school_rank_entered_2019_eoy = min_rank(pct_students_entered_2019_eoy),
    school_rank_exited_2019_eoy = min_rank(pct_students_exited_2019_eoy)
    # amo_2020_eoy = 15/16 * pct_students_entered_2019_eoy
  ) %>%
  # select(
  #   district, school,
  #   pct_students_entered_2019_eoy, school_rank_entered_2019_eoy, # amo_2020_eoy
  #   pct_students_exited_2019_eoy, school_rank_exited_2019_eoy
  # ) %>%
  left_join(
    mobility_school_month %>%
      filter(
        # Include YTD measures only for the current and last school years.
        school_year %in% c(max(school_year), max(school_year) - 1),
        calendar_month == month_for_ytd_filter
      ) %>%
      select(district, school, school_year, pct_students_entered) %>%
      spread(school_year, pct_students_entered) %>%
      rename(
        pct_students_entered_2019_ytd = `2019`,
        pct_students_entered_2020_ytd = `2020`
      ) %>%
      mutate(
        school_rank_entered_2020_ytd = min_rank(pct_students_entered_2020_ytd)
        # amo_2020_on_track = pct_students_disciplined_2020_ytd <= 15/16 * pct_students_disciplined_2019_ytd
      ),
    by = c("district", "school")
  ) %>%
  left_join(
    mobility_school_month %>%
      filter(
        # Include YTD measures only for the current and last school years.
        school_year %in% c(max(school_year), max(school_year) - 1),
        calendar_month == month_for_ytd_filter
      ) %>%
      select(district, school, school_year, pct_students_exited) %>%
      spread(school_year, pct_students_exited) %>%
      rename(
        pct_students_exited_2019_ytd = `2019`,
        pct_students_exited_2020_ytd = `2020`
      ) %>%
      mutate(
        school_rank_exited_2020_ytd = min_rank(pct_students_exited_2020_ytd)
        # amo_2020_on_track = pct_students_disciplined_2020_ytd <= 15/16 * pct_students_disciplined_2019_ytd
      ),
    by = c("district", "school")
  ) %>%
  arrange(district, school)

# Summarize (district by month, YTD) ----

absence_district_month <-
  absence_student_month %>%
  filter(last_date_of_month <= today() | calendar_month == month_for_ytd_filter) %>%
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
  mutate(measure_name = str_c(school_year, " Year-to-Date")) %>%
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
  filter(last_date_of_month <= today() | calendar_month == month_for_ytd_filter) %>%
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
  mutate(measure_name = str_c(school_year, " Year-to-Date")) %>%
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

mobility_district_month <-
  mobility_student_month %>%
  filter(last_date_of_month <= today() | calendar_month == month_for_ytd_filter) %>%
  group_by(district, school_year, calendar_month, last_date_of_month) %>%
  summarize(
    n_students_entered = n_distinct(state_id[entered]),
    n_students_exited = n_distinct(state_id[exited]),
    n_students_ever_enrolled = n_distinct(state_id)
  ) %>%
  ungroup() %>%
  mutate(
    pct_students_entered = round(
      100 * n_students_entered / n_students_ever_enrolled,
      1
    ),
    pct_students_exited = round(
      100 * n_students_exited / n_students_ever_enrolled,
      1
    ),
    measure_name = str_c(school_year, " Year-to-Date")
  ) %>%
  select(measure_name, everything()) %>%
  arrange(measure_name, district, school_year, last_date_of_month)

# Don't need AMOs

# mobility_district_month <-
#   mobility_district_month %>%
#   mutate(measure_name = str_c(school_year, " Year-to-Date")) %>%
#   bind_rows(
#     absence_district_month %>%
#       filter(school_year == 2019) %>%
#       mutate(
#         measure_name = "2020 AMO",
#         pct_students_chr_absent = 15/16 * pct_students_chr_absent
#       ) %>%
#       select(-n_students_chr_absent, -n_students_in_district)
#   ) %>%
#   select(measure_name, everything()) %>%
#   arrange(measure_name, district, school_year, last_date_of_month)

# Write CSVs ----

walk2(
  .x = list(
    absence_school,
    absence_district_month,
    discipline_school,
    discipline_district_month,
    mobility_school,
    mobility_district_month
  ),
  .y = list(
    "absence-school-",
    "absence-district-month-",
    "discipline-school-",
    "discipline-district-month-",
    "mobility-school-",
    "mobility-district-month-"
  ),
  ~ write_csv(.x, path = str_c("data/", .y, today(), ".csv"))
)
