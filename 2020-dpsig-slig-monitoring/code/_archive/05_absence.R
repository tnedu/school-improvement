# abs ----

# Query absences from EIS, filter absences that actually occur during
# enrollments (based on start and end dates), add variables from calendar,
# classification, disability, and enrollment tables (instructional day numbers
# and periods), then fix missing values in those variables.

abs <- dbGetQuery(con_eis, sql_2_txt("absence_v2", years = yrs))

tmp <-
  abs %>%
  rename_all(tolower) %>%
  mutate_at(vars(ends_with("_date")), as_date) %>%
  inner_join(schools_csi_2019 %>% select(system, school)) %>%
  inner_join(
    enr %>% select(isp_id, begin_date, end_date),
    by = "isp_id"
  ) %>%
  filter(!is.na(abs_date)) %>%
  filter(abs_date < end_date | is.na(end_date)) %>%
  filter(abs_date >= begin_date) %>%
  inner_join(
    cal %>%
      inner_join(schools_csi_2019 %>% select(system, school)) %>%
      select(year, system, school, id_date, id_num, period),
    by = c("year", "system", "school", "abs_date" = "id_date")
  ) %>%
  left_join(cal_now, by = c("system", "school")) %>%
  group_by(year, system, school) %>%
  # mutate(
  #   id_num =
  #     case_when(
  #       is.na(id_num) &
  #         (is.na(abs_date) | abs_date < min(abs_date[!is.na(id_num)])) ~ 1,
  #       is.na(id_num) ~ max(id_num, na.rm = T),
  #       T ~ id_num
  #     ),
  #   period = ceiling(id_num / 20)
  # ) %>%
  ungroup() %>%
  filter(!is.na(isp_id)) %>%
  select(-begin_date, -end_date) %>%
  arrange(year, system, school, state_id, isp_id, abs_date)

# Add classifications, and identify absences of contemporaneously ED students.

tmp <-
  tmp %>%
  left_join(
    cla %>% select(isp_id, ed, cla_desc, begin_num, end_num),
    by = "isp_id"
  ) %>%
  mutate(
    ed = case_when(is.na(ed) ~ 0,
                   id_num < begin_num | id_num > end_num ~ 0,
                   T ~ ed)
  ) %>%
  group_by_at(vars(year:now_prd)) %>%
  summarize(ed = max(ed)) %>%
  ungroup() %>%
  arrange(year, system, school, state_id, isp_id, abs_date)

# Add disabilities, and identify absences of students with contemporaneous
# disabilities.

tmp <-
  tmp %>%
  left_join(
    dsb %>% select(isp_id, dis_level, begin_num, end_num),
    by = "isp_id"
  ) %>%
  mutate(
    swd = case_when(is.na(dis_level) ~ 0,
                    id_num < begin_num | id_num > end_num ~ 0,
                    T ~ 1)
  ) %>%
  group_by_at(vars(year:ed)) %>%
  summarize(swd = max(swd)) %>%
  ungroup() %>%
  arrange(year, system, school, state_id, isp_id, abs_date)

tmp <-
  tmp %>%
  mutate(ytd = 0) %>%
  bind_rows(
    tmp %>%
      filter(id_num <= now_num) %>%
      mutate(ytd = 1)
  ) %>%
  filter(year < max(year) | ytd == 1)

# Join student demographics and enrollments of all students (including those
# with no absences). Filter students enrolled for at least 50% of the school
# year.

# This table includes school-, student-, enrollment-, and absence-level
# variables.

tmp_2 <-
  schools_csi_2019 %>%
  left_join(
    enr %>%
      select(system, school, year, ipn, isp_id, state_id),
    by = c("system", "school")
  ) %>%
  right_join(
    enr_stu %>% select(system, school, year, ipn, state_id, ytd, days_enrolled),
    by = c("system", "school", "year", "ipn", "state_id")
  ) %>%
  left_join(
    cal %>%
      group_by(year, system, school, ipn) %>%
      summarize(days_total = as.numeric(n_distinct(id_num))) %>%
      ungroup(),
    by = c("system", "school", "year", "ipn")
  ) %>%
  left_join(cal_now, by = c("system", "school")) %>%
  mutate(
    # ell = case_when(is.na(elb) ~ 0,
    #                 elb %in% c("L", "N", as.character(1:4)) ~ 1,
    #                 T ~ 0),
    days_total =
      if_else(year == max(year, na.rm = T) | ytd == 1,
              now_num, days_total)
  ) %>%
  # Redundant filter below (see demographics)
  filter(days_enrolled >= 0.5 * days_total) %>%
  # left_join(stu, by = "state_id") %>%
  # Join absences
  left_join(
    tmp %>% transmute(isp_id, ytd, abs_type, abs_date, id_num, abs_period = period, ed, swd),
    by = c("isp_id", "ytd"),
    suffix = c("", "_abs")
  )
  # select(
  #   ytd, year, system:school_name, days_total, ipn:local_id, last_name:begin_9,
  #   days_enrolled, isp_id:elb_desc, ell, everything()
  # ) %>%
  # arrange(ytd, year, system_name, school_name, state_id, isp_id, abs_date)

stopifnot(min(tmp_2$days_enr / tmp_2$days_total) >= 0.5)

# abs <- tmp_2

# abs_stu ----

# Count absences for each student.

abs_stu <-
  tmp_2 %>%
  group_by_at(vars(ytd, year, system:school_name, state_id, days_enrolled)) %>%
  summarize(n_abs = as.numeric(n_distinct(id_num))) %>%
  ungroup() %>%
  mutate(
    pct_abs = 100 * n_abs / days_enrolled,
    chra = as.numeric(pct_abs >= 10)
  ) %>%
  arrange(ytd, year, system_name, school_name, state_id)

# abs_stu_prd ----

# Count absences by 20-day period for each student. Filter periods with at
# least 15 days.

abs_stu_prd <-
  enr_stu_prd %>%
  filter(enr == 1) %>%
  left_join(cal_now, by = c("system", "school"))

abs_stu_prd <-
  abs_stu_prd %>%
  mutate(ytd = 0) %>%
  bind_rows(abs_stu_prd %>% filter(period <= now_prd) %>% mutate(ytd = 1)) %>%
  filter(year < max(year) | ytd == 1) %>%
  select(-now_num, -now_prd) %>%
  left_join(
    enr_stu %>% select(-system_name, -school_name),
    by = c("ytd", "year", "system", "school", "ipn", "state_id")
  ) %>%
  left_join(
    tmp_2 %>% select(-system_name, -school_name, -isp_id, -days_enrolled, -days_total),
    by = c("ytd", "year", "system", "school", "ipn", "state_id", "period" = "abs_period")
  ) %>%
  group_by_at(vars(ytd, year, system, school, ipn, state_id, period)) %>%
  summarize(
    n_abs = if_else(
      mean(is.na(id_num)) == 1,
      0,
      as.numeric(n_distinct(id_num))
    )
  ) %>%
  ungroup()

# abs_stu_prd <-
#   tmp_2 %>%
#   # filter(ytd == 0) %>%
#   group_by_at(vars(ytd, year, system:school_name, state_id, abs_period)) %>%
#   summarize(n_abs = as.numeric(n_distinct(id_num))) %>%
#   ungroup() %>%
#   # Best way to do this?
#   mutate_at(vars(abs_period), funs(if_else(ytd == 1 & is.na(.))))

# if(asd) {
#   abs_stu_prd <-
#     abs_stu_prd %>%
#     complete(
#       nesting(ytd, year, system, system_name, operator, school, school_name, ipn,
#               sch_grp, state_id),
#       period
#     )
# } else {
#   abs_stu_prd <-
#     abs_stu_prd %>%
#     complete(
#       nesting(ytd, year, system, system_name, school, school_name, ipn, state_id),
#       period
#     )
# }

abs_stu_prd <-
  abs_stu_prd %>%
  # # For each student, the following inner join filters out periods for which
  # # the student was not enrolled, enabling accurate student counts by period
  # # for the abs_sch_prd table.
  # inner_join(
  #   enr_stu_prd %>%
  #     filter(enr == 1) %>%
  #     select(year, system, school, ipn, state_id, period),
  #   by = c("year", "system", "school", "ipn", "state_id", "period")
  # ) %>%
  
  # Need to set up cal_prd for ytd joins
  inner_join(cal_prd, by = c("year", "system", "school", "period")) %>%
  mutate(
    n_abs = if_else(is.na(period), 0, n_abs),
    pct_abs = 100 * n_abs / days_prd,
    chra = as.numeric(pct_abs >= 10)
  ) %>%
  filter(days_prd >= 15) %>%
  mutate_at(vars(n_abs, pct_abs, chra), funs(if_else(is.na(.), 0, .))) %>%
  arrange(year, system_name, school_name, state_id, period)

# abs_sch ----

abs_sch <-
  abs_stu %>%
  group_by_at(vars(ytd:sch_grp)) %>%
  summarize(
    avg_attend = 100 - 100 * sum(n_abs) / sum(days_enr),
    med_abs_chra = median(pct_abs[chra == 1]),
    med_abs_not = median(pct_abs[chra == 0]),
    # avg_attend_chra = 100 - 100 * sum(n_abs[chra == 1]) / sum(days_enr[chra == 1]),
    # avg_attend_not = 100 - 100 * sum(n_abs[chra == 0]) / sum(days_enr[chra == 0]),
    n_stus = as.numeric(n_distinct(state_id)),
    n_chra = as.numeric(n_distinct(state_id[chra == 1]))
  ) %>%
  ungroup() %>%
  mutate(pct_chra = 100 * n_chra / n_stus) %>%
  arrange(system_name, school_name, year, ytd)

# abs_pri ----

abs_pri <-
  abs_stu %>%
  filter(sch_grp == "TN Priority Schools") %>%
  group_by(ytd, year, sch_grp) %>%
  summarize(
    avg_attend = 100 - 100 * sum(n_abs) / sum(days_enr),
    med_abs_chra = median(pct_abs[chra == 1]),
    med_abs_not = median(pct_abs[chra == 0]),
    n_stus = as.numeric(n_distinct(state_id)),
    n_chra = as.numeric(n_distinct(state_id[chra == 1]))
  ) %>%
  ungroup() %>%
  mutate(pct_chra = 100 * n_chra / n_stus) %>%
  rename(school_name = sch_grp) %>%
  arrange(year, ytd)

# abs_sch_grd ----

# abs_sch_grp ----

abs_sch_grp <-
  abs %>%
  group_by_at(vars(ytd:school_name, state_id, days_enr)) %>%
  summarize(n_abs = as.numeric(n_distinct(id_num))) %>%
  ungroup()

if(asd) {
  abs_sch_grp <-
    abs_sch_grp %>%
    left_join(
      demos_stu %>% select(-system_name, -operator, -school_name, -ipn),
      by = c("year", "system", "school", "state_id")
    )
} else {
  abs_sch_grp <-
    abs_sch_grp %>%
    left_join(
      demos_stu %>% select(-system_name, -school_name, -ipn),
      by = c("year", "system", "school", "state_id")
    )
}

abs_sch_grp <-
  abs_sch_grp %>%
  mutate(
    pct_abs = 100 * n_abs / days_enr,
    chra = as.numeric(pct_abs >= 10),
    Male = as.numeric(gender == "M"),
    Black = as.numeric(race == "B"),
    Hispanic = as.numeric(ethnicity == "H"),
    White = as.numeric(race == "W"),
    BHN = as.numeric(race %in% c("B", "I") | ethnicity == "H")
  ) %>%
  rename_at(vars(ell, ed, swd), toupper) %>%
  gather(grp, grp_val, Male, BHN, ED, SWD, ELL, Black, Hispanic, White) %>%
  mutate(
    grp = if_else(grp_val == 1, grp, str_c("Not ", grp)),
    grp = if_else(grp == "Not Male", "Female", grp)
  ) %>%
  filter(grp %in% c("BHN", "Black", "ED", "ELL", "Female", "Hispanic", "Male",
                    "Not ED", "Not ELL", "Not SWD", "SWD", "White")) %>%
  group_by_at(vars(ytd:school_name, grp)) %>%
  summarize(
    avg_attend = 100 - 100 * sum(n_abs) / sum(days_enr),
    n_stus = as.numeric(n_distinct(state_id)),
    n_chra = as.numeric(n_distinct(state_id[chra == 1]))
  ) %>%
  ungroup() %>%
  mutate(pct_chra = 100 * n_chra / n_stus) %>%
  arrange(system_name, school_name, grp, year, ytd)
  
# abs_sch_prd ----

abs_sch_prd <-
  abs_stu_prd %>%
  group_by_at(vars(year:school_name, period)) %>%
  summarize(
    n_stus = as.numeric(n_distinct(state_id)),
    n_chra = as.numeric(n_distinct(state_id[chra == 1]))
  ) %>%
  ungroup() %>%
  mutate(pct_chra = 100 * n_chra / n_stus) %>%
  arrange(year, system_name, school_name, period)

# abs_dst ----

abs_dst <-
  abs_stu %>%
  group_by_at(vars(ytd:system_name)) %>%
  summarize(
    avg_attend = 100 - 100 * sum(n_abs) / sum(days_enr),
    med_abs_chra = median(pct_abs[chra == 1]),
    med_abs_not = median(pct_abs[chra == 0]),
    n_stus = as.numeric(n_distinct(state_id)),
    n_chra = as.numeric(n_distinct(state_id[chra == 1]))
  ) %>%
  ungroup() %>%
  mutate(pct_chra = 100 * n_chra / n_stus) %>%
  arrange(system_name, year, ytd)

# abs_dst_grp ----

abs_dst_grp <-
  abs %>%
  group_by_at(vars(ytd:school_name, state_id, days_enr)) %>%
  summarize(n_abs = as.numeric(n_distinct(id_num))) %>%
  ungroup()

if(asd) {
  abs_dst_grp <-
    abs_dst_grp %>%
    left_join(
      demos_stu %>% select(-system_name, -operator, -school_name, -ipn),
      by = c("year", "system", "school", "state_id")
    )
} else {
  abs_dst_grp <-
    abs_dst_grp %>%
    left_join(
      demos_stu %>% select(-system_name, -school_name, -ipn),
      by = c("year", "system", "school", "state_id")
    )
}

abs_dst_grp <-
  abs_dst_grp %>%
  mutate(
    pct_abs = 100 * n_abs / days_enr,
    chra = as.numeric(pct_abs >= 10),
    Male = as.numeric(gender == "M"),
    Black = as.numeric(race == "B"),
    Hispanic = as.numeric(ethnicity == "H"),
    White = as.numeric(race == "W"),
    BHN = as.numeric(race %in% c("B", "I") | ethnicity == "H")
  ) %>%
  rename_at(vars(ell, ed, swd), toupper) %>%
  gather(grp, grp_val, Male, BHN, ED, SWD, ELL, Black, Hispanic, White) %>%
  mutate(
    grp = if_else(grp_val == 1, grp, str_c("Not ", grp)),
    grp = if_else(grp == "Not Male", "Female", grp)
  ) %>%
  filter(grp %in% c("BHN", "Black", "ED", "ELL", "Female", "Hispanic", "Male",
                    "Not ED", "Not ELL", "Not SWD", "SWD", "White")) %>%
  group_by_at(vars(ytd:system_name, grp)) %>%
  summarize(
    avg_attend = 100 - 100 * sum(n_abs) / sum(days_enr),
    n_stus = as.numeric(n_distinct(state_id)),
    n_chra = as.numeric(n_distinct(state_id[chra == 1]))
  ) %>%
  ungroup() %>%
  mutate(pct_chra = 100 * n_chra / n_stus) %>%
  arrange(system_name, grp, year, ytd)

# abs_dst_prd ----

abs_dst_prd <-
  abs_stu_prd %>%
  group_by(year, system, system_name, period) %>%
  summarize(
    n_stus = n_distinct(state_id),
    n_chra = n_distinct(state_id[chra == 1])
  ) %>%
  ungroup() %>%
  mutate(pct_chra = 100 * n_chra / n_stus)

# abs_20d <-
#   abs_inc %>%
#   filter(id_num > now_num - 20)

# still messed up, need to fix
# abs_enr_prd <-
#   abs_inc %>%
#   group_by(year, system, school, state_id, isp_id, period) %>%
#   summarize(n_abs = n_distinct(id_num)) %>%
#   ungroup() %>%
#   mutate(
#     pct_abs = 100 * n_abs / 20,
#     chra = as.numeric(pct_abs >= 10),
#     dummy = n_abs
#   ) %>%
#   spread(period, dummy) %>%
#   gather(period, dummy, `1`:`10`) %>%
#   mutate(period = as.numeric(period)) %>%
#   arrange(year, system, school, state_id, isp_id, period)
