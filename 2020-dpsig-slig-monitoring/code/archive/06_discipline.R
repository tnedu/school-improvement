# dsc ----

# Query discipline incidents from EIS, filter incidents that actually occur
# during enrollments (based on start and end dates), add variables from
# calendar, classification, disability, and enrollment tables (instructional
# day numbers and periods), then fix missing values in those variables.

dsc <-
  sch %>%
  left_join(
    get_disc(years = yrs, districts = unique(sch[["system"]])) %>%
      select(-SYSTEM_NAME, -SCHOOL_NAME),
    by = c("system" = "SYSTEM", "school" = "SCHOOL")
  ) %>%
  rename_all(tolower) %>%
  mutate_at(vars(ends_with("_date")), as_date) %>%
  left_join(
    enr %>% select(isp_id, begin_date, end_date),
    by = "isp_id"
  ) %>%
  filter(!is.na(da_begin_date)) %>%
  filter(da_begin_date < end_date | is.na(end_date)) %>%
  filter(is.na(da_end_date) | da_end_date >= begin_date) %>%
  full_join(
    cal %>%
      transmute(year, system, school, id_date,
                begin_num = id_num, begin_prd = period),
    by = c("year", "system", "school", "da_begin_date" = "id_date")
  ) %>%
  full_join(
    cal %>%
      transmute(year, system, school, id_date,
                end_num = id_num, end_prd = period),
    by = c("year", "system", "school", "da_end_date" = "id_date")
  ) %>%
  left_join(cal_now, by = c("system", "school")) %>%
  group_by(year, system, school) %>%
  mutate(
    begin_num =
      case_when(
        is.na(begin_num) &
          (is.na(da_begin_date) |
             da_begin_date < min(da_begin_date[!is.na(begin_num)])) ~ 1,
        is.na(begin_num) ~ max(begin_num, na.rm = T),
        T ~ begin_num
      ),
    end_num =
      case_when(
        is.na(end_num) &
          (is.na(da_end_date) |
             da_end_date > max(da_end_date[!is.na(end_num)])) ~ max(end_num, na.rm = T),
        is.na(end_num) ~ 1,
        T ~ end_num
      ),
    begin_prd = ceiling(begin_num / 20),
    end_prd = ceiling(end_num / 20)
  ) %>%
  ungroup() %>%
  filter(!is.na(isp_id)) %>%
  select(
    year, system:school_name, state_id, everything(),
    -begin_date, -end_date
  ) %>%
  arrange(year, system, school, isp_id, da_begin_date)

dsc <-
  dsc %>%
  mutate(ytd = 0) %>%
  bind_rows(
    dsc %>%
      filter(begin_num <= now_num) %>%
      mutate(ytd = 1)
  )

# dsc_stu ----

dsc_stu <-
  enr_stu %>%
  left_join(
    sch %>% select(system, school, sch_grp),
    by = c("system", "school")
  ) %>%
  left_join(
    dsc %>%
      mutate(
        days_disciplined = if_else(discipline_action == "R",
                                   0, days_disciplined),
        last_30 = begin_num >= now_num - 40 & begin_num < now_num - 10
      ) %>%
      group_by(ytd, year, system, school, state_id) %>%
      summarize(
        n_inc = n_distinct(begin_num),
        n_inc_30 = n_distinct(begin_num[last_30]),
        n_days = sum(days_disciplined)
      ) %>%
      ungroup(),
    by = c("ytd", "year", "system", "school", "state_id")
  ) %>%
  mutate_at(
    vars(n_inc, n_inc_30, n_days),
    funs(if_else(is.na(.), 0, as.numeric(.)))
  ) %>%
  select(ytd:school_name, sch_grp, everything()) %>%
  arrange(ytd, year, sch_grp, system_name, school_name, state_id)

# dsc_stu_type ----

# Not sure this table is necessary

# dsc_stu_type <-
#   enr_stu %>%
#   left_join(
#     dsc %>%
#       mutate(
#         days_disciplined = if_else(discipline_action == "R",
#                                    0, days_disciplined)
#       ) %>%
#       group_by_at(vars(ytd, year:sch_grp, discipline_action)) %>%
#       summarize(
#         n_inc = n_distinct(begin_num),
#         n_days = sum(days_disciplined)
#       ) %>%
#       ungroup() %>%
#       select(-system_name, -school_name),
#     by = c("ytd", "year", "system", "school", "state_id")
#   ) %>%
#   mutate_at(
#     vars(n_inc, n_days),
#     funs(if_else(is.na(.), 0, as.numeric(.)))
#   ) %>%
#   mutate(discipline_action = if_else(is.na(discipline_action), "N", discipline_action)) %>%
#   arrange(ytd, year, system_name, school_name, state_id, discipline_action)

# dsc_stu_prd ----

# Count disciplinary incidents by 20-day period for each student. Filter
# periods with at least 15 days.

dsc_stu_prd <-
  enr_stu %>%
  left_join(
    sch %>% select(system, school, sch_grp),
    by = c("system", "school")
  ) %>%
  left_join(
    dsc %>%
      filter(ytd == 0) %>%
      mutate(
        days_disciplined = if_else(discipline_action == "R",
                                   0, days_disciplined)
      ) %>%
      group_by(year, system, school, state_id, begin_prd) %>%
      summarize(
        n_inc = as.numeric(n_distinct(begin_num)),
        n_days = sum(days_disciplined)
      ) %>%
      ungroup(),
    by = c("year", "system", "school", "state_id")
  )

if(asd) {
  dsc_stu_prd <-
    dsc_stu_prd %>%
    complete(
      nesting(year, system, system_name, operator, school, school_name, sch_grp, ipn,
              state_id),
      begin_prd
    )
} else {
  dsc_stu_prd <-
    dsc_stu_prd %>%
    complete(
      nesting(year, system, system_name, school, school_name, sch_grp, ipn, state_id),
      begin_prd
    )
}

dsc_stu_prd <-
  dsc_stu_prd %>%
  # For each student, the following inner join filters out periods for which
  # the student was not enrolled, enabling accurate student counts by period
  # for the abs_sch_prd table.
  inner_join(
    enr_stu_prd %>%
      filter(enr == 1) %>%
      select(year, system, school, state_id, period),
    by = c("year", "system", "school", "state_id", "begin_prd" = "period")
  ) %>%
  inner_join(
    cal_prd,
    by = c("year", "system", "school", "begin_prd" = "period")
  ) %>%
  filter(days_prd >= 15) %>%
  mutate_at(
    vars(n_inc, n_days),
    funs(if_else(is.na(.), 0, .))
  ) %>%
  arrange(year, sch_grp, system_name, school_name, state_id, begin_prd)

# dsc_sch ----

# Note that tot_days_dsc ignores remands (which can skew discipline day
# counts), and pct_days_dsc only reflects students with at least one
# disciplinary incident.

dsc_sch <-
  dsc_stu %>%
  group_by_at(vars(ytd:ipn)) %>%
  # programming opportunity (summarize, ungroup, mutate) ----
  summarize(
    n_stus = as.numeric(n_distinct(state_id)),
    n_stus_dsc = as.numeric(n_distinct(state_id[n_inc > 0])),
    n_stus_30 = as.numeric(n_distinct(state_id[last_30 == 1])),
    n_stus_dsc_30 = as.numeric(n_distinct(state_id[last_30 == 1 & n_inc_30 > 0])),
    n_inc = sum(n_inc),
    n_inc_30 = sum(n_inc_30),
    tot_days_enr = sum(days_enr[n_inc > 0]),
    tot_days_dsc = sum(n_days)
  ) %>%
  ungroup() %>%
  mutate(
    n_inc_per_stu = n_inc / n_stus,
    pct_stus_dsc = 100 * n_stus_dsc / n_stus,
    pct_stus_dsc_30 = 100 * n_stus_dsc_30 / n_stus_30,
    pct_days_dsc = 100 * tot_days_dsc / tot_days_enr
  ) %>%
  arrange(sch_grp, system_name, school_name, year, ytd)

# dsc_pri ----

dsc_pri <-
  dsc_stu %>%
  filter(sch_grp == "TN Priority Schools") %>%
  group_by(ytd, year, sch_grp) %>%
  summarize(
    n_stus = as.numeric(n_distinct(state_id)),
    n_stus_dsc = as.numeric(n_distinct(state_id[n_inc > 0])),
    n_inc = sum(n_inc),
    tot_days_enr = sum(days_enr[n_inc > 0]),
    tot_days_dsc = sum(n_days)
  ) %>%
  ungroup() %>%
  mutate(
    n_inc_per_stu = n_inc / n_stus,
    pct_stus_dsc = 100 * n_stus_dsc / n_stus,
    pct_days_dsc = 100 * tot_days_dsc / tot_days_enr
  ) %>%
  rename(school_name = sch_grp) %>%
  arrange(year, ytd)

# dsc_sch_grp ----

# dsc_sch_grp <-
#   dsc %>%
#   group_by_at(vars(year:state_id)) %>%
#   summarize(n_abs = as.numeric(n_distinct(id_num))) %>%
#   ungroup()

if(asd) {
  dsc_sch_grp <-
    dsc_stu %>%
    left_join(
      demos_stu %>%
        select(-system_name, -operator, -school_name, -sch_grp, -ipn, -local_id),
      by = c("year", "system", "school", "state_id")
    )
} else {
  dsc_sch_grp <-
    dsc_stu %>%
    left_join(
      demos_stu %>% select(-system_name, -school_name, -sch_grp, -ipn, -local_id),
      by = c("year", "system", "school", "state_id")
    )
}

dsc_sch_grp <-
  dsc_sch_grp %>%
  mutate(
    # pct_abs = 100 * n_abs / days_enr,
    # chra = as.numeric(pct_abs >= 10),
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
  group_by_at(vars(ytd:ipn, grp)) %>%
  summarize(
    n_stus = as.numeric(n_distinct(state_id)),
    n_stus_dsc = as.numeric(n_distinct(state_id[n_inc > 0])),
    tot_days_enr = sum(days_enr[n_inc > 0]),
    tot_days_dsc = sum(n_days)
  ) %>%
  ungroup() %>%
  mutate(
    pct_stus_dsc = 100 * n_stus_dsc / n_stus,
    pct_days_dsc = 100 * tot_days_dsc / tot_days_enr
  ) %>%
  arrange(system_name, school_name, grp, year, ytd)

# dsc_sch_type ----

dsc_sch_type <-
  dsc %>%
  group_by_at(vars(ytd, year:school_name, sch_grp)) %>%
  mutate(n_inc_tot = n_distinct(state_id, da_begin_date)) %>%
  group_by(n_inc_tot, discipline_action, add = T) %>%
  summarize(n_inc_type = n_distinct(state_id, da_begin_date)) %>%
  ungroup() %>%
  mutate(pct_inc_type = 100 * n_inc_type / n_inc_tot) %>%
  arrange_at(vars(ytd, year:school_name, sch_grp, discipline_action))

# dsc_sch_prd ----

dsc_sch_prd <-
  dsc_stu_prd %>%
  group_by_at(vars(year:ipn, begin_prd)) %>%
  summarize(
    n_stus = as.numeric(n_distinct(state_id)),
    n_stus_dsc = as.numeric(n_distinct(state_id[n_inc > 0])),
    tot_days_enr = sum(days_enr[n_inc > 0]),
    tot_days_dsc = sum(n_days)
  ) %>%
  ungroup() %>%
  mutate(
    pct_stus_dsc = 100 * n_stus_dsc / n_stus,
    pct_days_dsc = 100 * tot_days_dsc / tot_days_enr
  ) %>%
  arrange(year, system_name, school_name, begin_prd)

# dsc_dst ----

# Note that in some cases, the "district" here is not a full district (e.g.,
# the Partnership Network).

dsc_dst <-
  dsc_stu %>%
  group_by_at(vars(ytd:system_name)) %>%
  summarize(
    n_stus = as.numeric(n_distinct(state_id)),
    n_stus_dsc = as.numeric(n_distinct(state_id[n_inc > 0])),
    tot_days_enr = sum(days_enr[n_inc > 0]),
    tot_days_dsc = sum(n_days)
  ) %>%
  ungroup() %>%
  mutate(
    pct_stus_dsc = 100 * n_stus_dsc / n_stus,
    pct_days_dsc = 100 * tot_days_dsc / tot_days_enr
  ) %>%
  arrange(system_name, year, ytd)

# dsc_dst_grp ----

# dsc_dst_grp <-
#   abs %>%
#   group_by_at(vars(year:school_name, state_id, days_enr)) %>%
#   summarize(n_abs = as.numeric(n_distinct(id_num))) %>%
#   ungroup()

if(asd) {
  dsc_dst_grp <-
    dsc_stu %>%
    left_join(
      demos_stu %>%
        select(-system_name, -operator, -school_name, -sch_grp, -ipn, -local_id),
      by = c("year", "system", "school", "state_id")
    )
} else {
  dsc_dst_grp <-
    dsc_stu %>%
    left_join(
      demos_stu %>% select(-system_name, -school_name, -sch_grp, -ipn, -local_id),
      by = c("year", "system", "school", "state_id")
    )
}

dsc_dst_grp <-
  dsc_dst_grp %>%
  mutate(
    # pct_abs = 100 * n_abs / days_enr,
    # chra = as.numeric(pct_abs >= 10),
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
    n_stus = as.numeric(n_distinct(state_id)),
    n_stus_dsc = as.numeric(n_distinct(state_id[n_inc > 0])),
    tot_days_enr = sum(days_enr[n_inc > 0]),
    tot_days_dsc = sum(n_days)
  ) %>%
  ungroup() %>%
  mutate(
    pct_stus_dsc = 100 * n_stus_dsc / n_stus,
    pct_days_dsc = 100 * tot_days_dsc / tot_days_enr
  ) %>%
  arrange(system_name, grp, year, ytd)

# dsc_dst_prd ----

dsc_dst_prd <-
  dsc_stu_prd %>%
  group_by(year, system, system_name, begin_prd) %>%
  summarize(
    n_stus = as.numeric(n_distinct(state_id)),
    n_stus_dsc = as.numeric(n_distinct(state_id[n_inc > 0])),
    tot_days_enr = sum(days_enr[n_inc > 0]),
    tot_days_dsc = sum(n_days)
  ) %>%
  ungroup() %>%
  mutate(
    pct_stus_dsc = 100 * n_stus_dsc / n_stus,
    pct_days_dsc = 100 * tot_days_dsc / tot_days_enr
  ) %>%
  arrange(year, system, system_name, begin_prd)
