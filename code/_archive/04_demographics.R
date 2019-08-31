# enr ----

# Query enrollments from EIS, add variables from calendar tables
# (instructional day numbers and periods), then fix missing values in those
# variables.

# Note that end_date is the student's first day not enrolled, but id_num and
# period reflect the last day enrolled (through the join on end_date_1).

# The dplyr::distinct command below collapses multiple grade levels within a
# small number of ISPs.

enr <-
  schools_csi_2019 %>%
  left_join(
    dbGetQuery(con_eis, sql_2_txt("enrollment", years = yrs)),
    by = c("system" = "SYSTEM", "school" = "SCHOOL")
  )

enr <-
  enr %>%
  rename_all(tolower) %>%
  distinct(isp_id, .keep_all = T) %>%
  mutate_at(vars(ends_with("_date")), as_date) %>%
  mutate(end_date_1 = end_date - 1) %>%
  full_join(
    cal %>%
      transmute(year, system, school, id_date,
                begin_num = id_num, begin_prd = period),
    by = c("year", "system", "school", "begin_date" = "id_date")
  ) %>%
  full_join(
    cal %>%
      transmute(year, system, school, id_date,
                end_num = id_num, end_prd = period),
    by = c("year", "system", "school", "end_date_1" = "id_date")
  ) %>%
  left_join(cal_now, by = c("system", "school")) %>%
  group_by(year, system, school) %>%
  mutate(
    # begin_num should equal 1 where (A) there is no begin_date or (B) where
    # the begin_date is less than the earliest begin_date with a non-missing
    # begin_num. If both of these conditions are FALSE but begin_num is still
    # missing, then begin_num should be the maximum begin_num (i.e., the last
    # day of school).
    begin_num =
      case_when(
        is.na(begin_num) &
          (is.na(begin_date) |
             begin_date < min(begin_date[!is.na(begin_num)])) ~ 1,
        is.na(begin_num) ~ max(begin_num, na.rm = T),
        T ~ begin_num
      ),
    # end_num should equal the maximum end_num (i.e., the last day of school)
    # where (A) there is no end_date or (B) where the end_date is greater than
    # the latest end_date with a non-missing end_num. If both of these
    # conditions are FALSE but end_num is still missing, then end_num should be
    # 1.
    end_num =
      case_when(
        is.na(end_num) &
          (is.na(end_date) |
             end_date > max(end_date_1[!is.na(end_num)]) + 1) ~ max(end_num, na.rm = T),
        is.na(end_num) ~ 1,
        T ~ end_num
      )
  ) %>%
  ungroup() %>%
  filter(!is.na(isp_id)) %>%
  mutate(
    end_num = if_else(year == max(year, na.rm = T), pmin(end_num, now_num), end_num),
    begin_prd = ceiling(begin_num / 20),
    end_prd = ceiling(end_num / 20)
  ) %>%
  select(
    year, system:school_name, ipn:begin_prd, end_num, end_prd,
    everything(), -end_date_1
  ) %>%
  arrange(year, system, school, isp_id, begin_date)

# enr_stu ----

# Tidy enrollments to the student level to determine days_enr for each
# year-system-school-student combination.

enr_stu <-
  enr %>%
  mutate(ytd = 0) %>%
  bind_rows(
    enr %>%
      filter(begin_num <= now_num) %>%
      mutate(
        end_num = if_else(end_num > now_num, now_num, end_num),
        ytd = 1
      )
  ) %>%
  group_by(year, ytd) %>%
  nest() %>%
  filter(year != max(year) | ytd == 1) %>%
  mutate(
    data = map(
      data,
      ~ .x %>%
        # inner_join(schools_csi_2019 %>% select(system, school)) %>%
        distinct(isp_id, .keep_all = T) %>%
        # filter(
        #   !is.na(begin_num), !is.na(end_num), !is.na(now_num),
        #   begin_num <= now_num
        # ) %>%
        mutate(days_enrolled = end_num - begin_num + 1) %>%
        group_by(system, system_name, school, school_name, ipn) %>%
        group_by(state_id, add = T) %>%
        summarize(
          days_enrolled = sum(days_enrolled)
          # now_num_max = max(now_num)
        ) %>%
        ungroup() %>%
        # mutate_at(vars(days_enrolled), funs(pmin(days_enrolled, now_num_max))) %>%
        mutate_at(vars(days_enrolled), funs(pmin(days_enrolled, 180))) %>%
        group_by(system, system_name, school, school_name, ipn) %>%
        filter(days_enrolled >= 0.5 * max(days_enrolled, na.rm = T)) %>%
        ungroup()
    )
  ) %>%
  unnest()

# enr_stu <-
#   enr %>%
#   mutate(ytd = 0) %>%
#   bind_rows(
#     enr %>%
#       filter(begin_num <= pmin(end_num, now_num)) %>%
#       mutate(
#         end_num = if_else(end_num > now_num, now_num, end_num),
#         ytd = 1
#       )
#   ) %>%
#   group_by(ytd) %>%
#   nest() %>%
#   mutate(
#     data =
#       map(data, function(df) {
#         df_new <-
#           df %>%
#           group_by(year, system, school, ipn, state_id) %>%
#           mutate(
#             begin = str_c("bnum_", min_rank(isp_id)),
#             end = str_c("enum_", min_rank(isp_id)),
#             # This specification shifts the 30-day period 10 days earlier to
#             # avoid low-quality data. (For example, if a district enters
#             # discipline data in weekly batches, then those recent data would
#             # bias our views here.)
#             last_30 = as.numeric(begin_num <= now_num - 10 & end_num > now_num - 40)
#           ) %>%
#           ungroup() %>%
#           distinct(isp_id, begin_num, .keep_all = T) %>%
#           distinct(isp_id, end_num, .keep_all = T) %>%
#           spread(begin, begin_num) %>%
#           spread(end, end_num) %>%
#           group_by_at(vars(year:state_id)) %>%
#           mutate_at(vars(local_id, last_30), max, na.rm = T) %>%
#           group_by(local_id, last_30, add = T) %>%
#           summarize_at(
#             vars(starts_with("bnum_"), starts_with("enum_")),
#             max, na.rm = T
#           ) %>%
#           ungroup() %>%
#           mutate_at(
#             vars(starts_with("bnum_"), starts_with("enum_")),
#             funs(if_else(. == -Inf, NA_real_, .))
#           ) %>%
#           mutate(days_enr = 0)
#         return(df_new)
#       })
#   ) %>%
#   unnest()
# 
# max_enr <- sum(grepl("bnum", names(enr_stu)))
# 
# # for(e in 2:max_enr) {
# #   for(n in c("bnum_", "enum_")) {
# #     col <- str_c(n, e)
# #     col_prev <- str_c(n, e - 1)
# #     enr_stu[[col]] <- if_else(is.na(enr_stu[[col]]),
# #                               enr_stu[[col_prev]],
# #                               enr_stu[[col]])
# #   }
# # }
# 
# for(i in 1:nrow(enr_stu)) {
#   days_dup <- NULL
#   for(e in 1:max_enr) {
#     col_beg <- str_c("bnum_", e)
#     col_end <- str_c("enum_", e)
#     # 2019-03-14 added the following two lines
#     val_beg <- enr_stu[[col_beg]][i]
#     if(is.na(val_beg)) {break}
#     days_dup <- c(days_dup, seq(enr_stu[[col_beg]][i], enr_stu[[col_end]][i]))
#   }
#   enr_stu$days_enr[i] <- n_distinct(days_dup)
# }
# 
# # If this statement is FALSE, then adjust the loop below by adding or removing
# # else-if statements.
# # stopifnot(sum(grepl("bnum", names(enr_stu))) == 5)
# 
# # for(i in 1:nrow(enr_stu)) {
# #   # for(v in 2:sum(grepl("bnum", names(enr_stu)))) {
# #   #   
# #   # }
# #   if(is.na(enr_stu$bnum_2[i])) {
# #     enr_stu$days_enr[i] <- enr_stu$enum_1[i] - enr_stu$bnum_1[i] + 1
# #   } else if(is.na(enr_stu$bnum_3[i])) {
# #     enr_stu$days_enr[i] <-
# #       length(unique(c(seq(enr_stu$bnum_1[i], enr_stu$enum_1[i]),
# #                       seq(enr_stu$bnum_2[i], enr_stu$enum_2[i]))))
# #   } else if(is.na(enr_stu$bnum_4[i])) { # if(is.na(enr_stu$bnum_4[i]))
# #     enr_stu$days_enr[i] <-
# #       length(unique(c(seq(enr_stu$bnum_1[i], enr_stu$enum_1[i]),
# #                       seq(enr_stu$bnum_2[i], enr_stu$enum_2[i]),
# #                       seq(enr_stu$bnum_3[i], enr_stu$enum_3[i]))))
# #   } else if(is.na(enr_stu$bnum_5[i])) {
# #     enr_stu$days_enr[i] <-
# #       length(unique(c(seq(enr_stu$bnum_1[i], enr_stu$enum_1[i]),
# #                       seq(enr_stu$bnum_2[i], enr_stu$enum_2[i]),
# #                       seq(enr_stu$bnum_3[i], enr_stu$enum_3[i]),
# #                       seq(enr_stu$bnum_4[i], enr_stu$enum_4[i]))))
# #   } else {
# #     enr_stu$days_enr[i] <-
# #       length(unique(c(seq(enr_stu$bnum_1[i], enr_stu$enum_1[i]),
# #                       seq(enr_stu$bnum_2[i], enr_stu$enum_2[i]),
# #                       seq(enr_stu$bnum_3[i], enr_stu$enum_3[i]),
# #                       seq(enr_stu$bnum_4[i], enr_stu$enum_4[i]),
# #                       seq(enr_stu$bnum_5[i], enr_stu$enum_5[i]))))
# #   }
# # }
# 
# enr_stu <-
#   enr_stu %>%
#   select(-starts_with("bnum"), -starts_with("enum")) %>%
#   arrange(ytd, year, system, school, state_id)

# enr_stu_prd ----

# Tidy enrollments to the student-by-period level to create an "enrolled"
# indicator by period for each student (to use for filtering in subsequent
# scripts).

enr_stu_prd <-
  enr %>%
  inner_join(schools_csi_2019 %>% select(system, school))

for(p in 1:max(enr$end_prd, na.rm = T)) {
  attach(enr_stu_prd)
  v <- str_c("prd_", p)
  enr_stu_prd[[v]] <- 0
  enr_stu_prd[[v]] <- if_else(p >= begin_prd & p <= end_prd, 1, 0)
  detach(enr_stu_prd)
}

enr_stu_prd <-
  enr_stu_prd %>%
  group_by(year, system, school, ipn, state_id, local_id) %>%
  summarize_at(vars(starts_with("prd_")), max) %>%
  ungroup() %>%
  gather(period, enr, starts_with("prd_")) %>%
  mutate(period = as.numeric(gsub("prd_", "", period))) %>%
  arrange(year, system, school, state_id, period)

# cla ----

# Query student classifications from EIS, filter classifications that actually
# overlap enrollments (based on start and end dates), add variables from
# calendar tables (instructional day numbers and periods), then fix missing
# values in those variables.

cla <-
  dbGetQuery(con_eis, sql_2_txt("classification", years = yrs)) %>%
  rename_all(tolower) %>%
  mutate_at(vars(ends_with("_date")), as_date) %>%
  inner_join(
    enr %>% inner_join(schools_csi_2019 %>% select(system, school)) %>% select(isp_id, begin_date, end_date),
    by = "isp_id"
  ) %>%
  filter(!is.na(sc_begin_date)) %>%
  filter(sc_begin_date < end_date | is.na(end_date)) %>%
  filter(is.na(sc_end_date) | sc_end_date >= begin_date) %>%
  full_join(
    cal %>%
      transmute(year, system, school, id_date,
                begin_num = id_num, begin_prd = period),
    by = c("year", "system", "school", "sc_begin_date" = "id_date")
  ) %>%
  full_join(
    cal %>%
      transmute(year, system, school,
                id_date, end_num = id_num, end_prd = period),
    by = c("year", "system", "school", "sc_end_date" = "id_date")
  ) %>%
  left_join(cal_now, by = c("system", "school")) %>%
  group_by(year, system, school) %>%
  mutate(
    # See notes in the enr section above.
    begin_num =
      case_when(
        is.na(begin_num) &
          (is.na(sc_begin_date) |
             sc_begin_date < min(sc_begin_date[!is.na(begin_num)])) ~ 1,
        is.na(begin_num) ~ max(begin_num, na.rm = T),
        T ~ begin_num
      ),
    end_num =
      case_when(
        is.na(end_num) &
          (is.na(sc_end_date) |
             sc_end_date > max(sc_end_date[!is.na(end_num)])) ~ max(end_num, na.rm = T),
        is.na(end_num) ~ 1,
        T ~ end_num
      ),
    begin_prd = ceiling(begin_num / 20),
    end_prd = ceiling(end_num / 20),
    ed = as.numeric(cla_code %in% c("FOS01", "H", "I", "J", "U"))
  ) %>%
  ungroup() %>%
  filter(!is.na(isp_id)) %>%
  select(year:isp_id, ed, everything(), -begin_date, -end_date) %>%
  arrange(year, system, school, state_id, isp_id, sc_begin_date)

cla %>%
  filter(ed == 1) %>%
  mutate(diff = end_num - begin_num + 1) %>%
  extract2("diff") %>%
  summary()

# not sure the tables below are needed

# cla_enr_prd <- cla
# 
# for(i in 1:9) {
#   attach(cla_enr_prd)
#   v <- str_c("prd_", i)
#   cla_enr_prd[[v]] <- as.numeric(i >= begin_prd & i <= end_prd)
#   detach(cla_enr_prd)
# }
# 
# cla_enr_prd <-
#   cla_enr_prd %>%
#   gather(period, prd_val, prd_1:prd_9) %>%
#   mutate(period = as.numeric(gsub("prd_", "", period))) %>%
#   group_by(year, system, school, state_id, isp_id, period) %>%
#   summarize_at(vars(prd_val, ed), max) %>%
#   ungroup() %>%
#   arrange(year, system, school, state_id, isp_id, period)
# 
# cla_enr <-
#   cla %>%
#   group_by(year, system, school, state_id, isp_id) %>%
#   summarize(ed = max(ed)) %>%
#   ungroup()
# 
# cla_enr_20d <-
#   cla %>%
#   filter(now_num >= begin_num, now_num - 20 <= end_num) %>%
#   group_by(year, system, school, state_id, isp_id) %>%
#   summarize(ed = max(ed)) %>%
#   ungroup()

# dsb ----

# Query student disabilities from EIS, filter disabilities that actually
# overlap enrollments (based on start and end dates), add variables from
# calendar tables (instructional day numbers and periods), then fix missing
# values in those variables.

dsb <-
  dbGetQuery(con_eis, sql_2_txt("disability", years = yrs)) %>%
  rename_all(tolower) %>%
  mutate_at(vars(ends_with("_date")), as_date) %>%
  inner_join(
    enr %>% inner_join(schools_csi_2019 %>% select(system, school)) %>% select(isp_id, begin_date, end_date),
    by = "isp_id"
  ) %>%
  filter(!is.na(dis_begin_date)) %>%
  filter(dis_begin_date < end_date | is.na(end_date)) %>%
  filter(is.na(dis_end_date) | dis_end_date >= begin_date) %>%
  full_join(
    cal %>%
      transmute(year, system, school, id_date,
                begin_num = id_num, begin_prd = period),
    by = c("year", "system", "school", "dis_begin_date" = "id_date")
  ) %>%
  full_join(
    cal %>%
      transmute(year, system, school,
                id_date, end_num = id_num, end_prd = period),
    by = c("year", "system", "school", "dis_end_date" = "id_date")
  ) %>%
  left_join(cal_now, by = c("system", "school")) %>%
  group_by(year, system, school) %>%
  mutate(
    # See notes in the enr section above.
    begin_num =
      case_when(
        is.na(begin_num) &
          (is.na(dis_begin_date) |
             dis_begin_date < min(dis_begin_date[!is.na(begin_num)])) ~ 1,
        is.na(begin_num) ~ max(begin_num, na.rm = T),
        T ~ begin_num
      ),
    end_num =
      case_when(
        is.na(end_num) &
          (is.na(dis_end_date) |
             dis_end_date > max(dis_end_date[!is.na(end_num)])) ~ max(end_num, na.rm = T),
        is.na(end_num) ~ 1,
        T ~ end_num
      ),
    begin_prd = ceiling(begin_num / 20),
    end_prd = ceiling(end_num / 20)
  ) %>%
  ungroup() %>%
  filter(!is.na(isp_id)) %>%
  select(-begin_date, -end_date) %>%
  arrange(year, system, school, state_id, isp_id, dis_begin_date)

dsb %>%
  mutate(diff = end_num - begin_num + 1) %>%
  extract2("diff") %>%
  summary()

# not sure we need these tables

# dsb_enr_prd <- dsb
# 
# for(i in 1:9) {
#   attach(dsb_enr_prd)
#   v <- str_c("prd_", i)
#   dsb_enr_prd[[v]] <- as.numeric(i >= begin_prd & i <= end_prd)
#   detach(dsb_enr_prd)
# }
# 
# dsb_enr_prd <-
#   dsb_enr_prd %>%
#   filter(dis_level == "P") %>%
#   gather(period, swd, prd_1:prd_9) %>%
#   mutate(dis_desc = if_else(swd == 1, dis_desc, "")) %>%
#   group_by(year, system, school, state_id, isp_id, period) %>%
#   summarize(
#     swd = max(swd),
#     dis_desc =
#       gsub("^, |, $", "",
#            glue::glue_collapse(unique(dis_desc), sep = ", "))
#   ) %>%
#   ungroup() %>%
#   transmute(
#     year, system, school, state_id, isp_id,
#     period = as.numeric(gsub("prd_", "", period)),
#     swd, prim_dis = dis_desc
#   ) %>%
#   arrange(year, system, school, state_id, isp_id, period)
# 
# dsb_enr <-
#   dsb %>%
#   filter(dis_level == "P", end_num - begin_num > 10) %>%
#   group_by(year, system, school, state_id, isp_id) %>%
#   summarize(
#     swd = 1,
#     dis_desc =
#       gsub("^, |, $", "",
#            glue::glue_collapse(unique(dis_desc), sep = ", "))
#   ) %>%
#   ungroup() %>%
#   transmute(year, system, school, state_id, isp_id, swd, prim_dis = dis_desc) %>%
#   arrange(year, system, school, state_id, isp_id)
# 
# dsb_enr_20d <-
#   dsb %>%
#   filter(
#     dis_level == "P", end_num - begin_num > 10,
#     now_num >= begin_num, now_num - 20 <= end_num
#   ) %>%
#   group_by(year, system, school, state_id, isp_id) %>%
#   summarize(
#     swd = 1,
#     dis_desc =
#       gsub("^, |, $", "",
#            glue::glue_collapse(unique(dis_desc), sep = ", "))
#   ) %>%
#   ungroup() %>%
#   transmute(year, system, school, state_id, isp_id, swd, prim_dis = dis_desc) %>%
#   arrange(year, system, school, state_id, isp_id)

# enr 2 ---

# not sure we need these tables

# enr <-
#   enr %>%
#   left_join(
#     cla_enr %>% select(isp_id, ed),
#     by = "isp_id"
#   ) %>%
#   left_join(
#     dsb_enr %>% select(isp_id, swd, prim_dis),
#     by = "isp_id"
#   ) %>%
#   mutate_at(vars(ed, swd), funs(if_else(is.na(.), 0, .))) %>%
#   select(year:grade, ed, swd, prim_dis, everything())
# 
# enr_20d <-
#   enr %>%
#   filter(now_num >= begin_num, now_num - 20 <= end_num)
# 
# enr_prd <- enr
# 
# walk(
#   .x = 1:9,
#   .f = function(x) {
#     v <- str_c("prd_", x)
#     enr_prd[[v]] <<-
#       as.numeric(x >= enr_prd[["begin_prd"]] & x <= enr_prd[["end_prd"]])
#   }
# )
# 
# enr_prd <-
#   enr_prd %>%
#   gather(period, enr, prd_1:prd_9) %>%
#   transmute(
#     year, system, school, ipn, state_id, local_id, isp_id,
#     period = as.numeric(gsub("prd_", "", period)), enr,
#     grade, ed, swd, prim_dis, elb, elb_desc
#   ) %>%
#   arrange(year, system, school, state_id, isp_id, period)

# stu ----

stu <-
  dbGetQuery(con_eis, sql_2_txt("student", years = yrs)) %>%
  rename_all(tolower) %>%
  distinct(state_id, .keep_all = T) %>%
  mutate(begin_date_usa = as_date(begin_date_usa)) %>%
  # right_join(
  #   enr %>%
  #     mutate(ell = if_else(elb %in% c("L", "N", as.character(1:4)), 1, 0)) %>%
  #     group_by_at(vars(year:local_id)) %>%
  #     summarize(
  #       grade = glue::glue_collapse(unique(grade), ", "),
  #       ell = max(ell)
  #     ) %>%
  #     ungroup(),
  #   by = "state_id"
  # ) 
  # select(year:ipn, state_id, local_id)
  arrange(state_id)

# demos_stu ----

demos_stu <-
  cla %>%
  mutate(ed = if_else(end_num - begin_num + 1 >= 20, ed, 0)) %>%
  group_by(isp_id) %>%
  summarize(ed = max(ed)) %>%
  ungroup() %>%
  full_join(
    dsb %>%
      filter(end_num - begin_num + 1 >= 20) %>%
      distinct(isp_id) %>%
      mutate(swd = 1),
    by = "isp_id"
  ) %>%
  right_join(enr %>% inner_join(schools_csi_2019 %>% select(system, school)), by = "isp_id") %>%
  mutate_at(vars(ed, swd), funs(if_else(is.na(.), 0, .))) %>%
  mutate(ell = if_else(elb %in% c("L", "N", as.character(1:4)), 1, 0)) %>%
  group_by(year, system, school, ipn, state_id, local_id) %>%
  summarize(
    grade = glue::glue_collapse(unique(grade), ", "),
    ed = max(ed),
    ell = max(ell),
    swd = max(swd)
  ) %>%
  ungroup() %>%
  left_join(stu, by = "state_id") %>%
  left_join(sch, by = c("system", "school"))

if(asd) {
  demos_stu <-
    demos_stu %>%
    select(
      year, system, system_name, operator, school, school_name, sch_grp, ipn,
      state_id, local_id, last_name:begin_9, everything()
    )
} else {
  demos_stu <-
    demos_stu %>%
    select(
      year, system, system_name, school, school_name, ipn,
      state_id, local_id, last_name:begin_9, everything()
    )
}

demos_stu <- arrange(demos_stu, year, system_name, school_name, state_id)
