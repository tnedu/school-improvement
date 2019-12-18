# cal ----

# Query school calendars from EIS, fill in missing calendar dates (e.g.,
# weekends, summer start dates), and identify 20-day periods.

cal <-
  sch %>%
  left_join(
    # 2019-03-12 changed sql_to_text to sql_2_txt
    dbGetQuery(con_eis, sql_2_txt("calendar", years = yrs)) %>%
      rename_all(tolower) %>% select(-system_name, -school_name),
    by = c("system", "school")
  ) %>%
  group_by(year, system, school, ipn) %>%
  mutate(id_num = rank(id_date)) %>%
  ungroup() %>%
  select(year, everything())

# Confirm that every school has exactly one IPN.
all_ipn <-
  cal %>%
  group_by(year, system, school) %>%
  summarize(n_ipn = n_distinct(ipn)) %>%
  ungroup()
  # extract2("n_ipn") %>%
  # max()

# stopifnot(all_ipn == 1)
rm(all_ipn)

cal_all <- tibble()

# Tabulate all dates within the range of dates in cal for a given year.
# Insert a dummy school for tidyr::complete below.

for(y in yrs) {
  
  d <- min(cal$system[cal$year == y])
  s <- min(cal$school[cal$year == y & cal$system == d])
  
  tmp <-
    tibble(
      year = y,
      system = d,
      school = s,
      id_date =
        seq(as.Date(min(cal$id_date[cal$year == y], na.rm = T)),
            as.Date(max(cal$id_date[cal$year == y], na.rm = T)),
            by = "days")
    )
  
  cal_all <- bind_rows(cal_all, tmp)
  
}

# Join cal and cal_all, introducing missing values for id_num, then plug in
# ID numbers where missing (e.g., for weekends).

plug <- function(df) {
  nr <- nrow(df)
  for(i in 1:nr) {
    cond_1 <- is.na(df$id_num[i])
    cond_2 <- mean(is.na(df$id_num[(i + 1):nr])) == 1
    if(cond_1 & cond_2) {
      df$id_num[i] <- max(df$id_num[1:(i - 1)], na.rm = T)
    } else if(cond_1 & !cond_2) {
      df$id_num[i] <- min(df$id_num[(i + 1):nr], na.rm = T) - 1
    }
  }
  return(df)
}

cal <-
  cal %>%
  mutate(id_date = as_date(id_date)) %>%
  full_join(
    cal_all %>% select(year, id_date),
    by = c("year", "id_date")
  )

# if(asd) {
#   cal <-
#     cal %>%
#     complete(
#       nesting(system, system_name, operator, school, school_name, sch_grp, ipn),
#       nesting(year, id_date)
#     )
# }
# 
# if(hpn) {
#   cal <-
#     cal %>%
#     complete(
#       nesting(system, system_name, school, school_name, sch_grp, ipn),
#       nesting(year, id_date)
#     )
# }

cal <-
  cal %>%
  complete(
    nesting(system, system_name, school, school_name, ipn),
    nesting(year, id_date)
  )

cal <-
  cal %>%
  filter(!is.na(system)) %>%
  group_by_at(vars(system:year)) %>%
  nest() %>%
  mutate(data = map(data, plug)) %>%
  unnest() %>%
  filter(!id_num %in% c(-Inf, Inf)) %>%
  mutate(
    id_date = as_date(id_date),
    period = ceiling(id_num / 20)
  ) %>%
  mutate_at(vars(id_num, period), funs(if_else(. == 0, 1, .)))

cal <-
  cal %>%
  select(
    year, system, system_name, school, school_name, ipn,
    everything()
  ) %>%
  arrange(year, system_name, school_name, id_date)

cal <- inner_join(cal, schools_csi_2019 %>% select(system, school))

rm(cal_all, d, s, y)

# cal_now ----

# Determine each school's instructional day threshold (i.e., the instructional
# day number beyond which we will disregard absences or discipline actions from
# past years).

cal_now <-
  cal %>%
  filter(year == max(year, na.rm = T), id_date <= now()) %>%
  group_by(system, school) %>%
  filter(id_date == max(id_date)) %>%
  ungroup() %>%
  transmute(
    system, school,
    now_num = id_num,
    now_prd = ceiling(now_num / 20)
  ) %>%
  arrange(system, school)

# cal_prd ----

# Count instructional days by period, up to the current date.

cal_prd <-
  cal %>%
  left_join(cal_now, by = c("system", "school"))

cal_prd <-
  cal_prd %>%
  mutate(ytd = 0) %>%
  bind_rows(cal_prd %>% filter(id_num <= now_num) %>% mutate(ytd = 1)) %>%
  filter(year < max(year) | ytd == 1) %>%
  group_by(ytd, year, system, school, period) %>%
  summarize(days_prd = n_distinct(id_num)) %>%
  ungroup()

# Rectify missing values of id_num, one school at a time.

# for(d2 in unique(tmp_2$system)) {
#   for(s2 in unique(tmp_2$school)) {
#     
#     tmp_3 <- tmp_2 %>% filter(system == d2, school == s2)
#     nr <- nrow(tmp_3)
#     
#     for(i in 1:nr) {
#       cond_1 <- is.na(tmp_3$id_num[i])
#       cond_2 <- min(tmp_3$id_num[(i + 1):nr], na.rm = T) == Inf
#       if(cond_1 & cond_2) {
#         tmp_3$id_num[i] <- max(tmp_3$id_num[1:(i - 1)], na.rm = T)
#       } else if(cond_1 & !cond_2) {
#         tmp_3$id_num[i] <- min(tmp_3$id_num[(i + 1):nr], na.rm = T) - 1
#       }
#     }
# 
#     tmp <- bind_rows(tmp, tmp_3)
#     
#   }
# }
# 
# rm(cond_1, cond_2, d, d2, i, nr, s, s2, y)
