# Functions ----

strip_attributes <- function(x, df = T) {
  if(df) attributes(x)$spec <- NULL
  if(!df) {
    attributes(x)$label <- NULL
    attributes(x)$format.stata <- NULL
  }
  return(x)
}

# Input ----

retention <-
  list(
    y19 = "N:/Data Mgmt and Reporting/DU_Data/Educators/Educator_Retention/Final_Cleaned/retention_1718_1819.dta",
    y18 = "N:/Data Mgmt and Reporting/DU_Data/Educators/Educator_Retention/Final_Cleaned/retention_1617_1718.dta",
    y17 = "N:/Data Mgmt and Reporting/DU_Data/Educators/Educator_Retention/Final_Cleaned/retention_1516_1617.dta"
  ) %>%
  map(read_dta) %>%
  map(~ mutate_all(.x, strip_attributes, df = F))

# staff_tvaas <-
#   list(
#     staff = list(
#       sy2019 = "data/staff-assignments-tncompass-2019.csv",
#       sy2018 = "data/staff-assignments-tncompass-2018.csv",
#       sy2017 = "data/staff-assignments-tncompass-2017.csv"
#     ),
#     tvaas = list(
#       sy2019 = "N:/ORP_accountability/data/2019_tvaas/SAS-TDOE-Teacher-VA-by-Subject.xlsx",
#       sy2018 = "N:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2017-18/SAS-TDOE-Teacher-VA-by-Subject.xlsx",
#       sy2017 = "N:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2016-17/Updated Teacher Subject Level Data 2016-17.xlsx"
#     )
#   ) %>%
#   map_at(
#     "staff",
#     ~ .x %>%
#       map(
#         .f = function(filename) {
#           read_csv(str_c(directory_master, filename)) %>%
#             janitor::clean_names() %>%
#             strip_attributes()
#         }
#       )
#   ) %>%
#   map_at(
#     "tvaas",
#     ~ .x %>%
#       map(read.xlsx) %>%
#       map(janitor::clean_names)
#   )

# teacher_eis <-
#   dbGetQuery(
#     connection_eis,
#     read_file("code/teacher.sql")
#   ) %>%
#   rename_all(tolower) %>%
#   inner_join(
#     schools_csi %>% select(district, school),
#     by = c("district", "school")
#   ) %>%
#   mutate_at(vars(ends_with("date")), as_date)

# tvaas_teacher <-
#   list(
#     sy2019 = "N:/ORP_accountability/data/2019_tvaas/SAS-TDOE-Teacher-VA-by-Subject.xlsx",
#     sy2018 = "N:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2017-18/SAS-TDOE-Teacher-VA-by-Subject.xlsx",
#     sy2017 = "N:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2016-17/Updated Teacher Subject Level Data 2016-17.xlsx"
#   ) %>%
#   map(
#     ~ .x %>%
#       read.xlsx() %>%
#       janitor::clean_names()
#   )

# Explore input ----

map(retention, ~ n_distinct(.x$final_district, .x$final_school))

map(retention, ~ count(.x, final_role))

# Estimate teacher turnover based on EIS teacher assignments ----

# turnover_eis <-
#   map(
#     2020:2018,
#     ~ teacher_eis %>%
#       filter(
#         school_year %in% c(.x, .x - 1),
#         ta_end_date - ta_begin_date >= 20
#       ) %>%
#       group_by(school_year) %>%
#       nest() %>%
#       arrange(desc(school_year)) %>%
#       extract2("data") %>%
#       map(function(df) transmute(df, district, school, person_id, present = T)) %>%
#       reduce(
#         left_join,
#         by = c("district", "school", "person_id"),
#         suffix = c("", "_last_year")
#       ) %>%
#       group_by(district, school) %>%
#       summarize(
#         n_teachers_retained = n_distinct(person_id[present & present_last_year]),
#         n_teachers_total = n_distinct(person_id)
#       ) %>%
#       ungroup()
#       # mutate(
#       #   pct_teachers_retained
#       # )
#   ) %>%
#   set_names(nm = str_c("sy", 2020:2018))

# Clean teacher turnover data for CSI schools ----

turnover_school <-
  retention %>%
  imap(
    ~ .x %>%
      filter(final_role == "teacher") %>%
      mutate(year = as.numeric(str_replace(.y, "y", ""))) %>%
      transmute(
        district = final_district,
        school = final_school,
        period = str_c(
          "20", year - 2, "-", year - 1,
          " to 20", year - 1, "-", year
        ),
        total,
        # retained,
        pct_exited = 100 - pct_retained,
        loe4,
        # retained_loe4,
        # The existing calculation uses total teachers for the denominator, but
        # total teachers who earned LOE 4+ is more appropriate for our purposes
        # here.
        pct_exited_loe4 = 100 - 100 * retained_loe4 / loe4
      ) %>%
      group_by(district, school, period) %>%
      nest(.key = "n") %>%
      mutate(
        pct = map(
          n,
          ~ .x %>%
            select(starts_with("pct")) %>%
            gather(type, pct) %>%
            rename(pct_exited = pct)
        ),
        n = map(
          n,
          ~ .x %>%
            select(total, loe4) %>%
            gather(type, denom)
        )
      ) %>%
      unnest() %>%
      filter(denom >= 5) %>%
      select(district, school, period, type, denom, pct_exited) %>%
      group_by(period, type) %>%
      mutate(
        # quintile = ntile(pct_exited, 5)
        quintile = if_else(
          pct_exited == 0,
          1,
          ceiling(percent_rank(pct_exited) * 5)
        )
      ) %>%
      ungroup() %>%
      inner_join(
        schools_csi %>% select(district, school),
        by = c("district", "school")
      )
  ) %>%
  reduce(bind_rows) %>%
  arrange(district, school, period)

# Output ----

write_csv(turnover_school, "data/turnover-school.csv")
