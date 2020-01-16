sch <- get_schools()

# if(asd) {
#   sch <-
#     get_schools() %>%
#     filter(system == 985) %>%
#     left_join(
#       read_csv(
#         paste0("N:/ORP_accountability/projects/Josh/school_lists/",
#                "asd_operator_assignments_180605.csv")
#       )[c("school", "operator")],
#       by = "school"
#     ) %>%
#     select(system, system_name, operator, school, school_name) %>%
#     arrange(school_name)
# }
# 
# if(hpn) {
#   sch <-
#     get_schools() %>%
#     filter(system == 330, school %in% c(21, 55, 194, 200, 245)) %>%
#     mutate(sch_grp = "Partnership Network") %>%
#     arrange(school_name)
# }
# 
# sch_pri <- get_file(2018, "priority")
# 
# sch <-
#   sch %>%
#   bind_rows(
#     sch_pri %>%
#       left_join(
#         sch,
#         by = c("system", "school"),
#         suffix = c("", "_y")
#       ) %>%
#       filter(is.na(school_name_y)) %>%
#       mutate(sch_grp = "TN Priority Schools") %>%
#       select(system:school_name, sch_grp)
#   )
