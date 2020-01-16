if(hpn) {
  
  pwalk(
    .l = list(d = list(abs_sch, abs_pri, abs_dst, abs_sch_prd),
              n = c("sch", "pri", "dst", "sch_prd")),
    .f = function(d, n) write_rds(d, path = str_c("data/abs_", n, ".rds"))
  )
  
  pwalk(
    .l = list(d = list(dsc_sch, dsc_pri, dsc_dst, dsc_sch_prd),
              n = c("sch", "pri", "dst", "sch_prd")),
    .f = function(d, n) write_rds(d, path = str_c("data/dsc_", n, ".rds"))
  )
  
}

if(asd) {
  
  d <- mget(grep("^dsc", ls(), value = T))
  
  author <- function(obj, name, dir) {
    write_rds(x = obj, path = str_c("data/", dir, "/", name, ".rds"))
  }
  
  pwalk(
    .l = list(obj = d, name = names(d), dir = "asd"),
    .f = author
  )
  
}
