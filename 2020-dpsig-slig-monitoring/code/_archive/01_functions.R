sql_2_txt <-
  
  function(
    query_name,
    years = yrs,
    districts = unique(sch$system),
    directory = "C:/Users/CA20397/Documents/Local Projects/eis_queries/"
  ) {
    
    if(length(years) == 1) {
      y <- str_c("= ", years)
    } else {
      y <- str_c("in (", glue::glue_collapse(years - 1, sep = ", "), ")")
    }
    
    if(length(districts) == 1) {
      d <- str_c("= ", districts)
    } else {
      d <- str_c("in (", glue::glue_collapse(districts, sep = ", "), ")")
    }
    
    con <- file(paste0(directory, query_name, ".sql"), "rb")
    sql_string <- ""
    
    while(T) {
      line <- readLines(con, n = 1)
      if(length(line) == 0) {break}
      line <- gsub("\\r", "\\n", line)
      line <- gsub("yyy", y, line)
      line <- gsub("ddd", d, line)
      sql_string <- paste(sql_string, line, "\n")
    }
    
    close(con)
    return(sql_string)
    
  }
