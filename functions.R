# lo tengo de aquí:
# https://stackoverflow.com/questions/44853322/how-to-read-the-contents-of-an-sql-file-into-an-r-script-to-run-a-query

getSQL <- function(filepath){
  con = file(filepath, "r")
  sql.string <- ""

  while (TRUE){
    line <- readLines(con, n = 1)

    if ( length(line) == 0 ){
      break
    }

    line <- gsub("\\t", " ", line)

    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }

    sql.string <- paste(sql.string, line)
  }

  close(con)
  return(sql.string)
}


count_pct <- function(df) {
  return(
    df %>%
      tally %>% 
      mutate(n_pct = 100*n/sum(n))
  )
}
