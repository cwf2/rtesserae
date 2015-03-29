library("stringi")
library("data.table")

standardize <- function(s) {
  s <- stri_trans_nfkd(s)
  s <- stri_trans_tolower(s)
  s <- gsub("[^[:alpha:]]+", "", s)
  s <- chartr("jv", "iu", s)
  return(s)
}
