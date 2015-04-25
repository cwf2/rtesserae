source(file.path("R", "corpus.R"))
source(file.path("R", "stem.R"))

s.text <- ingest.text("tesscorpus/vergil.aeneid.xml")
t.text <- ingest.text("tesscorpus/lucan.bellum_civile.xml")

s.word <- add.column(s.text)
t.word <- add.column(t.text)

s.stem <- add.col.feature(s.word, stems)
t.stem <- add.col.feature(t.word, stems)

tess.search <- function(s.feat, t.feat, s.word, t.word) {
  cat("Calculating feature frequencies\n")
  s.freq <- feature.frequencies(s.feat)
  t.freq <- feature.frequencies(t.feat)

  stoplist <- feature.stoplist(list(s.freq, t.freq))
  cat("Stoplist:", paste(stoplist, collapse=" "), "\n")
  
  cat("Generating links\n")
  result <- links(s.feat, t.feat, stoplist)
  result <- cbind(result,
    unit_s=s.text[result$s, unitid], 
    unit_t=t.text[result$t, unitid]
  )
  
  check.n.forms <- function(s, t) {
    forms <- unique(unlist(c(s.text[s, form], t.text[t, form])))
    length(forms)
  }
  
  cat ("Checking minimal match criteria:\n")
  # for each pair of phrases, make sure at least two source tokens
  cat (" [1/4] at least 2 source tokens\n")
  setkey(result, unit_s, unit_t, s)
  m <- result[! duplicated(result), .N, by=.(unit_s, unit_t)]
  m <- m[N>1, .(unit_s, unit_t)]
  result <- result[m]
  
  # make sure at least two target tokens
  cat (" [2/4] at least 2 target tokens\n")
  setkey(result, unit_s, unit_t, t)
  m <- result[! duplicated(result), .N, by=.(unit_s, unit_t)]
  m <- m[N>1, .(unit_s, unit_t)]
  result <- result[m]
  
  # make sure at least 2 features
  cat (" [3/4] at least 2 features\n")
  setkey(result, unit_s, unit_t, f)
  m <- result[! duplicated(result), .N, by=.(unit_s, unit_t)]
  m <- m[N>1, .(unit_s, unit_t)]
  result <- result[m]
  
  # make sure at least two differently inflected forms
#  cat (" [4/4] at least 2 forms\n")
#  result <- result[result[,check.n.forms(s,t), by=.(unit_s, unit_t)][V1>1], .(s, t, f, unit_s, unit_t)]
  
  return(result)
}


