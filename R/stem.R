source(file.path("R", "common.R"))

build.stem.cache <- function(file) {
  cat("Loading stems dictionary\n")
  stems <- fread(file, header=F, select=c(1,3))
  setnames(stems, 1:2, c("form", "feat"))
  
  cat("Standardizing orthography\n")
  stems[,form:=standardize(form),]
  stems[,feat:=standardize(feat),]
  
  cat("Removing NAs, duplicates\n")
  setkey(stems, form, feat)
  stems <- unique(stems)
  stems <- na.omit(stems)
  stems <- stems[! ""]
  
  cat("Initializing hash\n")
  nstems <- length(unique(stems$feat))
  index <- new.env(hash=T, size=nstems)
  pb <- txtProgressBar(min=1, max=nstems, style=3)
  
  cat("Populating hash\n")
  foo <- function(form, feat) {
    assign(form, c(feat), envir=index)
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
  stems[, foo(form, feat), by=form]
  
  close(pb)
  return(index)
}

stems <- build.stem.cache(file.path("data", "la.lexicon.csv"))
