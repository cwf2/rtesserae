source(file.path("R", "common.R"))

build.stem.cache <- function(file) {
  cat("Loading stems dictionary\n")
  stems <- read.csv(file, 
                    col.names=c("form", "pos", "stem"),
                    colClasses=c("character", "NULL", "character"),
                    header=F)
  stems <- data.table(stems)
  
  cat("Standardizing orthography\n")
  stems$form <- standardize(stems$form)
  stems$stem <- standardize(stems$stem)

  cat("Removing duplicates\n")
  setkey(stems, form, stem)
  stems <- unique(stems)
  stems <- na.omit(stems)
  stems <- stems[form != ""]
  
  return(stems)
}

stems <- build.stem.cache(file.path("data", "la.lexicon.csv"))

add.col.stem <- function(x) {
  ids <- which(x$type=="W")
  pb <- txtProgressBar(min=1, max=max(ids), style=3)
  
  do.call(rbind,
    lapply(ids, function(id) {
      setTxtProgressBar(pb, id)
      form.this <- x[id, form]
      
      do.call(rbind,
        lapply(stems[form==form.this, stem], function(stem) {
          data.frame(id=id, stem=stem, stringsAsFactors=F)
        })
      )
    })
  )
}

build.stem.cache2 <- function(stems) {
  forms <- unique(stems$form)
  
  estem <- new.env(hash=T, size=length(forms))
  
  
}




add.col.stem.two <- function(index.form) {
  
}