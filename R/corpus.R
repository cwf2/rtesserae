source(file.path("R", "common.R"))
library("XML")

tesre <- new.env()
tesre$nonword <- "\\W+"
tesre$word <- "\\w+"
tesre$pbound <- "([:;\\.!\\?]+[”\"]?\\s*)"
tesre$notseen <- "::::"

parse.phrase <- function(text, phraseid, unitid) {
  wtokens <- unlist(strsplit(x=text, split=tesre$nonword))
  ptokens <- unlist(strsplit(x=text, split=tesre$word))
  if (wtokens[1] == "") {
    wtokens <- wtokens[-1]
  }
  finalp <- ""
  if (length(wtokens) < length(ptokens)) {
    finalp <- ptokens[length(ptokens)]
    ptokens <- ptokens[-length(ptokens)]
  }
  
  tokens <- c(rbind(ptokens, wtokens), finalp)
  types <- c(rep(c("P", "W"), length(wtokens)), "P")
  not.empty <- tokens != ""
  
  data.table(
    display = tokens[not.empty],
    type = types[not.empty],
    unitid = rep(unitid, sum(not.empty)),
    phraseid = rep(phraseid, sum(not.empty))
  )
}

ingest.text <- function(file) {
  xml <- xmlParse(file=file)
  
  nnodes <- length(xpathApply(xml, "//TextUnit", function(x){1}))
  
  cat("Reading", file, "\n")
  pb <- txtProgressBar(min=1, max=nnodes, style=3)
  phraseid = 1
  
  token.table <- rbindlist(
    xpathApply(xml, "//TextUnit", function(node){
      setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
      unitid <- as.integer(xmlGetAttr(node, "id")) + 1
      textunit <- xmlValue(node)
      delimited <- gsub(tesre$pbound, paste("\\1", tesre$notseen, sep=""), textunit, perl=T)
      phrases <- unlist(strsplit(delimited, tesre$notseen))
      
      tokens.this.textunit <- rbindlist( 
        lapply(phrases, function(p) {
          tokens.this.phrase <- parse.phrase(p, phraseid=phraseid, unitid=unitid)
          phraseid <<- phraseid + 1
          return(tokens.this.phrase)
        })
      )
      
      if (! grepl(paste(tesre$notseen, "$", sep=""), delimited, perl=T)) {
        phraseid <<- phraseid - 1
      }
      
      return(tokens.this.textunit)
    })
  )
  close(pb)
  
  token.table$type = as.factor(token.table$type)
  
  setkey(token.table, unitid, phraseid)
  return(token.table)
}

add.column <- function(index) {
  wtok <- which(index$type=="W")
  wtok <- data.frame(id=wtok, tok=standardize(index[wtok, display]), stringsAsFactors=F)
  wtok <- wtok[grepl("[a-z]", wtok$tok),]
  utok <- unique(wtok$tok)
  hash <- new.env(hash=T, size=length(utok))
  for (tok in utok) { assign(tok, integer(0), envir=hash)}
  
  append.tok.id <- Vectorize(function(id, tok) {
    ids <- get(tok, envir=hash)
    ids <- c(ids, id)
    assign(tok, ids, envir=hash)
  })
  
  do.call(append.tok.id, wtok)
  return(hash)
}

links <- function(s, t) {
  rbindlist(
    lapply(intersect(ls(s), ls(t)), function(tok) {
      expand.grid(s=get(tok, envir=s), t=get(tok, envir=t), tok=tok)
    })
  )
}

feature.frequencies <- function(feature) {
  freq <- new.env(hash=T, size=length(ls(feature)))
  total <- 0
  
  for (f in ls(feature)) {
    tally <- length(get(f, envir=feature))
    assign(f, tally, envir=freq)
    total <- total + tally
  }
  for (f in ls(feature)) {
    assign(f, get(f, envir=freq)/total, envir=freq)
  }

  assign(".__TOTAL__", total, envir=freq)
  
  return(freq)
}

feature.stoplist <- function(freq.list, n=10) {
  
  feat.uniq <- unique(unlist(lapply(freq.list, ls)))
  mean.freq <- new.env(hash=T, size=length(feat.uniq))
  for (f in feat.uniq) {
    assign(f, 0, envir=mean.freq)
  }
  
  for (freq in freq.list) {
    for (f in ls(freq)) {
      freq.working <- get(f, envir=mean.freq)
      freq.this <- get(f, envir=freq)
      assign(f, freq.working + freq.this/length(freq.list), envir=mean.freq)
    }
  }
  
  sorted <- sort(sapply(ls(mean.freq), get, envir=mean.freq), decreasing=T)
  
  stoplist <- character(0)
  
  if (n>0) {
    n = min(n, length(sorted))
    stoplist <- names(sorted[1:n])
  } 
  
  return(stoplist)
}

mytest <- function() {
  s.text <- ingest.text("tesscorpus/vergil.aeneid.xml")
  t.text <- ingest.text("tesscorpus/lucan.bellum_civile.xml")
  
  s.word <- add.column(s.text)
  t.word <- add.column(t.text)
  
  s.freq <- feature.frequencies(s.word)
  t.freq <- feature.frequencies(t.word)
  
  stoplist <- feature.stoplist(list(s.freq, t.freq))
  
  rm(list=intersect(stoplist, ls(s.word)), envir=s.word)
  rm(list=intersect(stoplist, ls(t.word)), envir=t.word)

  result <- links(s.word, t.word)
  result <- cbind(result,unit_s=s.text[result$s, unitid], unit_t=t.text[result$t, unitid])
  setkey(result, tok, unit_s, unit_t)
  
  dup <- duplicated(result)
  m <- result[! dup, list(unit_s, unit_t)]
  m <- table(m)
  m <- as.data.table(m)
  m <- m[N>1,]
}
