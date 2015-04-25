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
    form = standardize(tokens[not.empty]),
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
  wtok <- data.frame(id=wtok, tok=index[wtok, form], stringsAsFactors=F)
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

feat <- function(form, feature) {
  return(unlist(mget(x=form, envir=feature, ifnotfound=form)))
}

add.col.feature <- function(index.token, dict.feature) {
  forms <- ls(index.token)
  index.feature <- new.env(hash=T, size=length(forms))
  
  cat("Stemming", length(forms), "forms\n")
  pb <- txtProgressBar(min=1, max=length(forms), style=3)
  
  for (f in forms) {
    indexable <- feat(f, feature=dict.feature)
    tokids.form <- get(f, index.token)
    
    for (i in indexable) {
      tokids.feature <- unlist(mget(i, envir=index.feature, mode="integer", ifnotfound=list(integer(0))))
      
      assign(i, value=c(tokids.feature, tokids.form), envir=index.feature)
    }
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
  close(pb)
  
  cat("Removing duplicate entries\n")
  pb <- txtProgressBar(min=1, max=length(ls(index.feature)), style=3)
  
  for (i in ls(index.feature)) {
    assign(i, value=unique(get(i, index.feature)), envir=index.feature)
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1) 
  }
  close(pb)
  
  return(index.feature)
}

links <- function(s, t, stoplist=character(0)) {
  features <- setdiff(intersect(ls(s), ls(t)), stoplist)
  pb <- txtProgressBar(min=1, max=length(features), style=3)
  
  links <- rbindlist(
    lapply(features, function(f) {
      setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
      expand.grid(s=get(f, envir=s), t=get(f, envir=t), f=f)
    })
  )
  
  close(pb)
  return(links)
}

feature.frequencies <- function(index.feature) {
  features <- ls(index.feature)
  freq <- new.env(hash=T, size=length(features))
  total <- 0
  
  cat ("Calculating", length(features), "feature tallies\n")
  pb <- txtProgressBar(min=1, max=length(features), style=3)
  for (f in features) {
    tally <- length(get(f, envir=index.feature))
    assign(f, tally, envir=freq)
    total <- total + tally
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
  close(pb)
  
  cat ("Converting to frequencies\n")
  pb <- txtProgressBar(min=1, max=length(features), style=3)
  for (f in features) {
    assign(f, get(f, envir=freq)/total, envir=freq)
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
  close(pb)

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

