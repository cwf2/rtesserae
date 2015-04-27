library("XML")
library("stringi")
library("data.table")

standardize <- function(s) {
  s <- stri_trans_nfkd(s)
  s <- stri_trans_tolower(s)
  s <- gsub("[^[:alpha:]]+", "", s)
  s <- chartr("jv", "iu", s)
  return(s)
}

tesre <- new.env()
tesre$nonword <- "\\W+"
tesre$word <- "\\w+"
tesre$pbound <- "([:;\\.!\\?]+[”\"]?\\s*)"
tesre$notseen <- "::::"

parse.phrase <- function(text, tokenid, phraseid, unitid) {
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
  not.empty <- tokens != "" & ! is.na(tokens)
  
  data.table(
    display = tokens[not.empty],
    form = standardize(tokens[not.empty]),
    type = types[not.empty],
    tokenid = seq(from=tokenid, length.out=sum(not.empty)),
    unitid = rep(unitid, sum(not.empty)),
    phraseid = rep(phraseid, sum(not.empty))
  )
}

ingest.text <- function(file) {
  # create an environment (i.e. pseudo-object) 
  # to hold this text and its features
  text.object <- new.env()
  
  # store the original file name
  assign("file", file, envir=text.object)
  
  # read the xml
  cat("Reading", file, "\n")
  xml <- xmlParse(file=file)
  
  # get the loci
  loc <- xpathSApply(xml, "//TextUnit", function(node) {xmlGetAttr(node, "loc")})
  assign("loc", loc, envir=text.object)
  nnodes <- length(loc)
  
  # get the text
  pb <- txtProgressBar(min=1, max=nnodes, style=3)
  phraseid = 1
  tokenid = 1
  
  token.table <- rbindlist(
    xpathApply(xml, "//TextUnit", function(node){
      setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
      unitid <- as.integer(xmlGetAttr(node, "id")) + 1
      textunit <- xmlValue(node)
      delimited <- gsub(tesre$pbound, paste("\\1", tesre$notseen, sep=""), textunit, perl=T)
      phrases <- unlist(strsplit(delimited, tesre$notseen))
      
      tokens.this.textunit <- rbindlist( 
        lapply(phrases, function(p) {
          tokens.this.phrase <- parse.phrase(p, tokenid=tokenid, phraseid=phraseid, unitid=unitid)
          phraseid <<- phraseid + 1
          tokenid <<- tokenid + nrow(tokens.this.phrase)
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
  
  setkey(token.table, tokenid)
  
  assign("tokens", token.table, envir=text.object)
  return(text.object)
}

add.column <- function(text) {
  forms <- unique(text$tokens[type=="W", form])
  
  assign("index.form", new.env(hash=T, size=length(forms)), envir=text)
  
  f <- function(form, tokenid) {
    assign(form, c(tokenid), envir=text$index.form)
  }
  
  text$tokens[type=="W", f(form, tokenid), by=form, ]
  
  # calculate frequencies
  assign("freq.form", feature.frequencies(text$index.form), envir=text)
}

feat <- function(form, feature) {
  return(unlist(mget(x=form, envir=feature, ifnotfound=form)))
}
freq <- Vectorize(function(form, freq) {
  return(get(form, envir=freq))
}, vectorize.args="form")

add.col.feature <- function(text, feat.name, feat.dict) {
  forms <- ls(text$index.form)
  
  index.feature <- new.env(hash=T)
  assign(paste("index", feat.name, sep="."), index.feature, envir=text)
  
  cat("Stemming", length(forms), "forms\n")
  pb <- txtProgressBar(min=1, max=length(forms), style=3)
  
  for (form in forms) {
    features <- feat(form, feature=feat.dict)
    tokids.form <- get(form, text$index.form)
    
    for (feature in features) {
      tokids.feature <- unlist(mget(feature, envir=index.feature, mode="integer", ifnotfound=list(integer(0))))
      
      assign(feature, value=c(tokids.feature, tokids.form), envir=index.feature)
    }
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
  close(pb)
  
  cat("Removing duplicate entries\n")
  pb <- txtProgressBar(min=1, max=length(ls(index.feature)), style=3)
  
  for (feature in ls(index.feature)) {
    assign(feature, unique(get(feature, index.feature)), envir=index.feature)
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1) 
  }
  close(pb)
  
  # frequencies
  assign(paste("freq", feat.name, sep="."), feature.frequencies(index.feature), envir=text)
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

score <- function(s.tokenid, t.tokenid, s.text, t.text, pb=NA) {
  
  s.match <- data.table(tokenid=unique(s.tokenid))
  t.match <- data.table(tokenid=unique(t.tokenid))
  
  s.match[,freq:=freq(s.text$tokens[tokenid, form], s.text$freq.form)]
  t.match[,freq:=freq(t.text$tokens[tokenid, form], t.text$freq.form)]
  
  setkey(s.match, freq, tokenid)
  setkey(t.match, freq, tokenid)
  
  s.endpoints <- sort(s.match[1:2, tokenid])
  t.endpoints <- sort(t.match[1:2, tokenid])
  
  s.dist <- sum(s.text$tokens[tokenid >= s.endpoints[1] & tokenid <= s.endpoints[2], type]=="W")
  t.dist <- sum(t.text$tokens[tokenid >= t.endpoints[1] & tokenid <= t.endpoints[2], type]=="W")
  
  s.invfreq <- sum(s.match[,1/freq])
  t.invfreq <- sum(t.match[,1/freq])
  
  score <- log((s.invfreq + t.invfreq)/(s.dist + t.dist))
    
  if (class(pb) == "txtProgressBar") {
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
  }
  
  return(score)
}

tess.search <- function(s.text, t.text, feat.name="form") {
  cat("Source:", s.text$file, "\n")
  cat("Target:", t.text$file, "\n")
  cat("Feature:", feat.name, "\n")
  
  s.feat <- get(paste("index", feat.name, sep="."), envir=s.text)
  t.feat <- get(paste("index", feat.name, sep="."), envir=t.text)
  
  s.freq <- get(paste("freq", feat.name, sep="."), envir=s.text)
  t.freq <- get(paste("freq", feat.name, sep="."), envir=t.text)
  
  stoplist <- feature.stoplist(list(s.freq, t.freq))
  cat("Stoplist:", paste(stoplist, collapse=" "), "\n")
  
  features <- setdiff(intersect(ls(s.feat), ls(t.feat)), stoplist)
  
  cat("Generating links\n")
  pb <- txtProgressBar(min=1, max=length(features), style=3)
  result <- rbindlist(
    lapply(features, function(feature) {
      setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
      expand.grid(
        s.tokenid=get(feature, envir=s.feat), 
        t.tokenid=get(feature, envir=t.feat), 
        feature=feature
      )
    })
  )
  close(pb)
  result <- cbind(result,
    s.unitid=s.text$tokens[result$s.tokenid, unitid], 
    t.unitid=t.text$tokens[result$t.tokenid, unitid]
  )
    
  cat ("Checking minimal match criteria:\n")
  # for each pair of phrases, make sure at least two source tokens
  cat (" [1/3] at least 2 source tokens\n")
  setkey(result, s.unitid, t.unitid, s.tokenid)
  m <- result[! duplicated(result), .N, by=.(s.unitid, t.unitid)]
  m <- m[N>1, .(s.unitid, t.unitid)]
  result <- result[m]
  
  # make sure at least two target tokens
  cat (" [2/3] at least 2 target tokens\n")
  setkey(result, s.unitid, t.unitid, t.tokenid)
  m <- result[! duplicated(result), .N, by=.(s.unitid, t.unitid)]
  m <- m[N>1, .(s.unitid, t.unitid)]
  result <- result[m]
  
  # make sure at least 2 features
  cat (" [3/3] at least 2 features\n")
  setkey(result, s.unitid, t.unitid, feature)
  m <- result[! duplicated(result), .N, by=.(s.unitid, t.unitid)]
  m <- m[N>1, .(s.unitid, t.unitid)]
  result <- result[m]

  return(result)
}

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

