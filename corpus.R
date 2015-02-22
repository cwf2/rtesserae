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
  
  data.frame(
    token = tokens[not.empty],
    type = types[not.empty],
    unitid = rep(unitid, sum(not.empty)),
    phraseid = rep(phraseid, sum(not.empty))
  )
}

ingest.text <- function(file) {
  xml <- xmlParse(file=file)
  
  nnodes <- length(xpathApply(xml, "//TextUnit", function(x){1}))
  
  pb <- txtProgressBar(min=1, max=nnodes, style=3)
  phraseid = 0
  
  do.call(rbind, 
    xpathApply(xml, "//TextUnit", function(node){
      setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
      unitid <- as.integer(xmlGetAttr(node, "id"))
      textunit <- xmlValue(node)
      delimited <- gsub(tesre$pbound, paste("\\1", tesre$notseen, sep=""), textunit, perl=T)
      phrases <- unlist(strsplit(delimited, tesre$notseen))
      
      tokens.this.textunit <- do.call(rbind, 
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
}
