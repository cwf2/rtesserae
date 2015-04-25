ingest.tweets.user <- function (file, quiet=F) {
  if (quiet != T) { 
    cat("Reading", file, "\n")
  }
  data <- paste(scan(file, what="character", encoding="UTF-8", sep="\n", quiet=T), collapse="\n")
  data <- unlist(strsplit(data, "\\n}\\n"))
  
  m <- regexpr("(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}) (\\d+) {\n(.+)", data, perl=T)

  valid <- m > -1
  if (is.null(valid) | length(which(valid)) < 1) { return(NULL) }
  
  cap_start <- attr(m, "capture.start", exact=T)[valid, 2:3]
  cap_stop <- cap_start + attr(m, "capture.length", exact=T)[valid, 2:3] - 1
  if (length(which(valid)) == 1) {
    cap_start <- matrix(cap_start, nrow=1)
    cap_stop <- matrix(cap_stop, nrow=1)
  }
  
  return(data.frame(
    unitid=substr(data[valid], start=cap_start[,1], stop=cap_stop[,1]),
    tweet=substr(data[valid], start=cap_start[,2], stop=cap_stop[,2])
  ))
}

ingest.tweets.corpus <- function(dir) {
  cat("Checking corpus", dir, "... ")
  
  files <- grep("^\\d+\\.dat$", dir(dir), value=T, perl=T)
  cat("found", length(files), "files\n")
  
  pb=txtProgressBar(min=0, max=length(files), style=3)
  
  corpus <- lapply(files, function(file) {
    tweets.this.user <- ingest.tweets.user(file.path(dir, file), quiet=T)
    if (is.null(tweets.this.user)) { return(NULL)}
    userid <- substr(file, 1, nchar(file)-4)
    tweets.this.user <- cbind(userid=rep(userid, times=nrow(tweets.this.user)), tweets.this.user)
    setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
    return(tweets.this.user)
  })
  
  close(pb)
  
  cat("Collating\n")
  corpus <- rbindlist(corpus)
  
  return(corpus)
}

# system.time(
#   tokens <- lapply(corpus$tweet, function(tweet) {
#     return(unlist(strsplit(tweet, "\\W+")))
#   })
# )
