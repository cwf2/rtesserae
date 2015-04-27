source(file.path("R", "corpus.R"))

s <- ingest.text(file.path("tesscorpus", "vergil.aeneid.xml"))
t <- ingest.text(file.path("tesscorpus", "lucan.bellum_civile.xml"))

add.column(s)
add.column(t)

# exact-word search
result <- tess.search(s, t)

# get scores
setkey(result, t.unitid, s.unitid)
result.loc <- result[,.(t.loc=t$loc[t.unitid], s.loc=s$loc[s.unitid]), by=.(s.unitid, t.unitid)]
pb <- txtProgressBar(min=1, max=nrow(result.loc), style=3)
result.score <- result[,score(s.tokenid, t.tokenid, s, t, pb), by=.(s.unitid, t.unitid)]$V1
close(pb)

bench <- fread(file.path("data", "lucan-vergil.word.txt"))
bench <- bench[,.(t.loc=sub(".+\\s", "", TARGET_LOC), s.loc=sub(".+\\s", "", SOURCE_LOC), score=SCORE, feat=SHARED),]


# # stem search
# stems <- build.stem.cache(file.path("data", "la.lexicon.csv"))
#
# add.col.feature(s, "stem", stems)
# add.col.feature(t, "stem", stems)
# result <- tess.search(s, t, feat.name="stem")
# setkey(result, t.unitid, s.unitid)
# result.loc <- result[,.(t.loc=t$loc[t.unitid], s.loc=s$loc[s.unitid], feat=paste(feature, collapse=" ")), by=.(t.unitid, s.unitid)]
# 
# bench <- fread(file.path("data", "lucan-vergil.stem.txt"))
# bench.loc <- bench[,.(t.loc=sub(".+\\s", "", TARGET_LOC), s.loc=sub(".+\\s", "", SOURCE_LOC), feat=SHARED),]
# 
# result.key <- result.loc[,paste(t.loc, s.loc),]
# bench.key <- bench.loc[,paste(t.loc, s.loc),]
