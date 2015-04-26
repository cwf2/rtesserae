source(file.path("R", "corpus.R"))
source(file.path("R", "stem.R"))

s <- ingest.text(file.path("tesscorpus", "vergil.aeneid.xml"))
t <- ingest.text(file.path("tesscorpus", "lucan.bellum_civile.xml"))

add.column(s)
add.column(t)

add.col.feature(s, "stem", stems)
add.col.feature(t, "stem", stems)

# exact-word search
result <- tess.search(s, t)

# check against benchmark
setkey(result, t.unitid, s.unitid)
result.loc <- result[,.(t.loc=t$loc[t.unitid], s.loc=s$loc[s.unitid]), by=.(t.unitid, s.unitid)][,.(t.loc, s.loc),]

bench <- fread(file.path("data", "lucan-vergil.word.txt"))
bench.loc <- bench[,.(t.loc=sub(".+\\s", "", TARGET_LOC), s.loc=sub(".+\\s", "", SOURCE_LOC), feat=SHARED),][,.(t.loc, s.loc),]

identical(result.loc, bench.loc)


# stem search
result <- tess.search(s, t, feat.name="stem")
setkey(result, t.unitid, s.unitid)
result.loc <- result[,.(t.loc=t$loc[t.unitid], s.loc=s$loc[s.unitid], feat=paste(feature, collapse=" ")), by=.(t.unitid, s.unitid)]

bench <- fread(file.path("data", "lucan-vergil.stem.txt"))
bench.loc <- bench[,.(t.loc=sub(".+\\s", "", TARGET_LOC), s.loc=sub(".+\\s", "", SOURCE_LOC), feat=SHARED),]

result.key <- result.loc[,paste(t.loc, s.loc),]
bench.key <- bench.loc[,paste(t.loc, s.loc),]
