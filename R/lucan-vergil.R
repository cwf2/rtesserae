source(file.path("R", "corpus.R"))
source(file.path("R", "stem.R"))

s <- ingest.text("tesscorpus/vergil.aeneid.xml")
t <- ingest.text("tesscorpus/lucan.bellum_civile.xml")

add.column(s)
add.column(t)

add.col.feature(s, "stem", stems)
add.col.feature(t, "stem", stems)

result <- tess.search(s, t)

# check against benchmark
setkey(result, t.unitid, s.unitid)
result.loc <- result[,.(t.loc=t$loc[t.unitid], s.loc=s$loc[s.unitid]), by=.(t.unitid, s.unitid)][,.(t.loc, s.loc),]

bench <- fread("data/lucan-vergil.word.txt")
bench.loc <- bench[,.(t.loc=sub(".+\\s", "", TARGET_LOC), s.loc=sub(".+\\s", "", SOURCE_LOC), feat=SHARED),][,.(t.loc, s.loc),]
