app <- ShinyDriver$new("../../", loadTimeout = 1e+05)
app$snapshotInit("test1")

app$setInputs(rmapdb = "Samples")
app$snapshot()
