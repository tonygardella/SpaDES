x = list(
  name="randomLandscapes",
  description="Generate RasterStack of random maps representative of a forest landscape (DEM, forestAge, forestCover, habitatQuality, percentPine). Requires a global simulation parameter `stackName` be set.",
  keywords=c("random map", "random landscape"),
  authors=c(person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre"))),
  version=numeric_version("1.0.0"),
  spatialExtent=NA,
  timeframe=as.POSIXlt(c(NA, NA)),
  timestepUnit=NA_real_,
  citation=list(),
  reqdPkgs=list("raster", "RColorBrewer", "tkrplot", "RandomFields"),
  parameters=rbind(
    defineParameter("stackName", "character", "randomLandscape"),
    defineParameter("nx", "numeric", 100L, NA, NA),
    defineParameter("ny", "numeric", 100L, NA, NA),
    defineParameter("inRAM", "logical", FALSE, NA, NA),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA),
    defineParameter(".plotInterval", "numeric", 1, NA, NA),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA)),
  inputObjects=data.frame(objectName=character(),
                          objectClass=character(),
                          other=character(), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName="landscape",
                           objectClass="RasterStack",
                           other=NA_character_, stringsAsFactors=FALSE)
)

cat(as.yaml(x), file="_ignore/randomLandscapes.yml")
