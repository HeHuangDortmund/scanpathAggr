library(scanpathAggr)

data(scanpath_MIT1003_i1182314083)
data(scanpath_OSIE_1001)
inputs <- list(MIT1003 = scanpath_MIT1003_i1182314083, OSIE = scanpath_OSIE_1001)
reference <- readRDS("fixtures/rgeos-0.6-4.rds")
coordinateTolerance <- 1e-7
areaTolerance <- 1e-5

for (dataset in names(inputs)) {
  for (mode in c("default", "simplified")) {
    case <- paste(dataset, mode)
    expected <- reference[[case]]
    actual <- scanpathAggr(inputs[[dataset]], 3, simplify = mode == "simplified")
    xy <- c("Fixation_Position_X_px", "Fixation_Position_Y_px")
    timing <- c("Event_Start_Trial_Time_ms", "Event_Ende_Trial_Time_ms", "Duration")
    observed <- actual$representScanpath
    recorded <- expected$representScanpath

    stopifnot(
      identical(names(actual), names(expected)),
      identical(dimnames(observed), dimnames(recorded)),
      identical(observed[timing], recorded[timing]),
      max(abs(as.matrix(observed[xy]) - as.matrix(recorded[xy]))) < coordinateTolerance,
      length(actual$AOIpolygons) == nrow(recorded)
    )
    for (i in seq_along(actual$AOIpolygons)) {
      stopifnot(inherits(actual$AOIpolygons[[i]], "SpatialPolygons"))
      aoi <- sf::st_as_sfc(actual$AOIpolygons[[i]])
      baselineAOI <- sf::st_as_sfc(expected$AOIpolygons[[i]])
      difference <- sf::st_sym_difference(aoi, baselineAOI)
      stopifnot(sum(as.numeric(sf::st_area(difference))) < areaTolerance)
    }
  }
}

grDevices::pdf(NULL)
image <- system.file("stimulus/i1182314083.jpeg", package = "scanpathAggr")
mit <- scanpathAggr(inputs$MIT1003, 3)
stopifnot(inherits(ggplot2::ggplotGrob(scanpathVisualisation(
  inputs$MIT1003, mit, image
)), "gtable"))

square <- rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0))
sections <- list(list(square), list(square + 2), list(square + 4))
labels <- getClusteringLabelsForEachSection(sections, .25, compareFirstAndRest = TRUE)
stopifnot(identical(as.numeric(labels), c(1, 1, 2)))

fixations <- data.frame(
  Fixation_Position_X_px = c(4, 6, 15),
  Fixation_Position_Y_px = c(4, 6, 15),
  Duration = c(.2, .4, .8)
)
combined <- getRepresentScanpath(sections, rep(1, 3), .04, fixations,
                                unionAOI = FALSE, durationrec = TRUE)
stopifnot(
  isTRUE(all.equal(unname(unlist(combined$representScanpath[1, ])),
                   c(7, 7, .04, .12, .3))),
  inherits(combined$AOIpolygons[[1]], "SpatialPolygons")
)

missingAOIs <- list(NA, NA)
labels <- getClusteringLabelsForEachSection(missingAOIs, .2)
empty <- getRepresentScanpath(missingAOIs, labels, .04)
stopifnot(
  identical(labels, c(0L, 0L)),
  nrow(empty$representScanpath) == 0L,
  length(empty$AOIpolygons) == 0L,
  identical(representativeScanpathSimplify(empty, 50), empty),
  inherits(ggplot2::ggplotGrob(scanpathVisualisation(
    representScanpath = empty, stimulusImgPath = image
  )), "gtable")
)

invisible(grDevices::dev.off())
