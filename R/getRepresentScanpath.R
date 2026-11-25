#' Function to find a representative scanpath by giving concave polygons (AOIs) on each cross section and the
#' clustering number of each cross section

#' @param  concave_hull output of the function getConcaveHulls()
#'
#' @param clusterings output of the function getClusteringLabelsForEachSection()
#' @param et numeric,distance of the cross sections in seconds.
#' @param  Fixation_singleStimulus data.frame with default NULL containing 6 column, including
#' (1) Participant, which is the name or id for the participants
#' (2) Fixation_Position_X_px
#' (3) Fixation_Position_Y_px
#' (4) Event_Start_Trial_Time_ms, start time of fixation in seconds
#' (5) Event_Ende_Trial_Time_ms, end time of the fixation in seconds
#' (6) Duration
#' Only needed by durationrec = TRUE, otherwise use the default value NULL.

#'
#' @param unionAOI logical. Use the union (TRUE) or intersection (FALSE) of the polygons
#' for each short-time AOI. Intersections without polygonal area are omitted.
#' @param durationrec logical, if to recalculate the duration of the fixation in representative scanpath using the
#' average duration of the fixations in the short time AOI
#' @return A list of two elements:
#'  -representScanpath: a data.frame for the representScanpath
#'  -AOIpolygons: List of elements of class 'SpatialPolygons' from R package "sp"


#' @export
#' @examples
#' data(scanpath_MIT1003_i1182314083)
#' concave_hull = getConcaveHulls(scanpath_MIT1003_i1182314083, 3, 0.04)
#' clusterings = getClusteringLabelsForEachSection(concave_hull, 0.2)
#' RepresentScanpath = getRepresentScanpath(concave_hull, clusterings, 0.04,
#'                                             scanpath_MIT1003_i1182314083)




getRepresentScanpath = function(concave_hull, clusterings, et,
                                Fixation_singleStimulus = NULL,
                                unionAOI = TRUE,
                                durationrec =FALSE){

  if (durationrec && is.null(Fixation_singleStimulus)) {
    stop("Fixation_singleStimulus is required when durationrec is TRUE")
  }
  if (durationrec) {
    points <- sf::st_as_sf(Fixation_singleStimulus,
      coords = c("Fixation_Position_X_px", "Fixation_Position_Y_px"),
      na.fail = FALSE
    )
  }
  geometries <- lapply(concave_hull, sectionGeometry)
  labels <- names(table(clusterings))
  labels <- labels[labels != "0"]
  combine <- if (unionAOI) sf::st_union else sf::st_intersection

  fixations <- data.frame(
    Fixation_Position_X_px = numeric(),
    Fixation_Position_Y_px = numeric(),
    Event_Start_Trial_Time_ms = numeric(),
    Event_Ende_Trial_Time_ms = numeric(),
    Duration = numeric()
  )
  polygons <- list()
  for (label in labels) {
    sections <- which(clusterings == label)
    duration <- length(sections) * et
    if (duration < 0.1) next

    parts <- Filter(Negate(is.null), geometries[sections])
    if (!length(parts)) next
    aoi <- Reduce(combine, parts)
    aoi <- aoi[!sf::st_is_empty(aoi)]
    if (!length(aoi)) next

    # A boundary-only intersection has no polygonal AOI to report.
    if (any(sf::st_geometry_type(aoi) == "GEOMETRYCOLLECTION")) {
      aoi <- sf::st_collection_extract(aoi, "POLYGON", warn = FALSE)
    }
    aoi <- aoi[sf::st_geometry_type(aoi) %in% c("POLYGON", "MULTIPOLYGON")]
    if (!length(aoi)) next

    polygon <- methods::as(aoi, "Spatial")
    center <- polygonCenter(polygon)
    if (durationrec) {
      inside <- lengths(sf::st_intersects(sf::st_geometry(points), aoi)) > 0L
      average <- mean(Fixation_singleStimulus$Duration[inside], na.rm = TRUE)
      if (!is.na(average)) duration <- average
    }

    fixations[nrow(fixations) + 1L, ] <- c(
      center, range(sections) * et, duration
    )
    polygons[[length(polygons) + 1L]] <- polygon
    rownames(fixations)[nrow(fixations)] <- label
  }

  list(representScanpath = fixations, AOIpolygons = polygons)
}
