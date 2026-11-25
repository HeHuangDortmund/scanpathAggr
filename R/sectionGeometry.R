# Scanpath coordinates are planar image pixels with no coordinate reference system.
sectionGeometry <- function(hulls) {
  if (!is.list(hulls) || !length(hulls) || anyNA(hulls)) return(NULL)

  polygons <- lapply(hulls, function(x) {
    if (!all(is.finite(x)) || nrow(unique(x)) < 3L) return(NULL)
    if (!all(x[1L, ] == x[nrow(x), ])) x <- rbind(x, x[1L, ])
    polygon <- sf::st_sfc(sf::st_polygon(list(x)))
    if (as.numeric(sf::st_area(polygon)) == 0) return(NULL)
    polygon
  })
  polygons <- Filter(Negate(is.null), polygons)
  if (!length(polygons)) return(NULL)
  Reduce(sf::st_union, polygons)
}

geometryOverlap <- function(x, y) {
  if (is.null(x) || is.null(y)) return(0)
  unionArea <- sum(as.numeric(sf::st_area(sf::st_union(x, y))))
  if (unionArea == 0) return(0)
  sum(as.numeric(sf::st_area(sf::st_intersection(x, y)))) / unionArea
}
