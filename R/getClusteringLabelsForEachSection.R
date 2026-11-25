
#' Function to clustering the cross sections and get clustering labels for each cross section

#' @param  convex_hull output of the function getConcaveHulls
#'
#' @param OR_p numeric between 0 and 1, threshold of the similarity of two polygons
#' @param compareFirstAndRest logic with default FALSE, whether to compare the the polygon on
#' first section with polygons on the rest sections within a short time AOI
#' @return A vector of integers indicating the clustering number of each section


#' @export
#' @examples
#' data(scanpath_MIT1003_i1182314083)
#' concave_hull = getConcaveHulls(scanpath_MIT1003_i1182314083, 3, 0.04)
#' clusterings = getClusteringLabelsForEachSection(concave_hull, 0.2)



getClusteringLabelsForEachSection = function(convex_hull,OR_p, compareFirstAndRest = FALSE){
  geometries <- lapply(convex_hull, sectionGeometry)
  n_plg <- length(geometries)
  clusterings <- integer(n_plg)
  if (n_plg < 2L) return(clusterings)

  similarities <- vapply(seq_len(n_plg - 1L), function(i) {
    geometryOverlap(geometries[[i]], geometries[[i + 1L]])
  }, numeric(1))
  overlapping <- which(similarities > 0)
  if (!length(overlapping)) return(clusterings)
  firstCluster <- overlapping[1L]
  clusterings[firstCluster] <- 1L

  clus = 1
  for (i in (firstCluster):(n_plg-1)) {
    if (is.null(geometries[[i + 1L]])) next
    if (similarities[i] < OR_p) clus <- clus + 1
    clusterings[i + 1L] <- clus
  }
  #---------------------------------------------------------------------


  #-------------------next step -----------------------------------------------
  # compare the similarity between fist section and the rest sections within the cluster,
  #
  # instead of neighbors
  if(compareFirstAndRest == TRUE){
    clusering_uniq = unique(clusterings)
    clusering_uniq = clusering_uniq[clusering_uniq!=0]

    for (i in seq(along = clusering_uniq)) {

      ind = which(clusterings == clusering_uniq[i])
      clusterBegin = min(ind)

      similarity2 = c()
      if(length(ind) > 2){
        for (j in 2:length(ind)) {
          similarity2[j-1] = geometryOverlap(geometries[[clusterBegin]], geometries[[ind[j]]])

        }
      }


      newclusterBegins = c()

      while (length(which(similarity2 < OR_p)) != 0) {
        xxx= min(which(similarity2 < OR_p)) + 1
        newclusterBegin = ind[xxx]
        newclusterBegins = c(newclusterBegins, newclusterBegin)
        ind = ind[-(1:(xxx-1))]

        similarity2 = c()
        if(length(ind) > 2){
          for (j in 2:length(ind)) {
            similarity2[j-1] = geometryOverlap(geometries[[newclusterBegin]], geometries[[ind[j]]])

          }
        }

      }

      #newBreaks = c(newBreaks,newclusterBegins)
      if(!is.null(newclusterBegins)){

        ind2 = which(clusterings == clusering_uniq[i])
        FirstSec = min(ind2)
        LastSec = max(ind2)

        breaks = c(FirstSec,newclusterBegins,LastSec)

        for (i in 1:(length(breaks)-1)) {
          clusterings[breaks[i]:breaks[i+1]] = breaks[i]*100
        }

      }

    }




    newClusters = unique(clusterings)[unique(clusterings) != 0]


    clusterings2 = clusterings
    for (i in 1:length(newClusters)) {
      clusterings2[clusterings == newClusters[i]] = i
    }






    return(clusterings2)
  }else{
    return(clusterings)
  }
}
