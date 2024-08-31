
#' Function to clustering the cross sections and get clustering labels for each cross section

#' @param  convex_hull output of the function getConcaveHulls
#'
#' @param OR_p numeric between 0 and 1, threshold of the similarity of two polygons
#' @param compareFirstAndRest logic with default FALSE, whether to compare the the polygon on
#' first section with polygons on the rest sections within a short time AOI
#' @return A vector of integers indicating the clustering number of each section


#' @export
#' @import geometry rgeos sp methods
#' @examples
#' data(scanpath_MIT1003_i1182314083)
#' concave_hull = getConcaveHulls(scanpath_MIT1003_i1182314083, 3, 0.04)
#' clusterings = getClusteringLabelsForEachSection(concave_hull, 0.2)



getClusteringLabelsForEachSection = function(convex_hull,OR_p, compareFirstAndRest = FALSE){
  #library(geometry)
  #library(rgeos)
  gpcPoly = list()
  for (i in 1:length(convex_hull)) {
    if(any(is.na(convex_hull[[i]]))){
      gpcPoly[[i]] = NA
    } else{
      n_ch = length(convex_hull[[i]])
      if(n_ch == 1){
        tmp = as.data.frame(convex_hull[[i]])
        names(tmp) = c("x", "y")
        gpcPoly[[i]] = as(tmp, "gpc.poly")
      } else{
        chtmp = list()
        for (k in 1:n_ch) {
          chtmp[[k]] = as.data.frame(convex_hull[[i]][[k]])
          names(chtmp[[k]]) = c("x", "y")
          chtmp[[k]] = as(chtmp[[k]], "gpc.poly")
        }

        CHtmp = chtmp[[1]]
        for (h in 2:n_ch) {
          CHtmp = union(CHtmp, chtmp[[h]])
        }
        gpcPoly[[i]] = CHtmp

      }
    }
  }


  n_plg = length(gpcPoly)

  ## 2.1 similarity of each 2 neighbors
  similarities = c()
  NAorNOt = is.na(gpcPoly)
  for (i in 1:(n_plg-1)) {
    #if(is.na(gpcPoly[[i]]) | is.na(gpcPoly[[i+1]])){
    if(NAorNOt[i] | NAorNOt[i+1]){
      similarities[i] = 0
    }else{
      area_union = area.poly(union(gpcPoly[[i]], gpcPoly[[i+1]]))
      area_intersec = area.poly(intersect(gpcPoly[[i]], gpcPoly[[i+1]]))
      similarity = area_intersec/area_union
      similarities[i] = similarity
    }
  }



  #------------------------rewrite--------------------------------------------
  clusterings = c()
  firstCluster = min(which(similarities > 0))
  clusterings[firstCluster] = 1
  #clusterings[firstCluster+1] = 1
  if(firstCluster > 1){
    clusterings[1:(firstCluster-1)] = 0
  }


  clus = 1
  for (i in (firstCluster):(n_plg-1)) {
    if(similarities[i]>= OR_p){
      clusterings[i+1] = clus
    }else if(similarities[i] == 0 & is.na(gpcPoly[[i+1]])){
      clusterings[i+1] = 0
    }else if(similarities[i] < OR_p & !is.na(gpcPoly[[i+1]])){
      clus = clus + 1
      clusterings[i+1] = clus
    }
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
          area_union = area.poly(union(gpcPoly[[clusterBegin]], gpcPoly[[ind[j]]]))
          area_intersec = area.poly(intersect(gpcPoly[[clusterBegin]], gpcPoly[[ind[j]]]))
          similarity2[j-1] = area_intersec/area_union

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
            area_union = area.poly(union(gpcPoly[[newclusterBegin]], gpcPoly[[ind[j]]]))
            area_intersec = area.poly(intersect(gpcPoly[[newclusterBegin]], gpcPoly[[ind[j]]]))
            similarity2[j-1] = area_intersec/area_union

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
