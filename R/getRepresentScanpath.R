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
#' @param unionAOI logical with default TRUE, using the Union (TRUE) of intersection(FALSE) of the polygons as the shape of the short time AOIs
#' @param durationrec logical, if to recalculate the duration of the fixation in representative scanpath using the
#' average duration of the fixations in the short time AOI
#' @return A list of two elements:
#'  -representScanpath: a data.frame for the representScanpath
#'  -AOIpolygons: List of elements of class 'SpatialPolygons' from R package "sp"


#' @export
#' @import prevR sp
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

  #library(prevR)
  #library(sp)

  options(warn=-1)

  coordsToSpatialPolygons = function(x){
    if(is.na(x)){
      sps = NA
    }else{
      p = Polygon(x[[1]])
      ps = Polygons(list(p),1)
      sps = SpatialPolygons(list(ps))
    }
    return(sps)
  }
  SpatialPolygonsofHulls = lapply(concave_hull, coordsToSpatialPolygons)
  #table(clusterings)
  #labels = names(table(clusterings))[-1] # remove cluster 0
  labels = names(table(clusterings))[names(table(clusterings))!=0]

  if(unionAOI == FALSE){
    intersectPolygon_list = list()
    for (i in seq(along = labels)) {
      ind = which(clusterings == labels[i])
      #get intersections of the convex hulls with index 'ind'
      if(length(ind) == 1){
        intersectPolygon = SpatialPolygonsofHulls[[ind]]
      } else{
        intersectPolygon = SpatialPolygonsofHulls[[ind[1]]]
        for (j in ind[-1]) {
          intersectPolygon = gIntersection(intersectPolygon,
                                           SpatialPolygonsofHulls[[j]])

          #if(class(intersectPolygon) == "SpatialCollections"){
          if(is(overlapPolygon, "SpatialCollections")){
            # some times the results is not a SpatialPolygons
            #  e.g. collection of polygons and lines
            intersectPolygon = intersectPolygon@polyobj
          }

        }


      }
      intersectPolygon_list = c(intersectPolygon_list, intersectPolygon)
    }
  }else{
    overlapPolygon_list = list()
    for (i in seq(along = labels)) {
      ind = which(clusterings == labels[i])
      #get intersections of the convex hulls with index 'ind'
      if(length(ind) == 1){
        overlapPolygon = SpatialPolygonsofHulls[[ind]]
      } else{
        overlapPolygon = SpatialPolygonsofHulls[[ind[1]]]
        for (j in ind[-1]) {
          overlapPolygon = gUnion(overlapPolygon,
                                  SpatialPolygonsofHulls[[j]])

          #if(class(overlapPolygon) == "SpatialCollections"){
          if(is(overlapPolygon, "SpatialCollections")){
            # some times the results is not a SpatialPolygons
            #  e.g. collection of polygons and lines
            overlapPolygon = overlapPolygon@polyobj
          }

        }


      }
      overlapPolygon_list = c(overlapPolygon_list, overlapPolygon)

    }
  }


  #clusterCentroid = intersectPolygon_list[[1]]@polygons[[1]]@Polygons[[1]]@labpt
  #clusterDuration = table(clusterings)[-1]*et
  clusterDuration = table(clusterings)[names(table(clusterings))!=0]*et
  #length(intersectPolygon_list)

  tmpFun = function(x){

    #if(class(x) == "SpatialPoints"){
    if(is(x, "SpatialPoints")){

      if(!is.null(dim(x@coords))){
        return(colMeans(x@coords))
      }else{
        return(x@coords)
      }


    #}else if(class(x) == "SpatialLines"){
    }else if(is(x,"SpatialLines")){
      return(colMeans(x@lines[[1]]@Lines[[1]]@coords))
    }else{
      return(x@polygons[[1]]@Polygons[[1]]@labpt)
    }

  }

  if(unionAOI == FALSE){
    Fixation_Position = do.call(rbind,lapply(intersectPolygon_list, tmpFun))
  }else{
    Fixation_Position = do.call(rbind,lapply(overlapPolygon_list, tmpFun))
  }


  indLogi = clusterDuration >= 0.1  #  fixation should not be short as 0.1 second?


  start_end = do.call(rbind,lapply(seq(along = labels), function(i) range(which(clusterings == labels[i])*et)))

  if(length(labels) == 1 | sum(indLogi) == 1){
    representScanpath = c(Fixation_Position[indLogi,],start_end[indLogi,],clusterDuration[indLogi])
    representScanpath = data.frame(Fixation_Position_X_px = representScanpath[1],
                                   Fixation_Position_Y_px = representScanpath[2],
                                   Event_Start_Trial_Time_ms= representScanpath[3],
                                   Event_Ende_Trial_Time_ms= representScanpath[4],
                                   Duration = representScanpath[5])
  }else{
    representScanpath = cbind(Fixation_Position[indLogi,],start_end[indLogi,],clusterDuration[indLogi])
    representScanpath = as.data.frame(representScanpath)
    names(representScanpath) = c("Fixation_Position_X_px", "Fixation_Position_Y_px","Event_Start_Trial_Time_ms", "Event_Ende_Trial_Time_ms","Duration")
  }



  #return(list(representScanpath = representScanpath, AOIpolygons = intersectPolygon_list))
  #------------------------------------------------------------------------------------
  #
  #----------------------------------------------------------------------------------------------HHHHH

  if(durationrec == TRUE){
    ind = list()
    if(unionAOI == FALSE){
      for (i in 1:length(intersectPolygon_list)) {
        ind[[i]] = point.in.SpatialPolygons(Fixation_singleStimulus$Fixation_Position_X_px,
                                            Fixation_singleStimulus$Fixation_Position_Y_px,
                                            intersectPolygon_list[[i]])
      }
    }else{
      for (i in 1:length(overlapPolygon_list)) {
        ind[[i]] = point.in.SpatialPolygons(Fixation_singleStimulus$Fixation_Position_X_px,
                                            Fixation_singleStimulus$Fixation_Position_Y_px,
                                            overlapPolygon_list[[i]])
      }
    }



    fix_duration = c()

    for (i in 1:length(ind)) {
      fix_duration[i] = mean(Fixation_singleStimulus$Duration[ind[[i]]], na.rm = TRUE)
    }


    fix_duration = fix_duration[indLogi]

    tosub = !is.na(fix_duration)
    representScanpath$Duration[tosub] = fix_duration[tosub]
  }



  if(unionAOI == TRUE){
    AOIpolygons = overlapPolygon_list[indLogi]
  }else{
    AOIpolygons = intersectPolygon_list[indLogi]
  }

  #AOIpolygons = ifelse(unionAOI, overlapPolygon_list[indLogi], intersectPolygon_list[indLogi])
  return(list(representScanpath = representScanpath, AOIpolygons = AOIpolygons))
  #
}
