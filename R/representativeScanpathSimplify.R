#' @param  RepresentScanpath output of the function getReprensentativeScanpath()
#'
#' @param alpha numeric, threshold, saccade vector shorter as alpha will be merged

#' @return A list of two elements:
#'  -representScanpath: a data.frame for the representScanpath
#'  -AOIpolygons: List of 10 of class 'SpatialPolygons' from R package "sp"


#' @export
#' @import sp rgeos concaveman stats


representativeScanpathSimplify = function(RepresentScanpath, alpha){

  cords = RepresentScanpath$representScanpath[,c("Fixation_Position_X_px","Fixation_Position_Y_px")]



  n = dim(cords)[1] - 1
  saccadlength = c()
  for (i in 1:n) {
    saccadlength[i] = dist(cords[c(i,i+1),])
  }

  indshort = which(saccadlength < alpha)


  while (length(indshort)!=0) {
    RepresentScanpath = merge1st(RepresentScanpath, alpha)

    cords = RepresentScanpath$representScanpath[,c("Fixation_Position_X_px","Fixation_Position_Y_px")]
    n = dim(cords)[1] - 1
    saccadlength = c()
    for (i in 1:n) {
      saccadlength[i] = dist(cords[c(i,i+1),])
    }

    indshort = which(saccadlength < alpha)
  }

  return(RepresentScanpath)
}

coordsToSpatialPolygons = function(x){

  p = Polygon(x)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))

  return(sps)
}


polygonCenter = function(x){

  if(is(x,"SpatialPoints")){

    if(!is.null(dim(x@coords))){
      return(colMeans(x@coords))
    }else{
      return(x@coords)
    }


  }else if(is(x, "SpatialLines")){
    return(colMeans(x@lines[[1]]@Lines[[1]]@coords))
  }else{
    return(x@polygons[[1]]@Polygons[[1]]@labpt)
  }

}


merge1st = function(RepresentScanpath, alpha){


  cords = RepresentScanpath$representScanpath[,c("Fixation_Position_X_px","Fixation_Position_Y_px")]

  n = dim(cords)[1] - 1
  saccadlength = c()
  for (i in 1:n) {
    saccadlength[i] = dist(cords[c(i,i+1),])
  }

  indshort = which(saccadlength < alpha)
  if(length(indshort)!=0){
    ind = indshort[1]

    vertices = rbind(RepresentScanpath$AOIpolygons[[ind]]@polygons[[1]]@Polygons[[1]]@coords,
                     RepresentScanpath$AOIpolygons[[ind + 1]]@polygons[[1]]@Polygons[[1]]@coords)

    vertices = concaveman::concaveman(vertices)


    mergedPolygon = coordsToSpatialPolygons(vertices)
    newFixationPosition = polygonCenter(mergedPolygon)
    Fixation_start = RepresentScanpath$representScanpath$Event_Start_Trial_Time_ms[ind]
    Fixation_end = RepresentScanpath$representScanpath$Event_Ende_Trial_Time_ms[ind+1]
    Fixation_Duration = Fixation_end - Fixation_start

    RepresentScanpath$representScanpath$Fixation_Position_X_px[ind] = newFixationPosition[1]
    RepresentScanpath$representScanpath$Fixation_Position_Y_px[ind] = newFixationPosition[2]
    RepresentScanpath$representScanpath$Event_Ende_Trial_Time_ms[ind] = Fixation_end
    RepresentScanpath$representScanpath$Duration[ind] = Fixation_Duration

    RepresentScanpath$representScanpath = RepresentScanpath$representScanpath[-(ind+1),]

    RepresentScanpath$AOIpolygons[[ind]] = mergedPolygon
    RepresentScanpath$AOIpolygons = RepresentScanpath$AOIpolygons[-(ind+1)]

    return(RepresentScanpath)


  }else{
    return(RepresentScanpath)
  }



}

