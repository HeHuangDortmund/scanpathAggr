
#' Function to find a representative scanpath by giving a group of scanpaths

#' @param  scanpaths data.frame containing the following 4 or 6 column, including
#' (1) Participant, which is the name or id for the participants
#' (2) Fixation_Position_X_px
#' (3) Fixation_Position_Y_px
#' (4) Event_Start_Trial_Time_ms, start time of fixation in seconds
#' (5) Event_Ende_Trial_Time_ms, end time of the fixation in seconds
#' (6) Duration
#' If there are no (4) and (5), this function will create fake start and end time for the fixations.
#' Each row represents a fixation, and fixations are  in temporal order.
#'
#' @param ExperimentDuration numeric, total time of viewing in seconds
#' @param et numeric with default 0.04,time distance of the cross sections in seconds.
#' @param theta numeric between 0 and 1, threshold of the similarity of two polygons
#' @param simplify logical with default FALSE, if TRUE, the found representative scanpath
#' will be simplified by giving threshold alpha in pixel
#' @param  alpha numeric with defaulf 50 (pixel), threshold of the simplification in pixel,
#' fixations in representative scanpath closer than 50 pixel will be merged through their AOIs
#' @return A list of two elements:
#'  -representScanpath: a data.frame for the representScanpath
#'  -AOIpolygons: List of elements of class 'SpatialPolygons' from R package "sp"


#' @export
#' @import dbscan concaveman data.table geometry rgeos prevR sp
#' @examples
#' data(scanpath_MIT1003_i1182314083)
#' scanpathAggr(scanpath_MIT1003_i1182314083, 3)



scanpathAggr = function(scanpaths, ExperimentDuration, et = 0.04, theta = 0.2,
                        simplify = FALSE, alpha = 50){

  options(warn=-1)


  if(is.null(scanpaths$Event_Start_Trial_Time_ms)){

    # scanpath without b and e:
    # if scanpaths contains only duration of each fixation,
    # but without the exact start time und end time,
    # then creat fake start time und end time for them by distibuting the totol
    # saccade time evenly between the fixations

    FIXIATIONS = list()
    Participants = unique(scanpaths$Participant)
    for (j in 1:length(Participants)) {

      P_j = scanpaths[scanpaths$Participant == Participants[j],]
      n_fix = dim(P_j)[1]
      saccad_dur = (ExperimentDuration  - sum(P_j$Duration))/n_fix
      Event_Start_Trial_Time_ms = c(0,cumsum(P_j$Duration)[1:(n_fix-1)]) + (0:(n_fix-1))*saccad_dur
      Event_Ende_Trial_Time_ms = Event_Start_Trial_Time_ms + P_j$Duration

      FIXIATIONS[[j]] = data.frame(Participant = Participants[j],
                                   Fixation_Position_X_px = P_j$Fixation_Position_X_px,
                                   Fixation_Position_Y_px = P_j$Fixation_Position_Y_px,
                                   Event_Start_Trial_Time_ms = Event_Start_Trial_Time_ms,
                                   Event_Ende_Trial_Time_ms = Event_Ende_Trial_Time_ms,
                                   Duration = P_j$Duration)


    }
    scanpaths = do.call(rbind,FIXIATIONS)
  }



  concave_hull = getConcaveHulls(scanpaths,
                               ExperimentDuration,
                               et)

  clusterings = getClusteringLabelsForEachSection(concave_hull, theta)
  RepresentScanpath = getRepresentScanpath(concave_hull, clusterings, et, scanpaths)


  if(simplify){
    RepresentScanpath = representativeScanpathSimplify(RepresentScanpath, alpha)
  }



  return(RepresentScanpath)
}
