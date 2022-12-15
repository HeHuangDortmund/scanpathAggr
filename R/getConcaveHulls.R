
#' Function to calculate concave hulls on each cross section

#' @param  Fixation_singleStimulus data.frame containing the following 6 column, including
#' (1) Participant, which is the name or id for the participants
#' (2) Fixation_Position_X_px
#' (3) Fixation_Position_Y_px
#' (4) Event_Start_Trial_Time_ms, which is the start time of fixation in seconds
#' (5) Event_Ende_Trial_Time_ms, which is end time of the fixation in seconds
#' (6) Duration, duration of the fixation in seconds.
#' Each row of the data.frame is a fixation.


#' @param ExperimentDuration numeric, total time of viewing in seconds
#' @param et numeric,distance of the cross sections in seconds.
#' @param eps_ini numeric, the initial epsilon of DBSCAN, which should be half of stimulus width in pixel
#' @return A list of Lists with one element, wchich ist a numeric matrix of two column.
#' The output of this function can be uses direct in the function getClusteringLabelsForEachSection()


#' @export
#' @import dbscan concaveman data.table
#' @examples
#' data(scanpath_MIT1003_i1182314083)
#' getConcaveHulls(scanpath_MIT1003_i1182314083, 3, 0.04)




getConcaveHulls = function(Fixation_singleStimulus,
                          ExperimentDuration,
                          et,
                          eps_ini = 400  # half screen width
){

  #library(dbscan)
  #library(concaveman)
  #library(data.table)

  # define the features in data.table as NULL to pass R CMD check
  Event_Start_Trial_Time_ms <- Event_Ende_Trial_Time_ms <- Fixation_Position_X_px <- NULL
  Fixation_Position_Y_px <- cluster <- NULL

  Fixation_singleStimulus = as.data.table(Fixation_singleStimulus)
  timepoint = seq(et, ExperimentDuration, by = et)
  n_timepoint = length(timepoint)
  convex_hull = list()
  for (i in 1:n_timepoint) {

    eps = eps_ini
    data_on_section = Fixation_singleStimulus[Event_Start_Trial_Time_ms <= timepoint[i] &
                                                Event_Ende_Trial_Time_ms >= timepoint[i]]
    data_on_section = data_on_section[,c("Fixation_Position_X_px", "Fixation_Position_Y_px")]

    if(dim(data_on_section)[1] < 3){ # if no or less than 3 fixation on this section, it can not form a polygon
      convex_hull[[i]] = NA
    } else{
      minPts = max(ceiling(dim(data_on_section)[1]/2), 3) # more than half points and at least 3
      db = dbscan(data_on_section, eps = eps, minPts = minPts) #

      n_cluster = length(unique(db$cluster[db$cluster != 0])) # why minus one ? cluster 0 are noises
      n_cluster

      if(n_cluster == 0){
        convex_hull[[i]] = NA
      }else{
        while (n_cluster == 1) {# decrese eps until no cluster exists
          db = dbscan(data_on_section, eps = eps, minPts = minPts)
          #hullplot(data_on_section, db, main = "DBSCAN")
          n_cluster = length(unique(db$cluster[db$cluster != 0]))
          eps = eps - 1
        }
        eps = eps + 2
        db = dbscan(data_on_section, eps = eps, minPts = minPts)
        n_cluster = length(unique(db$cluster[db$cluster != 0])) #

        while (n_cluster == 1) {# increse minPts until no cluster exists
          db = dbscan(data_on_section, eps = eps, minPts = minPts)
          #hullplot(data_on_section, db, main = "DBSCAN")
          n_cluster = length(unique(db$cluster[db$cluster != 0]))
          minPts = minPts + 1
        }
        minPts = minPts - 2

        db = dbscan(data_on_section, eps = eps, minPts = minPts)
        n_cluster = length(unique(db$cluster[db$cluster != 0]))

        if(n_cluster == 0){
          convex_hull[[i]] = NA
        } else{
          data_on_section[, cluster:= db$cluster]
          clusters = list()
          for (j in 1:n_cluster) {
            clusters[[j]] =  as.matrix(data_on_section[cluster == j][,1:2])
          }
          convex_hull[[i]] =  lapply(clusters, concaveman)
        }

      }


    }
  }

  return(convex_hull)

}
