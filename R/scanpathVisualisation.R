#' Function to visualize scanpaths and AOIs on the stimulus


#' @param  scanPath  data.frame with default NULL containing 6 column, including
#' (1) Participant, which is the name or id for the participants
#' (2) Fixation_Position_X_px
#' (3) Fixation_Position_Y_px
#' (4) Event_Start_Trial_Time_ms, start time of fixation in seconds
#' (5) Event_Ende_Trial_Time_ms, end time of the fixation in seconds
#' (6) Duration
#' These are the original scanpaths, form which the representative scanpath is computed.
#' By NULL draw this function only the representative scanpath

#'
#' @param representScanpath The obtained representative scanpath, output of the function getReprentativeScanpath() or scanpathAggr().
#' The default ist NULL, under which show draw this function only the original scanpaths.
#' @param stimulusImgPath file path of the stimulus
#' @param drawAOIs logical (default: TRUE), whether to draw the short time AOIs contained in the representScanpath
#'
#' @return A object of class ggplot from the package ggplot2



#' @export
#' @import ggplot2 data.table grDevices
#' @importFrom imager load.image height width
#' @examples
#' data(scanpath_MIT1003_i1182314083)
#' reprensentScanpath = scanpathAggr(scanpath_MIT1003_i1182314083, 3)
#' stimuluspath = system.file('stimulus/i1182314083.jpeg', package = "scanpathAggr")
#' scanpathVisualisation(scanpath_MIT1003_i1182314083, reprensentScanpath, stimuluspath)


scanpathVisualisation = function(scanPath=NULL,
                                   representScanpath=NULL,
                                   stimulusImgPath,
                                   drawAOIs = TRUE){

  #library(ggplot2)
  #library(imager)
  #library(data.table)

  # define the features in data.table as NULL to pass R CMD check
  Event_Start_Trial_Time_ms <- Event_Ende_Trial_Time_ms <- Fixation_Position_X_px <- NULL
  Fixation_Position_Y_px <- cluster <- Participant <- Duration <- x <- y  <- Order <- NULL

  #background_image: A function used in ggplot for ploting the background image
  background_image <- function(raster.img){
    annotation_raster(raster.img,
                      xmin = -Inf, xmax = Inf,
                      ymin = -Inf, ymax = Inf)
  }

  img = load.image(stimulusImgPath)

  if(!is.null(scanPath)){
    scanPath$Fixation_Position_Y_px = height(img) - scanPath$Fixation_Position_Y_px
  }

  if(!is.null(representScanpath)){
    representScanpath$representScanpath$Fixation_Position_Y_px = height(img) - representScanpath$representScanpath$Fixation_Position_Y_px
  }



  #if(representative) cat('draw a representative scanpath')
  if(is.null(representScanpath)){ # only scanpath or scanpaths
    #check scanPath is from a single Paticipant or a group
    if('Participant' %in% names(scanPath)){
      if(length(unique(scanPath$Participant)) > 1){
        singleSP = FALSE
        cat('draw scanpath form a group of participants')
      }else{
        singleSP = TRUE
        cat('draw scanpath form a single participant')
      }
    }else{
      singleSP = TRUE
      cat('draw scanpath form a single participant')
    }

    if(singleSP == FALSE){
      # add a column named Order
      order = do.call(c,sapply(scanPath[,.N, by = Participant]$N, function(n) 1:n))
      scanPath$Order = order
      img = load.image(stimulusImgPath)
      p <- ggplot(scanPath, aes(x = Fixation_Position_X_px,
                                y = Fixation_Position_Y_px,
                                col = Participant)) +
        background_image(img) +
        geom_line()+
        geom_point(aes(size = Duration)) +
        ylim(0, height(img)) +
        xlim(0, width(img)) +
        theme(legend.position = "none")
      #geom_text(aes(label = Order, y = Fixation_Position_Y_px + 10))

    }else{ # draw a single scanpath of a single participant
      # add a column named order
      scanPath$Order = 1:dim(scanPath)[1]
      img = load.image(stimulusImgPath)
      randomColor = rainbow(26, s=.6, v=.9)[sample(1:26,1)]
      p <-ggplot(scanPath, aes(x = Fixation_Position_X_px, y = Fixation_Position_Y_px)) +
        background_image(img) +
        geom_line(col = randomColor)+
        geom_point(aes(size = Duration), col = randomColor)+
        ylim(0, height(img)) +
        xlim(0, width(img))+
        theme(legend.position = "none")

    }


  }else if(is.null(scanPath)){ # only representative scanpath ???????????????????

    img = load.image(stimulusImgPath)
    scanPath_rep = representScanpath$representScanpath
    scanPath_rep$Order = 1:(dim(scanPath_rep)[1])
    if(!is.null(representScanpath$AOIpolygons)){
      AOIs_coords = list()
      for(i in 1:length(representScanpath$AOIpolygons)){
        AOIs_coords[[i]] = representScanpath$AOIpolygons[[i]]@polygons[[1]]@Polygons[[1]]@coords
        AOIs_coords[[i]][,2] = height(img) - AOIs_coords[[i]][,2]
      }
      #AOIs = RepresentScanpath$AOIpolygons
      nrows = sapply(AOIs_coords, function(x) dim(x)[1])
      if(length(nrows) <= 9){
        AOIs = rep(paste0('AOI',1:length(nrows)), nrows)
      }else{
        tmp = c(paste0('AOI0',1:9), paste0('AOI', 10:length(nrows)))
        AOIs = rep(tmp, nrows)
      }


      AOIcoord = do.call(rbind,AOIs_coords)
      AOIcoord = as.data.frame(cbind(AOIcoord, AOIs))
      names(AOIcoord) = c('x', 'y', 'AOIs')
      AOIcoord$x = as.numeric(AOIcoord$x)
      AOIcoord$y = as.numeric(AOIcoord$y)
      scanPath_rep$Participant = 'represent'

      if(drawAOIs){
        p <- ggplot()+
          background_image(img) +
          geom_polygon(data = AOIcoord, aes(x = x, y = y, fill = AOIs),alpha = 0.7, color = NA)+
          geom_path(data = scanPath_rep, aes(x = Fixation_Position_X_px,
                                             y = Fixation_Position_Y_px), col = 'red', size = 2)+
          geom_point(data = scanPath_rep,aes(x = Fixation_Position_X_px,
                                             y = Fixation_Position_Y_px,
                                             size = Duration),col = 'red') +
          geom_label(data = scanPath_rep,aes(label = Order,
                                            x = Fixation_Position_X_px + 30,
                                            y = Fixation_Position_Y_px + 30), col = 'red', size = 2.5) +
          ylim(0, height(img)) +
          xlim(0, width(img))

        #theme(legend.position = "none")
      }else{ # if no AOIs in represent scanpath, than do not draw AOI polygons
        p <- ggplot()+
          background_image(img) +
          #geom_polygon(data = AOIcoord, aes(x = x, y = y, fill = AOIs),alpha = 0.7, color = NA)+
          geom_path(data = scanPath_rep, aes(x = Fixation_Position_X_px,
                                             y = Fixation_Position_Y_px), col = 'red', size = 2)+
          geom_point(data = scanPath_rep,aes(x = Fixation_Position_X_px,
                                             y = Fixation_Position_Y_px,
                                             size = Duration),col = 'red') +
          geom_label(data = scanPath_rep,aes(label = Order,
                                            x = Fixation_Position_X_px + 30,
                                            y = Fixation_Position_Y_px + 30), col = 'red', size = 2.5) +
          ylim(0, height(img)) +
          xlim(0, width(img))
      }
    }

  } else{ # scanpaths representative scanpath together

    # add a column named order
    #order = do.call(c,sapply(scanPath[,.N, by = Participant]$N, function(n) 1:n))
    #scanPath$Order = order
    scanPath_rep = representScanpath$representScanpath
    scanPath_rep$Order = 1:(dim(scanPath_rep)[1])

    if(!is.null(representScanpath$AOIpolygons)){
      AOIs_coords = list()
      for(i in 1:length(representScanpath$AOIpolygons)){
        AOIs_coords[[i]] = representScanpath$AOIpolygons[[i]]@polygons[[1]]@Polygons[[1]]@coords
        AOIs_coords[[i]][,2] = height(img) - AOIs_coords[[i]][,2]
      }
      #AOIs = RepresentScanpath$AOIpolygons
      nrows = sapply(AOIs_coords, function(x) dim(x)[1])
      if(length(nrows) <= 9){
        AOIs = rep(paste0('AOI',1:length(nrows)), nrows)
      }else{
        tmp = c(paste0('AOI0',1:9), paste0('AOI', 10:length(nrows)))
        AOIs = rep(tmp, nrows)
      }

      AOIcoord = do.call(rbind,AOIs_coords)
      AOIcoord = as.data.frame(cbind(AOIcoord, AOIs))
      names(AOIcoord) = c('x', 'y', 'AOIs')
      AOIcoord$x = as.numeric(AOIcoord$x)
      AOIcoord$y = as.numeric(AOIcoord$y)
      scanPath_rep$Participant = 'represent'
      img = load.image(stimulusImgPath)

      if(drawAOIs){
        p <- ggplot()+
          background_image(img) +
          geom_path(data=scanPath, aes(x = Fixation_Position_X_px,
                                       y = Fixation_Position_Y_px,
                                       col = Participant), show.legend=FALSE)+
          geom_point(data=scanPath,aes(x = Fixation_Position_X_px,
                                       y = Fixation_Position_Y_px,
                                       size = Duration,
                                       col = Participant), show.legend=FALSE) +
          geom_polygon(data = AOIcoord, aes(x = x, y = y, fill = AOIs),alpha = 0.7, color = NA)+
          geom_path(data = scanPath_rep, aes(x = Fixation_Position_X_px,
                                             y = Fixation_Position_Y_px), col = 'red', size = 2)+
          geom_point(data = scanPath_rep,aes(x = Fixation_Position_X_px,
                                             y = Fixation_Position_Y_px,
                                             size = Duration),col = 'red') +
          geom_label(data = scanPath_rep,aes(label = Order,
                                            x = Fixation_Position_X_px + 30,
                                            y = Fixation_Position_Y_px + 30), col = 'red', size = 2.5) +
          ylim(0, height(img)) +
          xlim(0, width(img))

          #theme(legend.position = "none")
      }else{
        p <- ggplot()+
          background_image(img) +
          geom_path(data=scanPath, aes(x = Fixation_Position_X_px,
                                       y = Fixation_Position_Y_px,
                                       col = Participant))+
          geom_point(data=scanPath,aes(x = Fixation_Position_X_px,
                                       y = Fixation_Position_Y_px,
                                       size = Duration,
                                       col = Participant)) +
          #geom_polygon(data = AOIcoord, aes(x = x, y = y, fill = AOIs, alpha = 0.7), color = NA)+
          geom_path(data = scanPath_rep, aes(x = Fixation_Position_X_px,
                                             y = Fixation_Position_Y_px), col = 'red', size = 2)+
          geom_point(data = scanPath_rep,aes(x = Fixation_Position_X_px,
                                             y = Fixation_Position_Y_px,
                                             size = Duration),col = 'red') +
          geom_label(data = scanPath_rep,aes(label = Order,
                                            x = Fixation_Position_X_px + 20,
                                            y = Fixation_Position_Y_px + 20), col = 'red', size = 2.5) +
          ylim(0, height(img)) +
          xlim(0, width(img))+
          theme(legend.position = "none")

      }




    }else{
      scanPath_rep$Participant = 'represent'
      img = load.image(stimulusImgPath)
      p <- ggplot()+
        background_image(img) +
        geom_path(data=scanPath, aes(x = Fixation_Position_X_px,
                                     y = Fixation_Position_Y_px,
                                     col = Participant))+
        geom_point(data=scanPath,aes(x = Fixation_Position_X_px,
                                     y = Fixation_Position_Y_px,
                                     size = Duration,
                                     col = Participant)) +
        #geom_polygon(data = AOIcoord, aes(x = x, y = y, fill = AOIs, alpha = 0.7), color = NA)+
        geom_path(data = scanPath_rep, aes(x = Fixation_Position_X_px,
                                           y = Fixation_Position_Y_px), col = 'red', size = 2)+
        geom_point(data = scanPath_rep,aes(x = Fixation_Position_X_px,
                                           y = Fixation_Position_Y_px,
                                           size = Duration),col = 'red') +
        geom_label(data = scanPath_rep,aes(label = Order,
                                          x = Fixation_Position_X_px + 30,
                                          y = Fixation_Position_Y_px + 30), col = 'red', size = 2.5) +
        ylim(0, height(img)) +
        xlim(0, width(img))+
        theme(legend.position = "none")
    }


  }
  return(p)
}


