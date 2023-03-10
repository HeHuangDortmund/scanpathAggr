% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docRepresentativeScanpathSimplify.R,
%   R/representativeScanpathSimplify.R
\docType{package}
\name{representativeScanpathSimplify}
\alias{representativeScanpathSimplify}
\title{Scanpath Aggregation Package}
\usage{
representativeScanpathSimplify(RepresentScanpath, alpha)
}
\arguments{
\item{RepresentScanpath}{output of the function getReprensentativeScanpath()}

\item{alpha}{numeric, threshold, saccade vector shorter as alpha will be merged}
}
\value{
A list of two elements:
 -representScanpath: a data.frame for the representScanpath
 -AOIpolygons: List of 10 of class 'SpatialPolygons' from R package "sp"
}
\description{
Simplify the found representative scanpath by merging the saccade vector shorter as giving threshold alpha
}
\author{
He Huang
}
