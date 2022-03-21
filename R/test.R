library(devtools)
#setwd("/Users/ninasong/Desktop/Craig_lab/GeoSpatial")
#create("testPackage")

############  Data Prep  ###############
# Including:
# spatialDataPrep
# resolutionCalc
# rasterPrep

#' Prepare input data
#'
#' This function ---
#'
#' @import Seurat
#' @importFrom utils read.csv
#' @export
#'
#' @param filename folder pathway that contains feature-bc-mstrix, analysis and spatial information.
#'
#' @return seruat  object that have cluster labels attached.
#'
#' @examples
#' filename <- "/Users/ninasong/Desktop/Craig_lab/GeoSpatial/breast_cancer"
#' sample <- spatialDataPrep(filename)
spatialDataPrep <- function(filename){
  inputSample <- Load10X_Spatial(filename)
  sampleTransform <- SCTransform(inputSample, assay = "Spatial", verbose = FALSE)
  sampleMeta <- read.csv(paste(filename, "/analysis/clustering/graphclust/clusters.csv", sep = ""), sep = ',')
  rownames(sampleMeta) <- sampleMeta$Barcode
  sampleCluster <- AddMetaData(object = sampleTransform, metadata = sampleMeta)
  Idents(sampleCluster) <- "Cluster"
  return(sampleCluster)
}


