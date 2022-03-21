#' Rasterize each cluster and return the integrated rasters (mosaic)
#' when the plot is horizontal setting
#'
#' This function ---
#'
#' @importFrom Seurat GetTissueCoordinates
#' @import raster
#' @import spdep
#' @import sp
#' @export
#'
#' @param sample seruat object that have cluster labels attached.
#'
#' @return length and height of the extent.
#'
#' @examples
#' sample <- spatialDataPrep("/Users/ninasong/Desktop/Craig_lab/GeoSpatial/breast_cancer")
#' mosaicIntegration <- rasterizeEachCluster(sample)

rasterizeEachCluster <- function(sample){
  sampleCoord <- GetTissueCoordinates(sample)
  sampleCoord$clusters <- sample$Cluster

  r <- rasterPrep(sample)
  mosaicClusters <- list()

  for (i in 1:length(unique(sampleCoord$clusters))){
    subCluster <- subset(sampleCoord, clusters == i)
    spdf <- subCluster
    coordinates(spdf) <- c("imagerow", "imagecol") #create a Spatial object
    nam <- paste("clusterRast", i, sep = "_")
    clusterName <- assign(nam, rasterize(spdf, r, field = i, extent = jc.extent, background = 0))
    valueCheck <- sum(clusterName@data@values)/i
    if (valueCheck != nrow(subCluster)){
      stop("The number of pixels is not equal to the number of barcodes")
    }
    mosaicClusters <- c(mosaicClusters, clusterName)
  }
  names(mosaicClusters)[1:2] <- c('x', 'y')
  mosaicClusters$fun <- max
  mosaicClusters$na.rm <- TRUE

  mosaicIntegration <- do.call(mosaic, mosaicClusters)
  return(mosaicIntegration)
}

#' Rasterize each cluster and return the integrated rasters (mosaic)
#' when the plot is veritical setting.
#'
#' This function ---
#'
#' @importFrom Seurat GetTissueCoordinates
#' @import raster
#' @import spdep
#' @import sp
#' @export
#'
#' @param sample seruat object that have cluster labels attached.
#'
#' @return length and height of the extent.
#'
#' @examples
#' sample <- spatialDataPrep("/Users/ninasong/Desktop/Craig_lab/GeoSpatial/slide120_D1")
#' mosaicIntegration <- rasterizeEachClusterVer(sample)

rasterizeEachClusterVer <- function(sample){
  sampleCoord <- GetTissueCoordinates(sample)
  sampleCoord$clusters <- sample$Cluster

  r <- rasterPrepVer(sample)
  mosaicClusters <- list()

  for (i in 1:length(unique(sampleCoord$clusters))){
    subCluster <- subset(sampleCoord, clusters == i)
    spdf <- subCluster
    coordinates(spdf) <- c("imagerow", "imagecol") #create a Spatial object
    nam <- paste("clusterRast", i, sep = "_")
    clusterName <- assign(nam, rasterize(spdf, r, field = i, extent = jc.extent, background = 0))
    valueCheck <- sum(clusterName@data@values)/i
    if (valueCheck != nrow(subCluster)){
      stop("The number of pixels is not equal to the number of barcodes")
    }
    mosaicClusters <- c(mosaicClusters, clusterName)
  }
  names(mosaicClusters)[1:2] <- c('x', 'y')
  mosaicClusters$fun <- max
  mosaicClusters$na.rm <- TRUE

  mosaicIntegration <- do.call(mosaic, mosaicClusters)
  return(mosaicIntegration)
}
