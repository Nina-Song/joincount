#' Finding the size of extent when the plot is horizonal setting.
#'
#' This function ---
#'
#' @importFrom Seurat GetTissueCoordinates
#' @import raster
#' @export
#'
#' @param sample seruat object that have cluster labels attached.
#'
#' @return length and height of the extent.
#'
#' @examples
#' sample <- spatialDataPrep("/Users/ninasong/Desktop/Craig_lab/GeoSpatial/breast_cancer")
#' raster <- rasterPrep(sample)

rasterPrep <- function(sample){
  #get coordinates
  sampleCoord <- GetTissueCoordinates(sample)
  sampleCoord$clusters <- sample$Cluster
  coordSummary <- as.data.frame(apply(sampleCoord,2,summary))
  #write_csv(coordSummary, paste(filename, "/coord.csv", sep = ""))

  #create raster that will apply to all subsets
  imagerow.min <- as.integer(coordSummary$imagerow[1])
  iamgerow.max <- as.integer(coordSummary$imagerow[6])
  imagecol.min <- as.integer(coordSummary$imagecol[1])
  iamgecol.max <- as.integer(coordSummary$imagecol[6])
  jc.extent <- extent(imagerow.min-100, iamgerow.max+100, imagecol.min-100, iamgecol.max+100)

  resolutionList <- resolutionCalc(sample)
  r <- raster(resolution = resolutionList, ext = jc.extent)
  return(r)
}

#' Finding the size of extent when the coordinates are veritical setting
#'
#' This function ---
#'
#' @importFrom Seurat GetTissueCoordinates
#' @import raster
#' @export
#'
#' @param sample seruat object that have cluster labels attached.
#'
#' @return length and height of the extent.
#'
#' @examples
#' sample <- spatialDataPrep("/Users/ninasong/Desktop/Craig_lab/GeoSpatial/slide120_D1")
#' raster <- rasterPrepVer(sample)

rasterPrepVer <- function(sample){
  #get coordinates
  sampleCoord <- GetTissueCoordinates(sample)
  sampleCoord$clusters <- sample$Cluster
  coordSummary <- as.data.frame(apply(sampleCoord,2,summary))
  #write_csv(coordSummary, paste(filename, "/coord.csv", sep = ""))

  #create raster that will apply to all subsets
  imagerow.min <- as.integer(coordSummary$imagerow[1])
  iamgerow.max <- as.integer(coordSummary$imagerow[6])
  imagecol.min <- as.integer(coordSummary$imagecol[1])
  iamgecol.max <- as.integer(coordSummary$imagecol[6])
  jc.extent <- extent(imagerow.min-100, iamgerow.max+100, imagecol.min-100, iamgecol.max+100)

  resolutionList <- resolutionCalcVer(sample)
  r <- raster(resolution = resolutionList, ext = jc.extent)
  return(r)
}
