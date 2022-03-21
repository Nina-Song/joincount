#' Visulization of the rasterization results
#'
#' This function ---
#'
#' @param mosaicIntegration Integrated rasters of each cluster.
#' @export
#'
#' @examples
#' sample <- spatialDataPrep("/Users/ninasong/Desktop/Craig_lab/GeoSpatial/breast_cancer")
#' mosaicIntegration <- rasterizeEachCluster(sample)
#' mosaicIntPlot(mosaicIntegration)
#'
mosaicIntPlot <- function(mosaicIntegration){
  cuts <- c(0,0.99,1.99,2.99,3.99,4.99,5.99,6.99,7.99,8.99, 9.99, 10.99, 11.99) #for setting colors per cluster
  colors.raster <- c("#FFFFFF", "#810505", "#f79c09", "#f30808","#f3eb17", "#bcf775", "#5cd04d", "#2b7f20", "#2cdcc2", "#78cdf5", "#1b5fe4", "#811be4", "#cc6ae6", "#f59ad5", "#9c6d6d")

  plot(mosaicIntegration, breaks = cuts, col = colors.raster)
  # return(p1)
}
