#' Plot transect object
#'
#' Plot transects with along with basic attributes
#'
#' @param x object of class transect
#' @param ... other arguments passed to ggplot2::geom_sf
#'
#' @method plot transect
#' @name plot
#'
#' @details plot.transect plots the transect using a minimal theme,
#'   and place a label with the ID and attributes over its centroids.
#'
#' @return An object of class ggplot.
#'
#' @examples
#'
#' transect <- read_transect('profiles.geojson')
#' plot(transect)
#'
#' @import ggplot2
#' @importFrom sf st_centroid
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @export
plot.transect <- function(x, ...) {
  n <- nrow(x)
  q <- brewer.pal.info[brewer.pal.info$category == 'qual',]
  v <- unlist(mapply(brewer.pal, q$maxcolors, rownames(q)))
  ggplot(x) + aes(color = id) +
    geom_sf(lwd = 1) +
    scale_color_manual(values = sample(v, n)) +
    geom_sf_text(
      data = suppressWarnings(x %>% st_centroid),
      aes(label = id), color = 'black', size = 4) +
    theme_minimal() +
    theme(legend.position = 'none')
}
