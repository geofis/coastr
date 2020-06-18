#' Read and prepare transects
#'
#' Read transects from admitted file formats by sf::st_read function, and
#'   prepare the data for analysis workflows.
#'
#' @param dsn character; the data source name containing line
#'   geometries. See details.
#'
#' @param profile logical; if TRUE (default), lines are prepared for generating
#'   topographic profiles; if FALSE, time-series transects are generated.
#'
#' @param start_from character; \code{'E'} assigns sequential id's to
#'   transects starting from East, \code{'N'} from North, and so on.
#'
#' @param ... other arguments passed to sf::st_read.
#'
#' @details A transect consists of one line comprised by two
#'   vertices, the first of which must be digitized landward.
#'   Currently, the function only accepts line geometries (no multiline)
#'   in projected coordinate systems (e.g. EPSG:32619). A profile transect
#'   must match at least two pixels of the reference DSM (see
#'   \code{read_dsm}).
#'
#'   A time-series transect must cut across a set of shorelines;
#'   the latter may be digitized by hand (following the landward-seaward
#'   convention) or using semi-automatic tools (e.g. CoastSat toolchain)
#'
#' @return An object of class transect, which inherits both 'sf' and 'data.frame'
#'   classes. If \code{profile=TRUE} an object of class \code{profile} is returned.
#'   If \code{profile=FALSE}, an objetct of class \code{time_series} (time-series) is
#'   returned. In any case, the return inherits the \code{sf} class.
#'
#' @examples
#' (file_path <- system.file('extdata', 'transects-long.geojson', package = 'coastr'))
#' (transect <- read_transect(file_path, profile = T, start_from = 'N'))
#'
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
#' @importFrom sf st_read st_coordinates st_centroid
#' @importFrom dplyr arrange
#'
#' @export
read_transect <- function(dsn, profile = T, start_from = NULL, ...) {
  assert_that(
    is.logical(profile),
    msg = 'Argument "profile" is not a logical value. Only TRUE or FALSE are admitted'
  )
  assert_that(length(profile) == 1)
  obj <- invisible(st_read(dsn, quiet = T, ...))
  obj <- obj[,-(1:ncol(obj))]
  obj$id <- increment_from_id(obj = obj, start_from = start_from)
  transect <- obj %>% arrange(id)
  if(profile) {
    class(transect) <- c("transect", "profile", class(transect))
  } else {
    class(transect) <- c("transect", "time_series", class(transect))
  }
  return(transect)
}

increment_from_id <- function(obj, start_from = 'N') {
  assert_that(is.character(start_from))
  assert_that(length(start_from) == 1)
  assert_that(nchar(start_from) == 1)
  we <- factor(order(st_coordinates(st_centroid(obj$geometry))[,1]))
  sn <- factor(order(st_coordinates(st_centroid(obj$geometry))[,2]))
  switch(start_from,
    'E' = rev(we),
    'N' = rev(sn),
    'S' = sn,
    'W' = we,
    stop('Invalid start_from value. Select one of E, N, S, W')
  )
}
