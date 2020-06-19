#' Read and prepare transects
#'
#' Read transects from admitted file formats by sf::st_read function, and
#'   prepare the data for analysis workflows.
#'
#' @param x object of class \code{sf} or path to geometries file. See details.
#'
#' @param start_from character; \code{'E'} assigns sequential id's to
#'   transects starting from East, \code{'N'} from North, and so on.
#'
#' @param ... other arguments passed to sf::st_read.
#'
#' @details If \code{x} is a character string, it must be the path to
#'   an external data source (e.g. a file of line geometries). If \code{x} is
#'   an object of class \code{sf}, it must be one with geometry types \code{LINESTRING}
#'   or \code{MULTILINESTRING}.
#'
#'   A transect consists of one line comprised by two
#'   vertices, the first of which must be digitized landward.
#'   Currently, the function only accepts LINESTRING or MULTILINESTRING
#'   in projected coordinate systems (e.g. EPSG:32619).
#'
#' @return An object of class transect, which inherits both 'sf' and 'data.frame'
#'   classes, containing a \code{coastr_id}.
#'
#' @examples
#' (file_path <- system.file('extdata', 'transects-long.geojson', package = 'coastr'))
#' (transect <- read_transect(file_path, start_from = 'N'))
#'
#' @importFrom magrittr "%>%"
#' @importFrom assertthat assert_that
#' @importFrom sf st_read st_coordinates st_centroid st_geometry_type
#' @importFrom dplyr arrange
#'
#' @export

read_transect <- function(x, ...) UseMethod('read_transect')

#' @name read_transect
#' @export

read_transect.character <- function(x, start_from = NULL, ...) {
  obj <- invisible(st_read(x, quiet = T, ...))
  check_geometry_type(obj)
  check_class_transect(x)
  transect <- add_id_col_arrange_class(obj, start_from  = start_from)
  return(transect)
}

#' @name read_transect
#' @export

read_transect.sf <- function(x, start_from = NULL, ...) {
  check_geometry_type(x)
  check_class_transect(x)
  obj <- x
  transect <- add_id_col_arrange_class(obj, start_from  = start_from)
  return(transect)
}

check_geometry_type <- function(obj) {
  geom_type <- st_geometry_type(obj)
  if(! all(geom_type %in% c('MULTILINESTRING', 'LINESTRING')))
    stop('Admitted geometry types are MULTILINESTRING or LINESTRING')
}

check_class_transect <- function(obj) {
  if(any(class(obj) %in% 'transect'))
    stop('The source is already an object of class transect')
}

add_id_col_arrange_class <- function(obj, ...) {
  obj$coastr_id <- increment_from_id(obj = obj, ...)
  transect <- obj %>% arrange(coastr_id)
  class(transect) <- c("transect", class(transect))
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
