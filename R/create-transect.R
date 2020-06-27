#' Create transects
#'
#' Create transects of user-defined length along an input line
#'
#' @param x name of an object of class \code{sf}, or a path to a file
#'   readable by the \code{sf::st_read} function, which must contain
#'   geometries of type \code{LINESTRING} or \code{MULTILINESTRING}.
#' @param transect_spacing character or numeric. If numeric, represents
#'   the spacing between transects in \code{x} CRS units. If character,
#'   accepts only the string 'vertices' as argument (allows partial matching)
#' @param length_left numeric; length of transects to the left
#'   of the input line. Default to \code{transect_spacing}
#' @param length_left numeric; length of transects to the right of the input line
#' @param reverse logical; reverse orientation of
#'   transects; default \code{FALSE}
#' @param ... other arguments passed to sf::st_read.
#' @details
#'
#' @return
#'
#' @examples
#'
#' @importFrom assertthat assert_that
#' @importFrom sf st_read st_coordinates st_geometry st_geometry_type st_line_sample st_cast st_crs `st_crs<-` st_sf st_sfc st_linestring st_multilinestring
#' @export
create_transect <- function(
  x, transect_spacing = NULL,
  length_left = if(
    all(!is.null(transect_spacing), is.numeric(transect_spacing)))
    transect_spacing,
  length_right = if(
    all(!is.null(transect_spacing), is.numeric(transect_spacing)))
    transect_spacing,
  reverse = FALSE, ...) {
  assert_that(
    any(
      all(is.character(transect_spacing), grepl(transect_spacing, 'vertices')),
      all(is.numeric(transect_spacing), transect_spacing > 0)),
    msg = 'transect_spacing must be "vertices" or a value greater than zero'
  )
  assert_that(
    !is.null(length_left), !is.null(length_right),
    msg = 'must set values for both length_left and length_right')
  assert_that(
    length_left >= 0, length_right >= 0,
    msg = 'length_left and length_right must be greater or equal than zero'
  )
  obj <- create_obj(x)
  check_object(obj)
  point <- create_point(obj, transect_spacing)
  coords <- calculate_xy_coords(point)
  lag <- calculate_lag(coords)
  angle <- calculate_mean_angle(lag)
  point_coordinates <- create_point_coordinates(angle, length_left, length_right)
  transects <- create_multilinestring(point_coordinates, length_left, length_right, reverse)
  return(transects)
}

create_obj <- function(x, ...) {
  if(is.character(x)) {
    obj <- invisible(st_read(dsn = x, quiet = T, ...))
  } else {
    if(any(class(x) %in% 'sf')) {
      obj <- x
    } else {
      stop('x must be a character string containing a path to a source
         or the name of an object of class sf')
    }
  }
  return(obj)
}

create_point <- function(obj, transect_spacing) {
  if(grepl(transect_spacing, 'vertices')) {
    point_vertex <- suppressWarnings(st_cast(obj, 'POINT'))
    point <- st_geometry(point_vertex)
  } else {
    multipoint <- st_line_sample(obj, density = 1/transect_spacing)
    point <- suppressWarnings(st_cast(multipoint, 'POINT'))
  }
  df_point <- data.frame(coastr_id = 1:length(point))
  point_sf <- st_sf(df_point, geometry = point)
  attr(point_sf, 'epsg') <- st_crs(obj)$epsg
  # class(point_sf) <- c('point', class(point_sf))
  return(point_sf)
}

calculate_xy_coords <- function(point) {
  x_coord <- st_coordinates(point)[, 1]
  y_coord <- st_coordinates(point)[, 2]
  coords <- data.frame(coastr_id = point$coastr_id, x = x_coord, y = y_coord)
  attr(coords, 'epsg') <- attributes(point)$epsg
  # class(coords) <- c('coords', class(coords))
  return(coords)
}

pad_vector <- function(x, position) {
  if(position == 'last') {
    x[length(x)] <- x[length(x)-1]
  } else {
    x[1] <- x[2]
  }
  return(x)
}

calculate_lag <- function(coords) {
  lag_forward <- sapply(coords[, 2:3], function(x) c(diff(x), NA))
  lag_backward <- sapply(coords[,2:3], function(x) {
    r <- rev(x)
    d <- diff(r)
    rb <- c(NA, rev(d))
  })
  lag_both <- cbind(coords, lag_forward, lag_backward)
  colnames(lag_both) <- c(colnames(coords),
                          'x_lag_forward', 'y_lag_forward',
                          'x_lag_backward', 'y_lag_backward')
  columns_to_pad <- grep('forward|backward', colnames(lag_both), value=T)
  lag_both[, columns_to_pad] <- sapply(
    columns_to_pad,
    function(x) {
      pad_vector(lag_both[, x], position=ifelse(grepl('forward', x), 'last', 'first'))
    }
  )
  attr(lag_both, 'epsg') <- attributes(coords)$epsg
  # class(lag_both) <- c('lag', class(lag_both))
  return(lag_both)
}

calculate_angle_by_side <- function(theta, phi) {
  left_angle <- ifelse(theta >= phi, phi + pi, phi)
  right_angle <- left_angle + pi
  df <- data.frame(left_angle, right_angle)
  return(df)
}

calculate_mean_angle <- function(lag) {
  angle_forward <- atan2(lag[,'y_lag_forward'], lag[, 'x_lag_forward'])
  angle_backward <- atan2(lag[,'y_lag_backward'], lag[, 'x_lag_backward'])
  angles_both <- cbind(lag, angle_forward, angle_backward)
  mean_angle <- rowMeans(angles_both[,c('angle_forward', 'angle_backward')], na.rm = T)
  df_angle <- cbind(angles_both, mean_angle)
  df_angle_by_side <- cbind(
    df_angle,
    calculate_angle_by_side(theta = df_angle$angle_forward, phi = df_angle$mean_angle)
  )
  attr(df_angle_by_side, 'epsg') <- attributes(lag)$epsg
  # class(df_angle) <- c('angle', class(df_angle))
  return(df_angle_by_side)
}

create_point_coordinates <- function(angle, length_left, length_right){
  point_coords <- within(angle, {
    y_right = y - length_right * sin(left_angle)
    x_right = x - length_right * cos(left_angle)
    y_left = y + length_left * sin(left_angle)
    x_left = x + length_left * cos(left_angle)
  })
  attr(point_coords, 'epsg') <- attributes(angle)$epsg
  return(point_coords)
}

create_multilinestring <- function(point_coordinates, length_left, length_right, reverse) {
  linestring_list <- lapply(1:nrow(point_coordinates), function(h) {
    linestring_sfg <- st_linestring(
      with(
        point_coordinates[h, ],
        if(reverse)
          rbind(c(x_right, y_right), c(x_left, y_left))
        else rbind(c(x_left, y_left), c(x_right, y_right))
      )
    )
    linestring_sfc <- st_sfc(linestring_sfg)
    linestring_sf <- st_sf(
      data.frame(coastr_id = h),
      geometry = linestring_sfc)
    return(linestring_sf)
  })
  linestring <- do.call('rbind', linestring_list)
  st_crs(linestring) <- attributes(point_coordinates)$epsg
  return(linestring)
}
