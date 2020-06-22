#' Create transects
#'
#' Create transects of user-defined length and at regular distance
#' along an input shoreline
#'
#' @param x name of an object of class shoreline
#' @param distance numeric; separation distance between transects
#' @param length numeric; Euclidean distance of transects
#' @param sides character; takes 'left', 'right' and 'both'
#'
#' @importFrom sf st_coordinates st_line_sample st_cast st_crs
#' @importFrom sf st_geometry st_sf st_sfc st_multilinestring
#' @importFrom zoo zoo
#' @export
create_transect <- function(obj, distance = NULL, transect_length = 1, sides) {
  point <- create_point(obj, distance)
  coords <- calculate_xy_coords(point)
  lag <- calculate_lag(coords)
  angle <- calculate_angle(lag)
  multilinestring <- create_multilinestring(angle, transect_length)
}

create_point <- function(obj, distance) {
  if(is.null(distance)) {
    point_vertex <- st_cast(obj, 'POINT')
    point <- st_geometry(point_vertex)
  } else {
    multipoint_distance <- st_line_sample(obj, density = 1/distance)
    point <- st_cast(multipoint_distance, 'POINT')
  }
  df_point <- data.frame(coastr_id = 1:length(point))
  point_sf <- st_sf(df_point, geometry = point)
  attr(point_sf, 'epsg') <- st_crs(obj)$epsg
  class(point_sf) <- c('point', class(point_sf))
  return(point_sf)
}

calculate_xy_coords <- function(point) {
  x_coord <- st_coordinates(point)[, 1]
  y_coord <- st_coordinates(point)[, 2]
  coords <- data.frame(coastr_id = point$coastr_id, x = x_coord, y = y_coord)
  attr(coords, 'epsg') <- attributes(point)$epsg
  class(coords) <- c('coords', class(coords))
  return(coords)
}

calculate_lag <- function(coords) {
  lag_forward <- sapply(zoo(coords[,2:3]), diff, na.pad=T)
  lag_backward <- sapply(rev(zoo(coords[,2:3])), function(x) rev(diff(x, na.pad=T)))
  lag_both <- cbind(coords, lag_forward, lag_backward)
  colnames(lag_both) <- c(colnames(coords),
                          'x_lag_forward', 'y_lag_forward',
                          'x_lag_backward', 'y_lag_backward')
  attr(lag_both, 'epsg') <- attributes(coords)$epsg
  class(lag_both) <- c('lag', class(lag_both))
  return(lag_both)
}

calculate_angle <- function(lag) {
  angle_forward <- atan2(lag[,'y_lag_forward'], lag[, 'x_lag_forward'])
  angle_backward <- atan2(lag[,'y_lag_backward'], lag[, 'x_lag_backward'])
  angles_both <- cbind(lag, angle_forward, angle_backward)
  mean_angle <- rowMeans(angles_both[,c('angle_forward', 'angle_backward')], na.rm = T)
  df_angle <- cbind(angles_both, mean_angle)
  df_angle[c(1,nrow(df_angle)), 'mean_angle'] <-
    df_angle[c(1,nrow(df_angle)), 'mean_angle'] + pi/2
  attr(df_angle, 'epsg') <- attributes(lag)$epsg
  class(df_angle) <- c('angle', class(df_angle))
  return(df_angle)
}

create_multilinestring <- function(angle, transect_length) {
  multilinestring <- lapply(angle$coastr_id, function(z) {
    x0 <- with(angle, x[coastr_id == z])
    y0 <- with(angle, y[coastr_id == z])
    x1 <- with(angle, x[coastr_id == z] + transect_length * cos(mean_angle[coastr_id == z]))
    y1 <- with(angle, y[coastr_id == z] + transect_length * sin(mean_angle[coastr_id == z]))
    x2 <- with(angle, x[coastr_id == z] - transect_length * cos(mean_angle[coastr_id == z]))
    y2 <- with(angle, y[coastr_id == z] - transect_length * sin(mean_angle[coastr_id == z]))
    multilinestring_sfc <- st_sfc(st_multilinestring(
      list(
        rbind(c(x0, y0), c(x1, y1)),
        rbind(c(x0, y0), c(x2, y2))))
    )
    multilinestring_sf <- st_sf(data.frame(coastr_id = z), geometry = multilinestring_sfc)
    class(multilinestring_sf) <- c('transect', class(multilinestring_sf))
    return(multilinestring_sf)
  })
  multilinestring <- do.call(rbind, multilinestring)
  st_crs(multilinestring) <- attributes(angle)$epsg
  return(multilinestring)
}
