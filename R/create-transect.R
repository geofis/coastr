#' Create transects
#'
#' Create transects of user-defined length and at regular distance
#' along an input shoreline
#'
#' @param x name of an object of class \code{sf}, or a path to a file
#'   readable by the \code{sf::st_read} function, which must contain
#'   geometries of type \code{LINESTRING} or \code{MULTILINESTRING}.
#' @param distance numeric; separation distance between transects
#' @param transect_length numeric; Euclidean distance of transects
#' @param sides character; takes 'left', 'right' and 'across'
#' @param ... other arguments passed to sf::st_read.
#' @details
#'
#' @return
#'
#' @examples
#'
#' @importFrom magrittr `%>%`
#' @importFrom sf st_read st_coordinates st_geometry st_geometry_type st_line_sample st_cast st_crs `st_crs<-` st_sf st_sfc st_linestring st_multilinestring
#' @importFrom zoo zoo
#' @export
create_transect <- function(x, distance = NULL,
                            transect_length = 1, sides = NULL, ...) {
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
  check_object(obj)
  point <- create_point(obj, distance)
  coords <- calculate_xy_coords(point)
  lag <- calculate_lag(coords)
  angle <- calculate_mean_angle(lag)
  point_coordinates <- create_point_coordinates(angle, transect_length)
  point_sides <- find_side_of_points(point_coordinates)
  transects_sides <- create_transect_sides(point_sides, sides)
  return(transects_sides)
}

create_point <- function(obj, distance = distance) {
  if(is.null(distance)) {
    point_vertex <- suppressWarnings(st_cast(obj, 'POINT'))
    point <- st_geometry(point_vertex)
  } else {
    multipoint_distance <- st_line_sample(obj, density = 1/distance)
    point <- suppressWarnings(st_cast(multipoint_distance, 'POINT'))
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

calculate_lag <- function(coords) {
  lag_forward <- sapply(zoo(coords[,2:3]), diff, na.pad=T)
  lag_backward <- sapply(rev(zoo(coords[,2:3])), function(x) rev(diff(x, na.pad=T)))
  lag_both <- cbind(coords, lag_forward, lag_backward)
  colnames(lag_both) <- c(colnames(coords),
                          'x_lag_forward', 'y_lag_forward',
                          'x_lag_backward', 'y_lag_backward')
  attr(lag_both, 'epsg') <- attributes(coords)$epsg
  # class(lag_both) <- c('lag', class(lag_both))
  return(lag_both)
}

calculate_mean_angle <- function(lag) {
  angle_forward <- atan2(lag[,'y_lag_forward'], lag[, 'x_lag_forward'])
  angle_backward <- atan2(lag[,'y_lag_backward'], lag[, 'x_lag_backward'])
  angles_both <- cbind(lag, angle_forward, angle_backward)
  mean_angle <- rowMeans(angles_both[,c('angle_forward', 'angle_backward')], na.rm = T)
  df_angle <- cbind(angles_both, mean_angle)
  df_angle[c(1,nrow(df_angle)), 'mean_angle'] <-
    df_angle[c(1,nrow(df_angle)), 'mean_angle'] + pi/2
  attr(df_angle, 'epsg') <- attributes(lag)$epsg
  # class(df_angle) <- c('angle', class(df_angle))
  return(df_angle)
}

create_point_coordinates <- function(angle, transect_length) {
  point_coordinates_list <- lapply(angle$coastr_id, function(z) {
    x0 <- with(angle, x[coastr_id == z])
    y0 <- with(angle, y[coastr_id == z])
    x1 <- with(angle, x[coastr_id == z] + transect_length * cos(mean_angle[coastr_id == z]))
    y1 <- with(angle, y[coastr_id == z] + transect_length * sin(mean_angle[coastr_id == z]))
    x2 <- with(angle, x[coastr_id == z] - transect_length * cos(mean_angle[coastr_id == z]))
    y2 <- with(angle, y[coastr_id == z] - transect_length * sin(mean_angle[coastr_id == z]))
    df <- data.frame(coastr_id = rep(z, 3), index = c(0, 1, 2),
                     x = c(x0, x1, x2), y = c(y0, y1, y2))
    return(df)
  })
  point_coordinates <- do.call(rbind, point_coordinates_list)
  attr(point_coordinates, 'epsg') <- attributes(angle)$epsg
  # class(point_coordinates) <- c('point_coordinates', class(angle))
  return(point_coordinates)
}

find_side_of_points <- function(point_coordinates) {
  coastr_id_index <- unique(point_coordinates$coastr_id)
  sides_list <- lapply(coastr_id_index, function(z) {
    sign1 <- with(point_coordinates,
                  ( (x[coastr_id == ifelse(z==length(coastr_id_index), z, z + 1) & index == 0] - x[coastr_id == ifelse(z==length(coastr_id_index), z-1, z) & index == 0 ]) *
                      (y[coastr_id == z & index == 1] - y[coastr_id == ifelse(z==length(coastr_id_index), z-1, z) & index == 0 ]) -
                      (y[coastr_id == ifelse(z==length(coastr_id_index), z, z + 1) & index == 0 ] - y[coastr_id == ifelse(z==length(coastr_id_index), z-1, z) & index == 0 ]) *
                      (x[coastr_id == z & index == 1] - x[coastr_id == ifelse(z==length(coastr_id_index), z-1, z) & index == 0 ]) )
    )
    side1 <- ifelse(sign1 < 0, 'right', ifelse(sign1 > 0, 'left', 'on the line'))
    sign2 <- with(point_coordinates,
                  ( (x[coastr_id == ifelse(z==length(coastr_id_index), z, z + 1) & index == 0] - x[coastr_id == ifelse(z==length(coastr_id_index), z-1, z) & index == 0 ]) *
                      (y[coastr_id == z & index == 2] - y[coastr_id == ifelse(z==length(coastr_id_index), z-1, z) & index == 0 ]) -
                      (y[coastr_id == ifelse(z==length(coastr_id_index), z, z + 1) & index == 0 ] - y[coastr_id == ifelse(z==length(coastr_id_index), z-1, z) & index == 0 ]) *
                      (x[coastr_id == z & index == 2] - x[coastr_id == ifelse(z==length(coastr_id_index), z-1, z) & index == 0 ]) )
    )
    side2 <- ifelse(sign2 < 0, 'right', ifelse(sign2 > 0, 'left', 'on the line'))
    sides_df <- data.frame(coastr_id = rep(z, 3), index = c(0, 1, 2),
                        side = c('on the line', side1, side2))
    return(sides_df)
  })
  sides_rbind <- do.call(rbind, sides_list)
  sides <- base::merge(point_coordinates, sides_rbind)
  attr(sides, 'epsg') <- attributes(point_coordinates)$epsg
  class(sides) <- c('sides_df', class(point_coordinates))
  return(sides)
}

create_transect_sides <- function(sides_df, sides) {
  if(is.null(sides))
    stop('argument sides must be a valid character string')
  if(!any(sides == 'left', sides == 'right', sides == 'across'))
    stop('argument sides must be a character string of value
         "left", "right", or "across"')
  linestring_list <- lapply(unique(sides_df$coastr_id), function(i) {
    if(sides == 'across') {
      j <- 'right'
      k <- 'left'
    } else {
      j <- sides
      k <- 'on the line'
    }
    linestring_side_list <- lapply(j, function(j) {
      linestring_sfg <- st_linestring(
        with(
          sides_df[sides_df$coastr_id==i, ],
          rbind(c(x[side==j], y[side==j]), c(x[side==k], y[side==k]))
        )
      )
      linestring_sfc <- st_sfc(linestring_sfg)
      linestring_sf <- st_sf(
        data.frame(
          coastr_id = i,
          side = ifelse(sides=='across', 'across', j)),
        geometry = linestring_sfc)
      return(linestring_sf)
    })
    linestring_side <- do.call('rbind', linestring_side_list)
    return(linestring_side)
  })
  linestring <- do.call('rbind', linestring_list)
  st_crs(linestring) <- attributes(sides_df)$epsg
  return(linestring)
}
