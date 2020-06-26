#' Create transects
#'
#' Create transects of user-defined length along an input line
#'
#' @param x name of an object of class \code{sf}, or a path to a file
#'   readable by the \code{sf::st_read} function, which must contain
#'   geometries of type \code{LINESTRING} or \code{MULTILINESTRING}.
#' @param transect_spacing numeric; spacing between transects
#' @param length_left numeric; length of transects to the left of the input line
#' @param length_left numeric; length of transects to the right of the input line
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
create_transect <- function(x, transect_spacing = NULL,
                            length_left = if(is.null(transect_spacing))
                              NULL else transect_spacing,
                            length_right = if(is.null(transect_spacing))
                              NULL else transect_spacing, ...) {
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
  point <- create_point(obj, transect_spacing)
  coords <- calculate_xy_coords(point)
  lag <- calculate_lag(coords)
  angle <- calculate_mean_angle(lag)
  point_coordinates <- create_point_coordinates(angle, length_left, length_right)
  point_sides <- find_side_of_points(point_coordinates)
  transects_sides <- create_transect_sides(point_sides, sides)
  return(transects_sides)
}

create_point <- function(obj, transect_spacing = transect_spacing) {
  if(is.null(transect_spacing)) {
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

create_multilinestring <- function(point_coordinates, length_left, length_right) {
  if(all(!is.null(length_left), !is.null(length_right))) {
    linestring_list <- lapply(1:nrow(point_coordinates), function(h) {
      linestring_sfg <- st_linestring(
        with(
          point_coordinates[h, ],
          rbind(c(x_left, y_left), c(x_right, y_right))
        )
      )
      linestring_sfc <- st_sfc(linestring_sfg)
      linestring_sf <- st_sf(
        data.frame(coastr_id = h, type = 'across'),
        geometry = linestring_sfc)
      return(linestring_sf)
    })
    linestring <- do.call('rbind', linestring_list)
  } else {
    if(is.null(length_right)) {
      
    } else {
      if (is.null(length_left)) {
        
      }
    }
  }
  st_crs(linestring) <- attributes(point_coordinates)$epsg
  return(linestring)
}
#











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
