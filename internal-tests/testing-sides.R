library(sf)
library(magrittr)
library(ggplot2)
path <- 'Descargas/tstline.geojson'
test_line <- st_read(path)
test_line %>% plot

point <- create_point(test_line, transect_spacing = 1)
point %>% ggplot + geom_sf(col='red', size=4) + geom_sf(data = test_line)

coords <- calculate_xy_coords(point)
coords

lag <- calculate_lag(coords)
lag

angle <- calculate_mean_angle(lag)
angle



point %>% ggplot + geom_sf(col='red', size=4) + geom_sf(data = test_line)
  
new <- with(angle, {a <- x + cos(left_angle); b <- y + sin(left_angle); return(data.frame(a,b))}) %>% st_as_sf(coords = c('a','b'), crs = 32619)
new2 <- with(angle, {a <- x - cos(left_angle); b <- y - sin(left_angle); return(data.frame(a,b))}) %>% st_as_sf(coords = c('a','b'), crs = 32619)

point %>% ggplot + geom_sf(col='red', size=4) + geom_sf(data = test_line) + geom_sf(data=new, col = 'orange') + geom_sf(data=new2, col = 'blue')


point_coordinates <- create_point_coordinates(angle, 1, 1)
l <- create_multilinestring(point_coordinates, 1, 1)
l %>% ggplot() + geom_sf(col='red') + geom_sf(data=test_line)
