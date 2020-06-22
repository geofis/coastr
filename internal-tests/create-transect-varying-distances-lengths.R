#Source multilinestring
test_shoreline_path <- system.file(
  'extdata/najayo-l8-shoreline-2013-04-29.geojson',
  package = 'coastr')
test_sf <- st_read(test_shoreline_path)
tdir <- tempdir()

#Transects placed at different separation distances. #Feature under development
#for the coastr package https://github.com/geofis/coastr
#Transectos localizados a diferentes distancias de separación. Función en
#desarrollo para el paquete coastr https://github.com/geofis/coastr
sapply(seq(1, 50, by=1), function(x) {
  t <- create_transect(obj = test_sf, distance = x, transect_length = 30)
  p <- t %>% ggplot + geom_sf(col='red') +
    geom_sf(data = test_sf) + coord_sf(datum = 32619) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_text(x=382610, y=2024000, label = paste0('sep = ', x, ' m'), size = 5) + theme_bw()
  jpeg(filename = paste0(tdir, '/sep_', ifelse(nchar(x)==1, paste0('0',x), x), '.jpg'))
  print(p)
  dev.off()
})
system(paste0('convert -delay 30 ', tdir, '/sep*.jpg ', tdir, '/separation.gif'))

#Transects of different lengths, separated by a fixed distance of 10 meters
#Transectos de diferentes longitudes, separados a una distancia fija de 10 metros
sapply(seq(5, 95, by=5), function(x) {
  t <- create_transect(obj = test_sf, distance = 10, transect_length = x)
  p <- t %>% ggplot + geom_sf(col='red') +
    geom_sf(data = test_sf) + coord_sf(datum = 32619) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_text(x=382800, y=2024000, label = paste0('transect length = ', x, ' m'), size = 5) + theme_bw()
  jpeg(filename = paste0(tdir, '/len_', ifelse(nchar(x)==1, paste0('0',x), x), '.jpg'))
  print(p)
  dev.off()
})
system(paste0('convert -delay 30 ', tdir, '/len*.jpg ', tdir, '/length.gif'))
