context('read transect')

file_path_line1 <- system.file('extdata/transects-long.geojson', package = 'coastr')
file_path_line2 <- system.file('extdata/transects-short.geojson', package = 'coastr')
file_path_test_pol <- system.file('inst/extdata/test-polygon.geojson', package = 'coastr')
transect <- read_transect(file_path_line1, start_from = 'N')
test_input_line <- invisible(st_read(file_path_line1, quiet = T))
test_input_pol <- invisible(st_read(file_path_test_pol, quiet = T))
increment_vector <- increment_from_id(transect, start_from = 'N')

test_that("transect file can be read and assigned the proper class", {
  expect_identical(class(transect), c('transect', 'sf', 'data.frame'))
})

test_that("coastr_id field exists and is factor in transect object", {
  expect_true(any(colnames(transect) %in% 'coastr_id'))
  expect_true(is.factor(transect$coastr_id))
})

test_that("check geometry actually checks geometries in input object", {
  expect_error(check_geometry_type(test_input_pol))
})

test_that("check geometry actually checks geometries in input object", {
  expect_null(check_geometry_type(test_input_line))
})

test_that("geometry length is equal to vector length of increments_from_id", {
  expect_equal(nrow(transect), length(increment_vector))
})
