test_that('handle_crs() works', {

  # one input
  res <- handle_crs(4258)
  expect_identical(names(res), c('crs_in', 'crs_out'))
  expect_equal(res[[1]], 4258)
  expect_equal(res[[1]], res[[2]])

  res <- handle_crs('4258')
  expect_identical(names(res), c('crs_in', 'crs_out'))
  expect_equal(res[[1]], '4258')
  expect_equal(res[[1]], res[[2]])

  expect_error(handle_crs('EPSG:4326'))

  # two inputs
  res <- handle_crs(c(4258, 4326))
  expect_identical(names(res), c('crs_in', 'crs_out'))
  expect_equal(res[[1]], 4258)
  expect_equal(res[[2]], 4326)

  res <- handle_crs(c('4258', '4326'))
  expect_identical(names(res), c('crs_in', 'crs_out'))
  expect_equal(res[[1]], '4258')
  expect_equal(res[[2]], '4326')

  expect_error(handle_crs(c('EPSG:4326', '4326')))
  expect_error(handle_crs(c('4326', 'EPSG:4326')))

})


