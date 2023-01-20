test_that('handle_crs() works', {
  # one input
  res <- handle_crs(4258)
  expect_identical(names(res), c('crs_in', 'crs_out'))
  expect_equal(res[[1]], 4258)
  expect_equal(res[[1]], res[[2]])

  # two inputs
  res <- handle_crs(c(4258, 4326))
  expect_identical(names(res), c('crs_in', 'crs_out'))
  expect_equal(res[[1]], 4258)
  expect_equal(res[[2]], 4326)
})


