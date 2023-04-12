skip_if(Sys.getenv('NOAPI_SKIP_LIVE_API_TESTS') == 'TRUE')

test_that('get_schools() works', {
  skip_on_cran()
  skip_if(check_api(nsr_url))

  # municipality
  df <- get_schools(1101)
  expect_identical(class(df), 'data.frame')
  expect_gte(nrow(df), 1)
  expect_equal(ncol(df), 16)

  # county
  df <- get_schools(11)
  expect_identical(class(df), 'data.frame')
  expect_gte(nrow(df), 1)
  expect_equal(ncol(df), 16)

  # check that 1 digit county codes work
  df <- get_schools(1)
  expect_identical(class(df), 'data.frame')
  expect_gte(nrow(df), 1)
  expect_equal(ncol(df), 16)

  # check that 3 digit municipality codes work
  df <- get_schools(301)
  expect_identical(class(df), 'data.frame')
  expect_gte(nrow(df), 1)
  expect_equal(ncol(df), 16)

  # all
  skip("get_schools's default method is too extensive to test every time")
  df <- get_schools('all')
  expect_identical(class(df), 'data.frame')
  expect_gte(nrow(df), 1)
  expect_equal(ncol(df), 16)

})

test_that('get_schools() works when simplify = FALSE', {
  skip_on_cran()
  skip_if(check_api(nsr_url))

  # municipality
  res <- suppressMessages(
    get_schools(c(9999, 1101, 4601), simplify = FALSE)
  )
  expect_identical(class(res), 'list')
  expect_equal(length(res), 3L)
  expect_null(res[[1]])
  expect_gte(nrow(res[[2]]), 1)
  expect_gte(nrow(res[[3]]), 1)

  # county
  res <- suppressMessages(
    get_schools(c(99, 11, 46), simplify = FALSE)
  )
  expect_identical(class(res), 'list')
  expect_equal(length(res), 3L)
  expect_null(res[[1]])
  expect_gte(nrow(res[[2]]), 1)
  expect_gte(nrow(res[[3]]), 1)

  # all
  skip("get_schools's default method is too extensive to test every time")
  res <- get_schools('all', simplify = FALSE)
  expect_identical(class(res), 'list')
  expect_equal(length(res), 1L)

})

test_that('get_schools() works when raw_response = TRUE', {
  skip_on_cran()
  skip_if(check_api(nsr_url))

  res <- suppressMessages(get_schools(1101, raw_response = TRUE))
  expect_true(is.list(res))
  expect_true(is.list(res[[1]]))
  expect_equal(names(res[[1]]), c('url', 'status', 'content', 'response'))
  expect_identical(class(res[[1]]), "noAPI")
  expect_true(is.list(res[[1]]$content))

})

test_that('get_schools() fails correctly', {
  # internal errors
  expect_error(get_schools(c(1101, 11)))
  expect_error(get_schools(c('all', 1101)))
  expect_error(get_schools(c(1101, 'all')))
  # expect_message(expect_null(get_schools(11)))
  # expect_message(expect_null(get_schools(1101)))
})

test_that('get_schools_single() works correctly', {
  skip_on_cran()
  skip_if(check_api(nsr_url))

  # Regular response
  df <- get_schools_single(type = 'kommune', unit = 1101)
  expect_identical(class(df), 'data.frame')
  # Raw response
  res <- get_schools_single(type = 'kommune', unit = 1101, raw_response = TRUE)
  expect_identical(class(res), 'noAPI')
})
