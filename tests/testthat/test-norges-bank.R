skip_if(Sys.getenv('NOAPI_SKIP_LIVE_API_TESTS') == 'TRUE')

test_that('get_exchange_rate() works for single currencies', {
  skip_on_cran()

  df <- get_exchange_rate('USD')
  expect_equal(class(df), 'data.frame')
  expect_equal(unique(df$currency), c('USD'))
  expect_equal(nrow(df), 10)
})

test_that('get_exchange_rate() works for multiple currencies', {
  skip_on_cran()

  df <- get_exchange_rate(c('USD', 'EUR'), n_obs = 5)
  expect_equal(class(df), 'data.frame')
  expect_equal(unique(df$currency), c('USD', 'EUR'))
  expect_equal(nrow(df), 10)
})

test_that('get_exchange_rate() returns expected frequencies', {
  skip_on_cran()

  df <- get_exchange_rate('USD', frequency = 'daily', n_obs = 1)
  expect_true(all(df$frequency == 'Business'))
  df <- get_exchange_rate('USD', frequency = 'monthly', n_obs = 1)
  expect_true(all(df$frequency == 'Monthly'))
  df <- get_exchange_rate('USD', frequency = 'annual', n_obs = 1)
  expect_true(all(df$frequency == 'Annual'))
})

test_that('get_exchange_rate() returns expected time periods', {
  skip_on_cran()

  df <- get_exchange_rate('USD', start = '2022-12-01', end = '2022-12-15')
  expect_true(min(df$date_from) == '2022-12-01')
  expect_true(max(df$date_from) == '2022-12-15')
  expect_equal(format(min(df$date_from), '%u'), '4') # Thursday
  expect_true(is.numeric(df$exch_rate))
})

test_that('get_exchange_rate() caps number of observations to 20', {
  skip_on_cran()

  expect_warning(df <- get_exchange_rate('ALL', n_obs = 21))
  expect_equal(class(df), 'data.frame')
  expect_equal(max(table(df$currency)), 20)
})

test_that('get_exchange_rate() works when simplify = FALSE', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  res <- suppressMessages(
    get_exchange_rate(
      c('AAA', 'EUR', 'USD'), frequency = 'monthly', simplify = FALSE)
  )
  expect_identical(class(res), 'list')
  expect_equal(length(res), 3L)
  expect_null(res[[1]])
  expect_gte(nrow(res[[2]]), 1)
  expect_gte(nrow(res[[3]]), 1)
})


test_that('get_exchange_rate() works when raw_response = TRUE', {
  skip_on_cran()

  res <- suppressMessages(get_exchange_rate('USD', raw_response = TRUE))
  expect_true(is.list(res))
  expect_true(is.list(res[[1]]))
  expect_equal(names(res[[1]]), c('url', 'status', 'content', 'response'))
  expect_identical(class(res[[1]]), 'noAPI')
  expect_true(is.list(res[[1]]$content))
  expect_invisible(get_exchange_rate('EUR', n_obs = 1, raw_response = TRUE))
})

test_that('get_exchange_rate() returns error if wrong currency is given', {
  expect_message(expect_null(get_exchange_rate('EURR'))) # internal error
  skip_on_cran()
  expect_message(expect_null(get_exchange_rate('ERR')))  # API error
})

test_that('get_exchange_rate_single() fails correctly', {
  expect_error(get_exchange_rate_single('EURR'))
  skip_on_cran()
  expect_error(get_exchange_rate_single('ERR'))
})
