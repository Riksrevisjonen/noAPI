# ---- Test for warnings and errors ----
test_that('get_municipality() issue warnings and errors as expected', {
  expect_error(get_municipalities(year = 1776))
  expect_error(get_municipalities('abc'))
})

# Skip rest of tests if live API test are disabled
skip_if(Sys.getenv('NOAPI_SKIP_LIVE_API_TESTS') == 'TRUE')

# ---- get_municipality() ----
test_that('get_municipality() returns expected results', {
  skip_on_cran()
  skip_if(check_api(ssb_url))

  df <- get_municipalities()
  expect_equal(class(df), 'data.frame')
  expect_equal(colnames(df), c('year', 'code', 'name'))
  # Municipality codes should always be four letter codes with leading zero
  expect_true(min(nzchar(df$code)) == max(nzchar(df$code)))

  # --- Tests for list ---
  df <- get_municipalities(c(2019, 2022), simplify = FALSE)
  expect_true(is.list(df))

  # --- Tests for adding county ---
  df <- get_municipalities(add_county = TRUE)
  expect_true(is.data.frame(df))
  expect_true(all(c('county', 'county_name') %in% colnames(df)))

  expect_warning(df <- get_municipalities(add_county = TRUE, raw_response = TRUE))
  expect_true(is.data.frame(df))

  # --- Tests for adding notes ---
  df <- get_municipalities(include_notes = TRUE)
  expect_true(is.data.frame(df))
  expect_equal(colnames(df), c('year', 'code', 'name', 'notes'))

  # --- Tests for simplified data.frame ---
  df <- get_municipalities(c(2019, 2022), simplify = TRUE)
  expect_true(is.data.frame(df))
  expect_true(length(unique(df$year)) == 2)

  # --- Tests for raw response ---
  df <- get_municipalities(raw_response = TRUE)
  expect_true(is.list(df))
  expect_true(is.list(df))
  expect_true(is.list(df[[1]]))
  expect_equal(names(df[[1]]), c('url', 'status', 'content', 'response'))
  expect_identical(class(df[[1]]), "noAPI")
  expect_true(is.list(df[[1]]$content))
})

# ---- get_counties() ----
test_that('get_counties() returns expected results', {
  skip_on_cran()
  skip_if(check_api(ssb_url))

  df <- get_counties()
  expect_equal(class(df), 'data.frame')
  expect_equal(colnames(df), c('year', 'code', 'name'))
  # County codes should always be two letter codes with leading zero
  expect_true(min(nzchar(df$code)) == max(nzchar(df$code)))

  # --- Tests for raw response ---
  df <- get_counties(raw_response = TRUE)
  expect_true(is.list(df))
  expect_true(is.list(df))
  expect_true(is.list(df[[1]]))
  expect_equal(names(df[[1]]), c('url', 'status', 'content', 'response'))
  expect_identical(class(df[[1]]), "noAPI")
  expect_true(is.list(df[[1]]$content))
})

# ---- get_countries() ----
test_that('get_counties() returns expected results', {
  skip_on_cran()
  skip_if(check_api(ssb_url))

  df <- get_countries()
  expect_equal(class(df), 'data.frame')
  expect_equal(colnames(df), c('year', 'code', 'name'))
  # County codes should always be three letter code
  expect_true(all(nchar(df$code) == 3))

  # --- Tests for raw response ---
  df <- get_countries(raw_response = TRUE)
  expect_true(is.list(df))
  expect_true(is.list(df))
  expect_true(is.list(df[[1]]))
  expect_equal(names(df[[1]]), c('url', 'status', 'content', 'response'))
  expect_identical(class(df[[1]]), "noAPI")
  expect_true(is.list(df[[1]]$content))
})
