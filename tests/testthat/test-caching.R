library(callr)

test_that("Caching is enabled by default", {
  skip_if(Sys.getenv('NOAPI_DISABLE_CACHING') == 'TRUE')
  # Setup external R session
  r <- callr::r_session$new(options = callr::r_session_options(user_profile = FALSE))
  r$run(function() library(noAPI))
  # Check that main functions are cached
  tmp <- r$run(function() memoise::is.memoised(get_entity))
  expect_true(tmp)
  tmp <- r$run(function() memoise::is.memoised(get_municipalities))
  expect_true(tmp)
  tmp <- r$run(function() memoise::is.memoised(get_roles))
  expect_true(tmp)
  r$kill()
})

test_that("Caching can be disabled", {
  # Setup external R session
  r <- callr::r_session$new(options = callr::r_session_options(user_profile = FALSE))
  r$run(function() Sys.setenv("NOAPI_DISABLE_CACHING" = "TRUE"))
  r$run(function() library(noAPI))
  # Check that main functions are NOT cached
  tmp <- r$run(function() memoise::is.memoised(get_entity))
  expect_false(tmp)
  tmp <- r$run(function() memoise::is.memoised(get_municipalities))
  expect_false(tmp)
  tmp <- r$run(function() memoise::is.memoised(get_roles))
  expect_false(tmp)
  r$kill()
})
