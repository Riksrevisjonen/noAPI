skip_if(Sys.getenv('NOAPI_SKIP_LIVE_API_TESTS') == 'TRUE')

# Set globals
kv_url2 <- paste0(kv_url, 'v1/#/default/get_punktsok')

# ---- get_address_info ----

test_that('get_address_info() works for single inputs', {
  skip_on_cran()
  skip_if(check_api(kv_url2))

  # Regular search
  df <- get_address_info('munkegata 1 trondheim')
  expect_identical(class(df), 'data.frame')
  expect_gte(nrow(df), 1)
  expect_equal(ncol(df), 23)

  # Advanced parameters
  df <- get_address_info(
    address_name = 'Munkegata', address_number = 1,
    mun_name = 'Trondheim')
  expect_identical(class(df), 'data.frame')
  expect_equal(nrow(df), 1)
  expect_equal(ncol(df), 23)

})

test_that('get_address_info() works for multiple inputs', {
  skip_on_cran()
  skip_if(check_api(kv_url2))

  # Regular search
  df <- get_address_info(c('munkegata 1 trondheim', 'tromsø gata'))
  expect_identical(class(df), 'data.frame')
  expect_gte(nrow(df), 2)
  expect_equal(ncol(df), 23)

  # Advanced parameters
  df <- get_address_info(
    search = c('munkegata 1 trondheim', 'storgata'),
    mun_code = list(NULL, '0301'))
  expect_identical(class(df), 'data.frame')
  expect_gte(nrow(df), 2)
  expect_equal(ncol(df), 23)
})

test_that('get_address_info() works when simplify = FALSE', {
  skip_on_cran()
  skip_if(check_api(kv_url2))

  # Regular search
  res <- suppressWarnings(
    get_address_info(
      c('andeby 1', 'munkegata 1 trondheim', 'tromsø gata'),
      simplify = FALSE)
  )
  expect_identical(class(res), 'list')
  expect_equal(length(res), 3L)
  expect_null(res[[1]])
  expect_gte(nrow(res[[2]]), 1)
  expect_gte(nrow(res[[3]]), 1)

})

test_that('get_address_info() works when raw_response = TRUE', {
  skip_on_cran()
  skip_if(check_api(kv_url2))

  res <- suppressMessages(
    expect_invisible(
      get_address_info('munkegata 1 trondheim', raw_response = TRUE))
  )
  expect_true(is.list(res))
  expect_true(is.list(res[[1]]))
  expect_equal(names(res[[1]]), c('url', 'status', 'content', 'response'))
  expect_identical(class(res[[1]]), "noAPI")
  expect_true(is.list(res[[1]]$content))
})

test_that('get_address_info() returns NULL on failures', {
  skip_on_cran()
  # No address found (warning + NULL)
  expect_warning(expect_null(get_address_info('munkegata 1 tronheim')))
})

# ---- find_address_from_point ----

test_that('find_address_from_point() works for vector inputs', {
  skip_on_cran()
  skip_if(check_api(kv_url2))

  # all addresses
  df <- find_address_from_point(c(lat = 59.91364, lon = 10.7508))
  expect_identical(class(df), 'data.frame')
  expect_gte(nrow(df), 1)
  expect_equal(ncol(df), 24)

  # only the closest address for each point
  df <- find_address_from_point(c(lat = 59.91364, lon = 10.7508), closest = TRUE)
  expect_identical(class(df), 'data.frame')
  expect_equal(nrow(df), 1)
  expect_equal(ncol(df), 24)

})

test_that('find_address_from_point() works for list inputs', {
  skip_on_cran()
  skip_if(check_api(kv_url2))

  # all addresses
  df <- find_address_from_point(
    list(c(lat = 59.91364, lon = 10.7508),
         c(lat = 59.91364, lon = 10.7508)))
  expect_identical(class(df), 'data.frame')

  # only the closest address for each point
  df <- find_address_from_point(
    list(c(lat = 59.91364, lon = 10.7508),
         c(lat = 59.91364, lon = 10.7508)),
    closest = TRUE)
  expect_equal(nrow(df), 2)

  # only the closest address for each point (with )
  res <- find_address_from_point(
    list(c(lat = 59.91364, lon = 10.7508),
         c(lat = 59.91364, lon = 10.7508)),
    simplify = FALSE,
    closest = TRUE)
  expect_equal(nrow(res[[1]]), 1)
  expect_equal(nrow(res[[2]]), 1)

})

test_that('find_address_from_point() works when simplify = FALSE', {
  skip_on_cran()
  skip_if(check_api(kv_url2))

  # all addresses
  res <- suppressMessages(
    find_address_from_point(
      list(
        c(lat = 10.7508, lon = 59.91364),
        c(lat = 59.91364, lon = 10.7508),
        c(lat = 59.91364, lon = 10.7508)),
      simplify = FALSE))
  expect_identical(class(res), 'list')
  expect_equal(length(res), 3L)
  expect_null(res[[1]])
  expect_gte(nrow(res[[2]]), 1)
  expect_gte(nrow(res[[3]]), 1)

  # only the closest address for each point
  res <- suppressMessages(
    find_address_from_point(
      list(
        c(lat = 10.7508, lon = 59.91364),
        c(lat = 59.91364, lon = 10.7508),
        c(lat = 59.91364, lon = 10.7508)),
      simplify = FALSE, closest = TRUE)
  )
  expect_identical(class(res), 'list')
  expect_equal(length(res), 3L)
  expect_null(res[[1]])
  expect_equal(nrow(res[[2]]), 1)
  expect_equal(nrow(res[[3]]), 1)

})

test_that('find_address_from_point() works when raw_response = TRUE', {
  skip_on_cran()
  skip_if(check_api(kv_url2))

  # expect_message(
  #   find_address_from_point(c(lat = 59.91364, lon = 10.7508),
  #                           raw_response = TRUE))
  res <- suppressMessages(
    expect_invisible(
      find_address_from_point(c(lat = 59.91364, lon = 10.7508),
                              raw_response = TRUE))
  )
  expect_true(is.list(res))
  expect_equal(names(res), c('url', 'status', 'content', 'response'))
  expect_identical(class(res), "noAPI")
  expect_true(is.list(res$content))
})

test_that('find_address_from_point() returns NULL on failures', {
  skip_on_cran()
  # Not valid input coordinates
  expect_message(
    expect_null(
      find_address_from_point(c(lon = 59.91364, lat = 10.7508))
    )
  )
})

# ---- get_address_info_single ----

test_that('get_address_info_single() works', {
  skip_on_cran()
  skip_if(check_api(kv_url2))

  # Regular search
  res <- get_address_info_single('munkegata 1 trondheim')
  expect_identical(class(res), 'data.frame')

  # Advanced parameters
  res <- get_address_info_single(
    address_name = 'Munkegata', address_number = 1,
    mun_name = 'Trondheim')
  expect_identical(class(res), 'data.frame')

})

test_that('get_address_info_single() fails correctly', {
  skip_on_cran()
  # No address found (warning)
  expect_warning(
    expect_null(
      get_address_info_single('munkegata 1 tronheim')
    )
  )
})

test_that('get_address_info_single() works when mulitiple pages are found', {
  skip_on_cran()
  skip_on_ci() # Test fails on GH Actions (not sure why)

  res1 <- rlang::with_interactive({
    suppressWarnings(
      get_address_info_single(
        search = 'munkegata trondheim',
        crs = 4258, size = 25)
    )
  }, value = FALSE)
  expect_equal(nrow(res1), 25)

  res2 <- suppressMessages(
    rlang::with_interactive({
      get_address_info_single(
        search = 'munkegata trondheim',
        crs = 4258, size = 25)
    }, value = TRUE)
  )
  expect_gt(nrow(res2), nrow(res1))

})

# ---- find_address_from_point_single ----

test_that('find_address_from_point_single() works for vector inputs', {
  skip_on_cran()
  skip_if(check_api(kv_url2))

  res <- find_address_from_point_single(
    c(lat = 59.91364, lon = 10.7508), crs = 4258, radius = 1,
    closest = FALSE)
  expect_identical(class(res), 'data.frame')

  res <- find_address_from_point_single(
    c(59.91364, 10.7508), crs = 4258, radius = 1,
    closest = FALSE)
  expect_identical(class(res), 'data.frame')
})

test_that('find_address_from_point_single() fails correctly', {
  # 'x' not correct length (vector)
  expect_error(find_address_from_point_single(
    c(lat = 59.91), crs = 4258, radius = 1, closest = FALSE))

  skip_on_cran()

  # Not valid input coordinates
  expect_error(find_address_from_point_single(
    c(lon = 59.91364, lat = 10.7508), crs = 4258, radius = 1,
    closest = FALSE))
  # No address found
  expect_warning(find_address_from_point_single(
    c(lat = 59.91, lon = 10.8), crs = 4258, radius = 1,
    closest = FALSE))
})

test_that('find_address_from_point_single() works when mulitiple pages are found', {
  skip_on_cran()
  skip_on_ci() # Test fails on GH Actions (not sure why)

  res1 <- rlang::with_interactive({
    suppressWarnings(
      find_address_from_point_single(
        c(lat = 59.91364, lon = 10.7508), radius = 100,
        crs = 4258, size = 10,
        closest = FALSE))
  }, value = FALSE)
  expect_equal(nrow(res1), 10)

  res2 <- suppressMessages(
    rlang::with_interactive({
      find_address_from_point_single(
        c(lat = 59.91364, lon = 10.7508), radius = 100,
        crs = 4258, size = 10,
        closest = FALSE)
    }, value = TRUE)
  )
  expect_gt(nrow(res2), nrow(res1))

})
