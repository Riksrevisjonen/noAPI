skip_if(Sys.getenv('NOAPI_SKIP_LIVE_API_TESTS') == 'TRUE')

# Set globals
entity_resp <- readRDS('../testdata/brreg-entity-resp.rds')
roles_resp <- readRDS('../testdata/brreg-roles-resp.rds')

# ---- get_entity() ----

test_that('get_entity() works for single id queries', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  # --- Numeric input ---
  df <- get_entity(974760843)
  # Check type
  expect_equal(class(df), 'data.frame')
  # Check rows
  expect_equal(nrow(df), 1)
  expect_true(df$navn == 'RIKSREVISJONEN')
  # Check column names
  expect_true(all(names(df) == names(entity_resp)))
  # Check column classes
  expect_true(all(sapply(df, class) == sapply(entity_resp, class)))

  # --- Character input ---
  df <- get_entity('974760843')
  # Check rows
  expect_equal(nrow(df), 1)
  expect_true(df$navn == 'RIKSREVISJONEN')
  # Check column names
  expect_true(all(names(df) == names(entity_resp)))
  # Check column classes
  expect_true(all(sapply(df, class) == sapply(entity_resp, class)))

  # --- Special check for handling of addresses ---
  df <- get_entity(989668137)
  expect_equal(nrow(df), 1)
  df <- get_entity(971022264)
  expect_equal(nrow(df), 1)
  df <- get_entity(985619433)
  expect_equal(nrow(df), 1)
  df <- get_entity(995469804) # No business address
  expect_equal(nrow(df), 1)
  df <- get_entity('928 187 896') # NUF, no postal code
  expect_equal(nrow(df), 1)

})

test_that('get_entity() works for sub-entities', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  # numbers
  df1 <- get_entity(999178197)
  df2 <- get_entity(999178197, type = 'sub')
  expect_equal(df1, df2)
  expect_message(expect_null(get_entity(999178197, type = 'main')))

  # names
  df1 <- get_entity('KUBEN YRKESARENA')
  expect_false('KUBEN YRKESARENA' %in% df1$navn)
  df2 <- get_entity('KUBEN YRKESARENA', type = 'main')
  expect_equal(df1, df2)
  df3 <- get_entity('KUBEN YRKESARENA', type = 'sub')
  expect_true('KUBEN YRKESARENA' %in% df3$navn)

})

test_that('get_entity() works for multitiple id queries', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  df <- get_entity(c(974760843, 971524960))
  # Check rows
  expect_equal(nrow(df), 2)
  expect_true(all(df$navn == c('RIKSREVISJONEN', 'STORTINGET')))
  # Check column names
  expect_true(all(names(df) == names(entity_resp)))
  # Check column classes
  expect_true(all(sapply(df, class) == sapply(entity_resp, class)))
})

test_that('get_entity() works for single names queries', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  df <- get_entity('Riksrevisjonen')
  # Check rows
  expect_gte(nrow(df), 1)
  expect_true(all(grepl('RIKSREVISJONEN', df$navn)))
  # Check column names
  expect_true(all(names(df) == names(entity_resp)))
  # Check column classes
  expect_true(all(sapply(df, class) == sapply(entity_resp, class)))
})

test_that('get_entity() works for multitiple name queries', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  df <- get_entity(c('Riksrevisjonen', 'Stortinget'))
  # Check rows
  expect_gte(nrow(df), 2)
  expect_true(all(grepl('RIKSREVISJONEN|STORTINGET', df$navn)))
  # Check column names
  expect_true(all(names(df) == names(entity_resp)))
  # Check column classes
  expect_true(all(sapply(df, class) == sapply(entity_resp, class)))
})

test_that('get_entity() works when simplify = FALSE', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  res <- suppressMessages(
    get_entity(c(999999999, 974760843, 971524960), simplify = FALSE)
  )
  expect_identical(class(res), 'list')
  expect_equal(length(res), 3L)
  expect_null(res[[1]])
  expect_gte(nrow(res[[2]]), 1)
  expect_gte(nrow(res[[3]]), 1)
})

test_that('get_entity() works when raw_response = TRUE', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  #expect_message(get_entity(974760843, raw_response = TRUE))
  res <- suppressMessages(
    expect_invisible(
      get_entity(974760843, raw_response = TRUE)))
  expect_true(is.list(res))
  expect_true(is.list(res[[1]]))
  expect_equal(names(res[[1]]), c('url', 'status', 'content', 'response'))
  expect_identical(class(res[[1]]), "noAPI")
  expect_true(is.list(res[[1]]$content))
})

test_that('get_entity() returns NULL on failures', {
  # Not valid 9-digit number (internal error)
  expect_null(suppressMessages(get_entity(99999)))
  expect_null(suppressMessages(get_entity(12345678)))
  expect_null(suppressMessages(get_entity(123456789)))
  # Name not found
  skip_on_cran()
  skip_if(check_api(brreg_url))
  expect_warning(expect_null(get_entity('QW124')))
})

# ---- get_roles() ----

test_that('get_roles() works for single queries', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  # Check structure
  df <- get_roles(974760843)
  expect_equal(class(df), 'data.frame')
  # Check rows
  expect_gte(nrow(df), 1)
  # Check column names
  expect_true(all(names(df) == names(roles_resp)))
  # Check column classes
  expect_true(all(sapply(df, class) == sapply(roles_resp, class)))#

})

test_that('get_roles() works for mulitiple queries', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  # Check structure
  df <- get_roles(c(974760843, 971524960))
  expect_equal(class(df), 'data.frame')
  # Check rows
  expect_gte(nrow(df), 1)
  # Check column names
  expect_true(all(names(df) == names(roles_resp)))
  # Check column classes
  expect_true(all(sapply(df, class) == sapply(roles_resp, class)))

})

test_that('get_roles() works when simplify = FALSE', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  res <- suppressMessages(
    get_roles(c(999999999, 974760843, 971524960), simplify = FALSE)
  )
  expect_identical(class(res), 'list')
  expect_equal(length(res), 3L)
  expect_null(res[[1]])
  expect_gte(nrow(res[[2]]), 1)
  expect_gte(nrow(res[[3]]), 1)
})

test_that('get_roles() works when raw_response = TRUE', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  # expect_message(get_roles(974760843, raw_response = TRUE))
  res <- suppressMessages(
    expect_invisible(
      get_roles(974760843, raw_response = TRUE))
  )
  expect_true(is.list(res))
  expect_true(is.list(res[[1]]))
  expect_equal(names(res[[1]]), c('url', 'status', 'content', 'response'))
  expect_identical(class(res[[1]]), "noAPI")
  expect_true(is.list(res[[1]]$content))
})

test_that('get_roles() returns NULL on failures', {
  # Not valid 9-digit number (internal error)
  expect_null(suppressMessages(get_roles(99999)))
  expect_null(suppressMessages(get_roles(12345678)))
  expect_null(suppressMessages(get_roles(123456789)))
})

# ---- get_brreg_single() ----

test_that('get_brreg_single() fails correctly', {
  # Not valid 9-digit number (internal error)
  expect_error(get_brreg_single(99999, brreg_type = 'enheter'))
  expect_error(get_brreg_single(12345678, brreg_type = 'enheter'))
  expect_error(get_brreg_single(123456789, brreg_type = 'enheter'))

  # Not valid 9-digit number (internal error)
  expect_error(get_brreg_single(99999, brreg_type = 'roller'))
  expect_error(get_brreg_single(12345678, brreg_type = 'roller'))
  expect_error(get_brreg_single(123456789, brreg_type = 'roller'))

  # Name not found (internal warning)
  skip_on_cran()
  skip_if(check_api(brreg_url))
  expect_warning(get_brreg_single('QW124', brreg_type = 'enheter'))
})

test_that('get_brreg_single() works when raw_response = TRUE', {
  skip_on_cran()
  skip_if(check_api(brreg_url))

  # Entities
  res <- get_brreg_single(974760843, brreg_type = 'enheter', type = 'both', raw_response = TRUE)
  expect_true(is.list(res))
  expect_equal(names(res), c('url', 'status', 'content', 'response'))
  expect_identical(class(res), "noAPI")
  expect_true(is.list(res$content))

  # Roles
  res <- get_brreg_single(974760843, brreg_type = 'roller', raw_response = TRUE)
  expect_true(is.list(res))
  expect_equal(names(res), c('url', 'status', 'content', 'response'))
  expect_identical(class(res), "noAPI")
  expect_true(is.list(res$content))

})
