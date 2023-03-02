entity_resp <- readRDS('../testdata/brreg-entity-resp.rds')
roles_resp <- readRDS('../testdata/brreg-roles-resp.rds')

# ---- get_entity() ----

test_that('get_entity() works for single id queries', {
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

test_that('get_entity() works for multitiple id queries', {
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

test_that('get_entity() works when raw_response = TRUE', {
  skip_if(check_api(brreg_url))

  res <- expect_invisible(get_entity(974760843, raw_response = TRUE))
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
  # Name not found (internal warning)
  skip_if(check_api(brreg_url))
  expect_warning(expect_null(get_entity('QW124')))
})

# ---- get_roles() ----

test_that('get_roles() works for single queries', {
  skip_if(check_api(brreg_url))

  # Check structure
  res <- get_roles(974760843)
  expect_true(is.list(res))
  # expect_true(names(res) == '974760843')
  expect_equal(names(res), c('persons', 'entities'))
  # Check column names
  expect_true(all(names(res$persons) ==
                    names(roles_resp$persons)))
  expect_true(all(names(res$entities) ==
                    names(roles_resp$entities)))
  # Check column classes
  expect_true(all(sapply(res$persons, class) ==
                    sapply(roles_resp$persons, class)))
  expect_true(all(sapply(res$entities, class) ==
                    sapply(roles_resp$entities, class)))

})

test_that('get_roles() works for mulitiple queries', {
  skip_if(check_api(brreg_url))

  # Check structure
  res <- get_roles(c(974760843, 971524960))
  expect_true(is.list(res))
  # expect_true(all(names(res) == c('974760843', '971524960')))
  expect_equal(names(res), c('persons', 'entities'))

  # Check column names
  expect_true(all(names(res$persons) ==
                    names(roles_resp$persons)))
  expect_true(all(names(res$entities) ==
                    names(roles_resp$entities)))

  # Check column classes
  expect_true(all(sapply(res$persons, class) ==
                    sapply(roles_resp$persons, class)))
  expect_true(all(sapply(res$entities, class) ==
                    sapply(roles_resp$entities, class)))

})

test_that('get_roles() works when raw_response = TRUE', {
  skip_if(check_api(brreg_url))

  res <- expect_invisible(get_roles(974760843, raw_response = TRUE))

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


# ----- get_municipalities() ----

test_that('get_municipalities works', {
  skip_if(check_api(brreg_url))

  df <- get_municipalities()
  # Check type
  expect_equal(class(df), 'data.frame')
  # Check cols
  expect_equal(ncol(df), 2)
  expect_equal(names(df), c('nummer', 'navn'))
})

test_that('get_municipalities works when raw_response = TRUE', {
  skip_if(check_api(brreg_url))

  res <- expect_invisible(get_municipalities(raw_response = TRUE))
  expect_true(is.list(res))
  expect_equal(names(res), c('url', 'status', 'content', 'response'))
  expect_identical(class(res), "noAPI")
  expect_true(is.list(res$content))
})


# ---- get_brreg_single() ----

get_brreg_single(974760843)

get_brreg_single(974760843, 'roller')

test_that('get_brreg_single() fails correctly', {
  # Not valid 9-digit number (internal error)
  expect_error(get_brreg_single(99999, type = 'enheter'))
  expect_error(get_brreg_single(12345678, type = 'enheter'))
  expect_error(get_brreg_single(123456789, type = 'enheter'))
  # Name not found (internal warning)
  skip_if(check_api(brreg_url))
  expect_warning(get_brreg_single('QW124', type = 'enheter'))

  # Not valid 9-digit number (internal error)
  expect_error(get_brreg_single(99999, type = 'roller'))
  expect_error(get_brreg_single(12345678, type = 'roller'))
  expect_error(get_brreg_single(123456789, type = 'roller'))
})

test_that('get_brreg_single() works when raw_response = TRUE', {
  skip_if(check_api(brreg_url))

  # Entities
  res <- get_brreg_single(974760843, type = 'enheter', raw_response = TRUE)
  expect_true(is.list(res))
  expect_equal(names(res), c('url', 'status', 'content', 'response'))
  expect_identical(class(res), "noAPI")
  expect_true(is.list(res$content))

  # Roles
  res <- get_brreg_single(974760843, type = 'roller', raw_response = TRUE)
  expect_true(is.list(res))
  expect_equal(names(res), c('url', 'status', 'content', 'response'))
  expect_identical(class(res), "noAPI")
  expect_true(is.list(res$content))

})
