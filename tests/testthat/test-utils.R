test_that('check_api() works', {
  expect_false(check_api('http://httpbin.org/status/200'))
  expect_true(check_api('http://httpbin.org/status/404'))
  expect_true(check_api('http://httpbin.org/status/500'))
})

test_that('send_query() retries on specific error codes', {
  # 400 (no retry)
  u <- request('http://httpbin.org/status/400')
  tmp1 <- bench::system_time(send_query(u, max_tries = 3, throttle_rate = 1))
  # 429 (should retry)
  u <- request('http://httpbin.org/status/429')
  tmp2 <- bench::system_time(send_query(u, max_tries = 3, throttle_rate = 1))
  expect_gte(tmp2[2], tmp1[2])
  # 503 (should retry)
  u <- request('http://httpbin.org/status/503')
  tmp3 <- bench::system_time(send_query(u, max_tries = 3, throttle_rate = 1))
  expect_gte(tmp3[2], tmp1[2])
})

test_that('parse_response() works for JSON-formats', {
  u <- 'https://httpbin.org/json'
  resp <- request(u) |> req_perform()
  # simplifyVector = TRUE
  parsed <- parse_response(resp, simplifyVector = TRUE)
  expect_identical(parsed$slideshow$author, 'Yours Truly')
  expect_identical(class(parsed$slideshow$slides), 'data.frame')
  # simplifyVector = FALSE
  parsed <- parse_response(resp, simplifyVector = FALSE)
  expect_identical(parsed$slideshow$author, 'Yours Truly')
  expect_identical(class(parsed$slideshow$slides), 'list')
})

test_that('parse_response() returns error for unknown formats', {
  u <- 'https://httpbin.org/html'
  resp <- request(u) |> req_perform()
  expect_error(parse_response(resp))
})

test_that('make_api_object() works', {
  u <- 'https://httpbin.org/json'
  resp <- request(u) |> req_perform()
  parsed <- parse_response(resp, simplifyVector = TRUE)
  res <- make_api_object(resp, parsed)
  expect_identical(names(res), c('url', 'status', 'content', 'response'))
  expect_identical(class(res), "noAPI")
  expect_true(is.list(res$content))
  expect_identical(res$response, resp)
  expect_identical(res$content, parsed)
})
