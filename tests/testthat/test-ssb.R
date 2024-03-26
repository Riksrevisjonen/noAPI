skip_if(Sys.getenv('NOAPI_SKIP_LIVE_API_TESTS') == 'TRUE')

test_that("request_ssb correctly returns a data frame", {
  skip_on_cran()

  ssb_url_test <- paste0(ssb_url, 'v0/dataset/1104.csv?lang=no')
  result <- request_ssb(ssb_url_test)
  expect_equal(class(result), 'data.frame')
})

test_that("get_ssb_table correctly returns a data frame", {
  skip_on_cran()

  result <- get_ssb_table(id = "1104")
  expect_equal(class(result), 'data.frame')
})

test_that("clean_column_names correctly cleans the column names", {
  skip_on_cran()

  df <- data.frame("Column 1" = 1:3, "ColUmn.2" = 4:6, check.names = FALSE)
  result <- clean_column_names(df)
  expect_equal(colnames(result), c("column_1", "column_2"))
})
