test_that('clean_entity_input() works', {
  # Names
  x <- 'ABC FORETAK 13'
  expect_identical(clean_entity_input('ABC FORETAK 13'), x)
  expect_identical(clean_entity_input(' ABC FORETAK 13 '), x)
  expect_identical(clean_entity_input('ABC  FORETAK 13'), x)
  expect_identical(clean_entity_input(' ABC-FORETAK!'), 'ABC-FORETAK!')
  # Numbers
  x <- '123456789'
  expect_identical(clean_entity_input('1234'), '1234')
  expect_identical(clean_entity_input('123456789'), x)
  expect_identical(clean_entity_input('123 456 789 '), x)
  expect_identical(clean_entity_input('NO123456789'), x)
  expect_identical(clean_entity_input('123456789 MVA'), x)
  expect_identical(clean_entity_input('123.456789$%'), x)
})

test_that('verify_entity_number() works', {
  expect_true(verify_entity_number('974760843')) # valid
  expect_true(verify_entity_number('971524960')) # valid
  expect_error(verify_entity_number('12345678')) # not 9-digits
  expect_error(verify_entity_number('123456789')) # not valid
  expect_error(verify_entity_number('972417806')) # not valid
})

test_that('mod11() works', {
  expect_true(mod11(974760843))
  expect_true(mod11(971524960))
  expect_true(mod11(972417807))
  expect_false(mod11(123456789))
  expect_false(mod11(974760841))
  expect_false(mod11(971524962))
  expect_false(mod11(972417806))
})
