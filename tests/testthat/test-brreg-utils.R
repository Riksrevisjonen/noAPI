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

test_that('is_valid_entity() works', {

  expect_true(is_valid_entity('801234569'))
  expect_true(is_valid_entity('987654325'))
  expect_true(is_valid_entity('974760843'))

  expect_false(is_valid_entity('87654321'))    # Too short
  expect_false(is_valid_entity('9876543210'))  # Too long
  expect_false(is_valid_entity('723456789'))   # Bad first digit
  expect_false(is_valid_entity('987654320'))   # Bad check control digit 1
  expect_false(is_valid_entity('987654321'))   # Bad check control digit 2
  expect_false(is_valid_entity('987654322'))   # Bad check control digit 3
  expect_false(is_valid_entity('987654323'))   # Bad check control digit 4
  expect_false(is_valid_entity('987654324'))   # Bad check control digit 5
  expect_false(is_valid_entity('987654326'))   # Bad check control digit 6
  expect_false(is_valid_entity('987654327'))   # Bad check control digit 7
  expect_false(is_valid_entity('987654328'))   # Bad check control digit 8
  expect_false(is_valid_entity('987654329'))   # Bad check control digit 9

})
