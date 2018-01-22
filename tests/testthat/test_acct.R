# Test 1: Check that outputs match what we expect

x <- c(100, -100, NA)
# format_acct(x) should return $100.00, $(100.00), NA

test_that("format_acct returns expected output", {
  expect_error(format_acct('test'))
  expect_true(any(is.na(format_acct(x))))
  expect_true(identical(format_acct(x[1]), "$100.00"))
  expect_true(identical(format_acct(x[2]), "$(100.00)"))
})
