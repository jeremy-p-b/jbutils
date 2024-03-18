test_that("wilson_lower returns a number between zero and 1", {
  expect_vector(wilson_lower(15,80), ptype=double(), size=NULL)
})

test_that("wilson_upper returns a number between zero and 1", {
  expect_vector(wilson_upper(15,80), ptype=double(), size=NULL)
})
