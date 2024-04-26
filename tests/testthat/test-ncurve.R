test_that("mu is correct in named list", {
  l <- MATH4753ZEML24::myncurve(mu = 10, sigma = 5, a = 6)
  ln <- as.numeric(l)
  expect_equal(ln[1], 10)
})

test_that("sigma is correct in named list", {
  l <- MATH4753ZEML24::myncurve(mu = 10, sigma = 5, a = 6)
  ln <- as.numeric(l)
  expect_equal(ln[2], 5)
})

test_that("a is correct in named list", {
  l <- MATH4753ZEML24::myncurve(mu = 10, sigma = 5, a = 6)
  ln <- as.numeric(l)
  area <-pnorm(6, 10, 5)
  arear <- round(area, 4)
  expect_equal(ln[3], arear)
})
