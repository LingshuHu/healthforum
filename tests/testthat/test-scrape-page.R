test_that("scrape_one_post works", {
  gas <- scrape_one_post(
    url = "https://patient.info/forums/discuss/can-gastritis-be-cured--613999",
    From = 1, To = 2)
  expect_true(is.data.frame(gas))
  expect_gt(ncol(gas), 4)
  expect_gt(nrow(gas), 4)
})
