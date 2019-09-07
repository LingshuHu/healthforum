test_that("scrape_one_post works", {
  skip_on_cran()
  skip_on_ci()

  gas <- scrape_one_post(
    url = "https://patient.info/forums/discuss/how-safe-is-a-colonoscopy--676942",
    From = 1, To = 2)
  expect_true(is.data.frame(gas))
  expect_gt(ncol(gas), 4)
  expect_gt(nrow(gas), 4)
  expect_error(
    scrape_one_post()
  )
  expect_error(
    scrape_one_post("https://patient.info/doctor/bronchiolitis-pro", 1, 10)
  )
})


test_that("scrape_one_group works", {
  skip_on_cran()
  skip_on_ci()

  gas <- scrape_one_group(
    group_url = "https://patient.info/forums/discuss/browse/angiotensin-ii-receptor-blockers-3037",
    random_post_number = 10)
  expect_true(is.data.frame(gas))
  expect_gt(ncol(gas), 4)
  expect_gt(nrow(gas), 4)
  expect_error(
    scrape_one_group()
  )
  expect_error(
    scrape_one_group("https://patient.info/doctor/bronchiolitis-pro", 10)
  )
})


test_that("scrape_groups_by_initial_letter works", {
  skip_on_cran()
  skip_on_ci()

  gas <- scrape_groups_by_initial_letter(
    index = "x",
    post_number_per_group = 1)
  expect_true(is.data.frame(gas))
  expect_gt(ncol(gas), 4)
  expect_gt(nrow(gas), 4)
  expect_error(
    scrape_groups_by_initial_letter()
  )
  expect_error(
    scrape_groups_by_initial_letter(index = 1, 10)
  )
})


test_that("scrape_groups_by_category works", {
  skip_on_cran()
  skip_on_ci()

  gas <- scrape_groups_by_category(
    cat = "https://patient.info/forums/categories/health-promotion-17",
    post_number_per_group = 2)
  expect_true(is.data.frame(gas))
  expect_gt(ncol(gas), 4)
  expect_gt(nrow(gas), 4)
  expect_error(
    scrape_groups_by_category()
  )
  expect_error(
    scrape_groups_by_category("https://patient.info/doctor/bronchiolitis-pro", 10)
  )
})



