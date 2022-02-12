context("figure_1 works as intended")

x <- phase_date_data(regreg)

test_that("figure_1 runs without errors", {
  expect_silent(figure_1(x))
})

test_that("figure_1 outputs a gg, ggplot class", {
  expect_equal(class(figure_1(x)), c("gg", "ggplot"))

})
