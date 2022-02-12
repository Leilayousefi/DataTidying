context("figure_2 works as intended")

x <- phase_date_data(regreg)

test_that("figure_2 runs without errors", {
  expect_silent(figure_2(x))
})

test_that("figure_2 outputs a gg, ggplot class", {
  expect_equal(class(figure_2(x)), c("gg", "ggplot"))

})
