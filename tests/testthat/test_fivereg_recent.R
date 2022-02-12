context("fivereg_recent works as intended")

x <- phase_date_data(regreg)

mtcars_df_slot <- mtcars
mtcars_df_slot$df <- mtcars

# checked manually using Excel
manual_fivereg <- "allergen, statistical-geography-unitary-authority-eng,
                    statistical-geography-non-metropolitan-district-eng,
                    statistical-geography-registration-district-wls,
                    statistical-geography-registration-district-eng"

test_that(
  "fivereg_recent runs without errors",
  {
    expect_silent(fivereg_recent(x))
  }
)

#
devtools::test()
#

test_that("fivereg_recent errors if input is not suitable dataframe", {
  expect_error(fivereg_recent(15))
  expect_error(fivereg_recent(mtcars_df_slot))

})

#
devtools::test()
#


test_that("fivereg_recent equals manual sorting", {
  expect_equal(fivereg_recent(x), manual_fivereg)

})

test_that("fivereg_recent with n; correct number of commas", {
  expect_equal(stringr::str_count(fivereg_recent(x, 10),
                                  ','),  # we didn't fix the last comma being
               # an and
               9)

})

#
devtools::test()
#
