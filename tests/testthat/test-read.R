context('Reading pdf')
library(litreadr)

testthat::test_that(
  "read_pdf returns a list",
  {
    testthat::expect_true(is.list(read_pdf(system.file("extdata", "Bladon_et_al_2020.pdf"))))
  }
)
