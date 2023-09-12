testthat::test_that("Execute Cohort Pathways", {
  testthat::skip_if(condition = skipCdmTests)
  
  testthat::expect_error(CohortPathways:::createIfNotExist(type = "file"))
  testthat::expect_error(CohortPathways:::createIfNotExist(type = "file", name = NULL))
  testthat::expect_error(CohortPathways:::createIfNotExist(type = "folder", name = 'xxx'))
  
})
