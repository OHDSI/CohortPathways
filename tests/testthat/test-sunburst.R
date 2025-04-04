test_that("Sunburst works", {
  cpResults <- list()
  cpResults$pathwayAnalysisStatsData <- data.frame(1)
  cpResults$pathwaysAnalysisEventsData <- data.frame(1)
  cpResults$pathwaycomboIds <- data.frame(1)
  cpResults$pathwayAnalysisCodesLong <- data.frame(
    pathwayAnalysisGenerationId = c(20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342, 20250404342),
    code = c(2, 6, 4, 6, 20, 52, 8, 24, 56, 16, 20, 24, 48, 52, 56, 32, 48, 52, 56),
    targetCohortId = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
    eventCohortId = c(1, 1, 2, 2, 2, 2, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7),
    isCombo = c(0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1),
    numberOfEvents = c(1, 2, 1, 2, 2, 3, 1, 2, 3, 1, 2, 2, 2, 3, 3, 1, 2, 3, 3)
  )
  cpResults$pathwayAnalysisCodesData <- data.frame(1)
  cpResults$pathwaysAnalysisPathsData <- data.frame(
    pathwayAnalysisGenerationId = c(20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730, 20250404730),
    targetCohortId = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
    step1 = c(16, 16, 4, 32, 16, 16, 48, 48, 56, 16, 48, 16, 48, 4, 48, 24, 8, 52, 4, 2, 4, 48, 16, 8, 48, 8, 20, 2, 32, 48, 8, 20),
    step2 = c(4, NA, 16, NA, 8, 2, 32, 20, NA, 56, 4, 6, NA, 16, 16, NA, NA, NA, 48, 48, NA, 8, 48, 48, 2, 48, 16, NA, 48, 16, 16, NA),
    step3 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 48, 4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 16, NA, NA, NA, NA, NA, NA),
    step4 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    step5 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    step6 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    step7 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    step8 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    step9 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    step10 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    countValue = c(3, 3013, 1, 5, 2, 1, 1, 1, 8, 2, 3, 1, 6640, 1, 1, 6, 153, 1, 2, 1, 44, 12, 208, 7, 5, 1, 1, 35, 2, 231, 5, 1)
  )
  
  cpResults$isCombo <- data.frame(
    targetCohortId = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
    comboId = c(2, 4, 6, 8, 16, 20, 24, 32, 48, 52, 56),
    numberOfEvents = c(1, 1, 2, 1, 1, 2, 2, 1, 2, 3, 3),
    isCombo = c(0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1)
  )
  generationSet <- data.frame(
    cohortId = c(1, 2, 3, 4, 5, 6, 7),
    cohortName = c("drug1", "drug2", "drug3", "target", "drug4", "drug5", "drug6")
  )
  plot <- CohortPathways::createPathwaySunburst(
    cpResults,
    generationSet
  )
  expect_true("data" %in% names(plot[[1]]$x))
  expect_true("sunburst" %in% class(plot[[1]]))
})
