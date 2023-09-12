testthat::test_that("Execute Cohort Pathways", {
  testthat::skip_if(condition = skipCdmTests)

  cohortDefinitionSet <- dplyr::tibble(
    cohortId = c(1, 2, 10, 20),
    cohortName = c("A", "B", "C", "D")
  )

  # make up date for a cohort table
  targetCohort <- dplyr::tibble(
    cohortDefinitionId = c(1, 1, 2),
    subjectId = c(1, 1, 1),
    cohortStartDate = c(
      as.Date("1999-01-01"),
      as.Date("1999-02-20"),
      as.Date("1999-03-01")
    ),
    cohortEndDate = c(
      as.Date("1999-01-31"),
      as.Date("1999-02-28"),
      as.Date("1999-03-31")
    )
  )

  eventCohort <- dplyr::tibble(
    cohortDefinitionId = c(10, 10, 20),
    subjectId = c(1, 1, 1),
    cohortStartDate = c(
      as.Date("1999-01-01"),
      as.Date("1999-01-20"),
      as.Date("1999-04-10")
    ),
    cohortEndDate = c(
      as.Date("1999-01-10"),
      as.Date("1999-02-20"),
      as.Date("1999-04-20")
    )
  )

  # upload table
  connection <-
    DatabaseConnector::connect(connectionDetails = connectionDetails)
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = cohortDatabaseSchema,
    tableName = cohortTableName,
    data = dplyr::bind_rows(targetCohort, eventCohort) |> dplyr::distinct(),
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE,
    progressBar = FALSE
  )

  dataInserted <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = paste0(
        "SELECT * FROM @cohort_database_schema.@table_name
        order by cohort_definition_id, subject_id, cohort_start_date;"
      ),
      cohort_database_schema = cohortDatabaseSchema,
      table_name = cohortTableName,
      snakeCaseToCamelCase = TRUE
    ) |>
    dplyr::tibble()

  testthat::expect_equal(
    object = dataInserted |>
      nrow(),
    expected = nrow(dplyr::bind_rows(targetCohort, eventCohort) |> dplyr::distinct())
  )

  exportFolder <- tempfile()
  dir.create(path = exportFolder, showWarnings = FALSE, recursive = TRUE)

  CohortPathways::executeCohortPathways(
    connection = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableName = cohortTableName,
    targetCohortIds = c(1, 2),
    eventCohortIds = c(10, 20),
    cohortDefinitionSet = cohortDefinitionSet,
    exportFolder = exportFolder,
    overwrite = TRUE
  )

  testthat::expect_true(object = file.exists(
    file.path(exportFolder, "pathwayAnalysisCodes.csv")
  ))
  testthat::expect_true(object = file.exists(
    file.path(exportFolder, "pathwayAnalysisCodesLong.csv")
  ))
  testthat::expect_true(object = file.exists(
    file.path(exportFolder, "pathwayAnalysisStats.csv")
  ))
  testthat::expect_true(object = file.exists(
    file.path(exportFolder, "pathwaysAnalysisPaths.csv")
  ))

  pathwayAnalysisCodes <-
    readr::read_csv(
      file = file.path(exportFolder, "pathwayAnalysisCodes.csv"),
      col_types = readr::cols()
    ) |> SqlRender::snakeCaseToCamelCaseNames()
  testthat::expect_true(object = nrow(pathwayAnalysisCodes) > 0)

  pathwayAnalysisCodesLong <-
    readr::read_csv(
      file = file.path(exportFolder, "pathwayAnalysisCodesLong.csv"),
      col_types = readr::cols()
    ) |> SqlRender::snakeCaseToCamelCaseNames()
  testthat::expect_true(object = nrow(pathwayAnalysisCodesLong) > 0)

  pathwayAnalysisStatsData <-
    readr::read_csv(
      file = file.path(exportFolder, "pathwayAnalysisStats.csv"),
      col_types = readr::cols()
    ) |> SqlRender::snakeCaseToCamelCaseNames()
  testthat::expect_true(object = nrow(pathwayAnalysisStatsData) > 0)

  pathwaysAnalysisPathsData <-
    readr::read_csv(
      file = file.path(exportFolder, "pathwaysAnalysisPaths.csv"),
      col_types = readr::cols()
    ) |> SqlRender::snakeCaseToCamelCaseNames()
  testthat::expect_true(object = nrow(pathwayAnalysisStatsData) > 0)

  DatabaseConnector::disconnect(connection = connection)

  CohortPathways::executeCohortPathways(
    connection = DatabaseConnector::connect(connectionDetails = connectionDetails),
    cohortDatabaseSchema = cohortDatabaseSchema,
    targetDatabaseSchema = cohortDatabaseSchema,
    cohortTableName = cohortTableName,
    targetCohortIds = c(1, 2),
    eventCohortIds = c(10, 20),
    cohortDefinitionSet = cohortDefinitionSet,
    exportFolder = exportFolder,
    overwrite = TRUE
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = DatabaseConnector::connect(connectionDetails = connectionDetails),
    sql = "DROP TABLE IF EXISTS @cohort_database_schema.@table_temp;",
    table_temp = cohortTableName,
    cohort_database_schema = cohortDatabaseSchema,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
})
