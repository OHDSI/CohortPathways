# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of CohortPathways
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Execute cohort pathway analysis.
#'
#' @description
#' Runs the cohort pathways on all instantiated combinations of target and event cohorts.
#' Assumes the cohorts have already been instantiated.
#'
#' @param connectionDetails   An object of type connectionDetails as created using the
#'                            \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                            DatabaseConnector package. Can be left NULL if \code{connection} is
#'                            provided.
#' @param connection          An object of type \code{connection} as created using the
#'                            \code{\link[DatabaseConnector]{connect}} function in the
#'                            DatabaseConnector package. Can be left NULL if connectionDetails
#'                            is provided, in which case a new connection will be opened at the start
#'                            of the function, and closed when the function finishes.
#' @param cohortDatabaseSchema   Schema name where your cohort tables reside. Note that for SQL Server,
#'                               this should include both the database and schema name, for example
#'                               'scratch.dbo'.
#' @param cohortTableName            The name of the cohort table.
#' @param targetCohortIds     A vector of one or more Cohort Ids corresponding to target cohort (s).
#' @param eventCohortIds      A vector of one or more Cohort Ids corresponding to event cohort (s).
#' @param cohortDefinitionSet A data frame object with minimum two columns, cohortId and in
#'                            targetCohortId and eventCohortId. This is the source of cohort names.
#' @param targetDatabaseSchema   (Optional) Schema name where output pathway tables would reside. This is also known as
#'                               as resultsDatabaseSChema. If not specified, scratch schema will be used. The output may not
#'                               persist in the database after disconnection. Note that for SQL Server,
#'                               this should include both the database and schema name, for example
#'                               'scratch.dbo'.
#' @param tempEmulationSchema   Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param exportFolder          The folder where the output will be exported to. If this folder
#'                              does not exist it will be created.
#' @param minCellCount          (Default = 5) The minimum cell count for fields contains person counts or fractions.
#' @param allowRepeats          (Default = FALSE) Allow cohort events/combos to appear multiple times in the same pathway.
#' @param maxDepth              (Default = 5) Maximum number of steps in a given pathway to be included in the sunburst plot
#' @param collapseWindow        (Default = 30) Any dates found within the specified collapse days will
#'                              be reassigned the earliest date. Collapsing dates reduces pathway
#'                              variation, leading to a reduction in 'noise' in the result.
#' @param overwrite             (Default = TRUE) Do you want to overwrite results?
#'
#' @return   Nothing is returned
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(
#'   dbms = "postgresql",
#'   server = "ohdsi.com",
#'   port = 5432,
#'   user = "me",
#'   password = "secure"
#' )
#'
#' executeCohortPathways(
#'   connectionDetails = connectionDetails,
#'   cohorts = cohorts,
#'   exportFolder = "export",
#'   cohortDatabaseSchema = "results"
#' )
#' }
#'
#' @export
executeCohortPathways <- function(connectionDetails = NULL,
                                  connection = NULL,
                                  cohortDatabaseSchema,
                                  cohortTableName = "cohort",
                                  targetDatabaseSchema = NULL,
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                  targetCohortIds,
                                  eventCohortIds,
                                  cohortDefinitionSet,
                                  exportFolder,
                                  minCellCount = 5,
                                  allowRepeats = FALSE,
                                  maxDepth = 5,
                                  collapseWindow = 30,
                                  overwrite = TRUE) {
  start <- Sys.time()
  message(paste0("Run Cohort Pathways started at ", start))
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(x = cohortDatabaseSchema,
                             min.len = 1,
                             add = errorMessage)
  if (!is.null(targetDatabaseSchema)) {
    checkmate::assertCharacter(x = cohortDatabaseSchema,
                               min.len = 1,
                               add = errorMessage)
  }
  checkmate::assertDouble(
    x = minCellCount,
    lower = 0,
    len = 1,
    add = errorMessage
  )
  checkmate::assertLogical(
    x = allowRepeats,
    any.missing = FALSE,
    len = 1,
    add = errorMessage
  )
  checkmate::assertDouble(
    x = maxDepth,
    lower = 0,
    len = 1,
    add = errorMessage
  )
  checkmate::assertDouble(
    x = collapseWindow,
    lower = 0,
    len = 1,
    add = errorMessage
  )
  checkmate::assertLogical(
    x = overwrite,
    any.missing = FALSE,
    len = 1,
    add = errorMessage
  )
  
  exportFolder <- normalizePath(exportFolder, mustWork = FALSE)
  errorMessage <-
    createIfNotExist(type = "folder",
                     name = exportFolder,
                     errorMessage = errorMessage)
  checkmate::assertDataFrame(
    x = cohortDefinitionSet,
    min.rows = length(c(targetCohortIds, eventCohortIds) |> unique()),
    any.missing = FALSE,
    min.cols = 2,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertNames(
    names(cohortDefinitionSet),
    must.include = c("cohortId",
                     "cohortName"),
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)
  
  # allow repeats is used as text 'true' or 'false' in sql
  if (allowRepeats) {
    allowRepeats <- "true"
  } else {
    allowRepeats <- "false"
  }
  
  
  if (file.exists(file.path(exportFolder, "pathwaysAnalysisPaths.csv"))) {
    if (!overwrite) {
      stop("   Previous pathwaysAnalysisPaths.csv exists in export folder",
           exportFolder)
    } else {
      message(
        "   Previous pathwaysAnalysisPaths.csv exists in export folder and will be replaced."
      )
    }
  }
  
  if (file.exists(file.path(exportFolder, "pathwaysAnalysisPaths.csv"))) {
    if (!overwrite) {
      stop("   Previous pathwaysAnalysisPaths.csv exists in export folder.",
           exportFolder)
    } else {
      message(
        "   Previous pathwaysAnalysisPaths.csv exists in export folder and will be replaced."
      )
    }
  }
  
  if (file.exists(file.path(exportFolder, "pathwayAnalysisCodes.csv"))) {
    if (!overwrite) {
      stop("   Previous pathwayAnalysisCodes.csv exists in export folder.",
           exportFolder)
    } else {
      message("   Previous pathwayAnalysisCodes.csv exists in export folder and will be replaced.")
    }
  }
  
  if (file.exists(file.path(exportFolder, "pathwayAnalysisCodesLong.csv"))) {
    if (!overwrite) {
      stop("   Previous pathwayAnalysisCodesLong.csv exists in export folder.",
           exportFolder)
    } else {
      message(
        "   Previous pathwayAnalysisCodesLong.csv exists in export folder and will be replaced."
      )
    }
  }
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  # perform checks on cohort database schema.
  tablesInCohortDatabaseSchema <-
    DatabaseConnector::getTableNames(connection = connection,
                                     databaseSchema = cohortDatabaseSchema) |>
    tolower()
  
  cohortTableName <- tolower(cohortTableName)
  if (!cohortTableName %in% c(tablesInCohortDatabaseSchema, "")) {
    stop(
      paste0(
        "Cohort table '",
        toupper(cohortTableName),
        "' not found in CohortDatabaseSchema '",
        cohortDatabaseSchema,
        "'"
      )
    )
  }
  
  cohortCounts <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT cohort_definition_id AS cohort_id,
              COUNT(*) AS cohort_entries,
              COUNT(DISTINCT subject_id) AS cohort_subjects
          FROM @cohort_database_schema.@cohort_table
          {@cohort_ids != ''} ? {WHERE cohort_definition_id IN (@cohort_ids)}
          GROUP BY cohort_definition_id;",
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTableName,
    cohort_ids = c(targetCohortIds, eventCohortIds),
    snakeCaseToCamelCase = TRUE
  ) |>
    dplyr::tibble()
  
  if (nrow(cohortCounts) < length(c(targetCohortIds, eventCohortIds) |> unique())) {
    message("Not all cohorts have more than 0 records.")
    
    if (nrow(cohortCounts |> dplyr::filter(.data$cohortId %in% c(targetCohortIds))) == 0) {
      stop("None of the target cohorts are instantiated.")
    }
    
    if (nrow(cohortCounts |> dplyr::filter(.data$cohortId %in% c(eventCohortIds))) == 0) {
      stop("None of the event cohorts are instantiated.")
    }
    
    message(
      sprintf(
        "    Found %s of %s (%1.2f%%) target cohorts instantiated. ",
        nrow(cohortCounts |> dplyr::filter(
          .data$cohortId %in% c(targetCohortIds)
        )),
        length(targetCohortIds),
        100 * (
          nrow(cohortCounts |> dplyr::filter(
            .data$cohortId %in% c(targetCohortIds)
          )) / length(targetCohortIds)
        )
      )
    )
    message(
      sprintf(
        "    Found %s of %s (%1.2f%%) event cohorts instantiated. ",
        nrow(cohortCounts |> dplyr::filter(
          .data$cohortId %in% c(eventCohortIds)
        )),
        length(eventCohortIds),
        100 * (
          nrow(cohortCounts |> dplyr::filter(
            .data$cohortId %in% c(eventCohortIds)
          )) / length(eventCohortIds)
        )
      )
    )
  }
  
  targetCohortTable <-
    paste0(cohortDatabaseSchema, ".", cohortTableName)
  
  instantiatedEventCohortIds <-
    intersect(x = eventCohortIds, y = cohortCounts$cohortId)
  instantiatedTargetCohortIds <-
    intersect(x = targetCohortIds, y = cohortCounts$cohortId)
  
  pathwayAnalysisCodes <- "pa_codes"
  if (!is.null(targetDatabaseSchema)) {
    # pathwayAnalysisCodes <-
    #   paste0(targetDatabaseSchema, ".", pathwayAnalysisCodeTable)
    pathwayAnalysisCodesTableIsTemp <- FALSE
  } else {
    pathwayAnalysisCodes <- paste0("#", pathwayAnalysisCodes)
    pathwayAnalysisCodesTableIsTemp <- TRUE
  }
  
  pathwayAnalysisEvents <- "pa_events"
  if (!is.null(targetDatabaseSchema)) {
    pathwayAnalysisEvents <-
      paste0(targetDatabaseSchema, ".", pathwayAnalysisEvents)
  } else {
    pathwayAnalysisEvents <- paste0("#", pathwayAnalysisEvents)
  }
  
  pathwayAnalysisPaths <- "pa_paths"
  if (!is.null(targetDatabaseSchema)) {
    pathwayAnalysisPaths <-
      paste0(targetDatabaseSchema, ".", pathwayAnalysisPaths)
  } else {
    pathwayAnalysisPaths <- paste0("#", pathwayAnalysisPaths)
  }
  
  pathwayAnalysisStats <- "pa_stats"
  if (!is.null(targetDatabaseSchema)) {
    pathwayAnalysisStats <-
      paste0(targetDatabaseSchema, ".", pathwayAnalysisStats)
  } else {
    pathwayAnalysisStats <- paste0("#", pathwayAnalysisStats)
  }
  
  # perform checks on target database schema.
  createTablePathwayAnalysisCodes <- TRUE
  createTablePathwayAnalysisEvents <- TRUE
  createTablePathwayAnalysisPaths <- TRUE
  createTablePathwayAnalysisStats <- TRUE
  
  targetCohortTable <-
    paste0(cohortDatabaseSchema, ".", cohortTableName)
  
  if (!is.null(targetDatabaseSchema)) {
    tablesIntargetDatabaseSchema <-
      DatabaseConnector::getTableNames(connection = connection,
                                       databaseSchema = targetDatabaseSchema)
    
    if ("pa_codes" %in% tolower(tablesIntargetDatabaseSchema)) {
      createTablePathwayAnalysisCodes <- FALSE
    }
    if ("pa_events" %in% tolower(tablesIntargetDatabaseSchema)) {
      createTablePathwayAnalysisEvents <- FALSE
    }
    if ("pa_paths" %in% tolower(tablesIntargetDatabaseSchema)) {
      createTablePathwayAnalysisPaths <- FALSE
    }
    if ("pa_stats" %in% tolower(tablesIntargetDatabaseSchema)) {
      createTablePathwayAnalysisStats <- FALSE
    }
  }
  
  if (createTablePathwayAnalysisCodes) {
    sql <-
      SqlRender::readSql(
        sourceFile = system.file(
          "sql",
          "sql_server",
          "CreateTablePathwayAnalysisCodes.sql",
          package = utils::packageName()
        )
      )
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      pathway_analysis_codes = pathwayAnalysisCodes
    )
  }
  
  if (createTablePathwayAnalysisEvents) {
    sql <-
      SqlRender::readSql(
        sourceFile = system.file(
          "sql",
          "sql_server",
          "CreateTablePathwayAnalysisEvents.sql",
          package = utils::packageName()
        )
      )
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      pathway_analysis_events = pathwayAnalysisEvents
    )
  }
  
  if (createTablePathwayAnalysisPaths) {
    sql <-
      SqlRender::readSql(
        sourceFile = system.file(
          "sql",
          "sql_server",
          "CreateTablePathwayAnalysisPaths.sql",
          package = utils::packageName()
        )
      )
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      pathway_analysis_paths = pathwayAnalysisPaths
    )
  }
  
  if (createTablePathwayAnalysisStats) {
    sql <-
      SqlRender::readSql(
        sourceFile = system.file(
          "sql",
          "sql_server",
          "CreateTablePathwayAnalysisStats.sql",
          package = utils::packageName()
        )
      )
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = sql,
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      pathway_analysis_stats = pathwayAnalysisStats
    )
  }
  
  pathwayAnalysisSql <-
    SqlRender::readSql(
      sourceFile = system.file(
        "sql",
        "sql_server",
        "RunPathwayAnalysis.sql",
        package = utils::packageName()
      )
    )
  
  generationIds <- c()
  eventCohortIdIndexMaps <-
    dplyr::tibble("eventCohortId" = instantiatedEventCohortIds |> unique()) |>
    dplyr::arrange(.data$eventCohortId) |>
    dplyr::mutate(cohortIndex = dplyr::row_number())
  
  pathwaysAnalysisPathsSql <-
    SqlRender::readSql(
      sourceFile = system.file(
        "sql",
        "sql_server",
        "PathwayAnalysisPaths.sql",
        package = utils::packageName()
      )
    )
  
  for (i in (1:length(instantiatedTargetCohortIds))) {
    targetCohortId <- instantiatedTargetCohortIds[[i]]
    
    generationId <-
      (as.integer(format(Sys.Date(), "%Y%m%d")) * 1000) +
      sample(x = 1:1000,
             size = 1,
             replace = FALSE)
    
    eventCohortIdIndexMap <- eventCohortIdIndexMaps |>
      dplyr::rowwise() |>
      dplyr::mutate(
        sql = paste0(
          "SELECT ",
          .data$eventCohortId,
          " AS cohort_definition_id, ",
          .data$cohortIndex,
          " AS cohort_index"
        )
      ) |>
      dplyr::pull(.data$sql) |>
      paste0(collapse = " union all ")
    
    message(
      paste0(
        "   Generating Cohort Pathways for target cohort: ",
        targetCohortId,
        ". Generation id: ",
        generationId,
        "."
      )
    )
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = pathwayAnalysisSql,
      profile = FALSE,
      progressBar = TRUE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema,
      pathway_analysis_events = pathwayAnalysisEvents,
      pathway_analysis_stats = pathwayAnalysisStats,
      allow_repeats = allowRepeats,
      combo_window = collapseWindow,
      max_depth = maxDepth,
      pathway_target_cohort_id = targetCohortId,
      target_cohort_table = targetCohortTable,
      generation_id = generationId,
      event_cohort_id_index_map = eventCohortIdIndexMap
    )
    
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = pathwaysAnalysisPathsSql,
      profile = FALSE,
      progressBar = FALSE,
      reportOverallTime = FALSE,
      pathway_analysis_events = pathwayAnalysisEvents,
      tempEmulationSchema = tempEmulationSchema,
      pathway_analysis_paths = pathwayAnalysisPaths,
      generation_id = generationId
    )
    message(" Done.")
    generationIds <- c(generationId, generationIds)
  }
  
  pathwayAnalysisStatsData <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = "SELECT * FROM @pathway_analysis_stats
              WHERE target_cohort_id IN (@target_cohort_id)
                AND pathway_analysis_generation_id IN (@pathways_analysis_generation_ids);",
      snakeCaseToCamelCase = TRUE,
      pathway_analysis_stats = pathwayAnalysisStats,
      target_cohort_id = instantiatedTargetCohortIds,
      pathways_analysis_generation_ids = generationIds
    ) |>
    dplyr::tibble()
  
  pathwaysAnalysisPathsData <-
    DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = " SELECT * FROM @pathway_analysis_paths
              WHERE target_cohort_id IN (@target_cohort_id)
                AND pathway_analysis_generation_id IN (@pathways_analysis_generation_ids);",
      snakeCaseToCamelCase = TRUE,
      pathway_analysis_paths = pathwayAnalysisPaths,
      target_cohort_id = instantiatedTargetCohortIds,
      pathways_analysis_generation_ids = generationIds
    ) |>
    dplyr::tibble()
  
  pathwaycomboIds <- pathwaysAnalysisPathsData |>
    dplyr::select(dplyr::starts_with("step")) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("step"),
      names_to = "names",
      values_to = "comboIds"
    ) |>
    dplyr::select(comboIds) |>
    dplyr::distinct() |>
    dplyr::filter(.data$comboIds > 0) |>
    dplyr::select(comboIds) |> 
    dplyr::arrange(.data$comboIds)
  
  pathwayAnalysisCodesLong <- c()
  for (i in (1:nrow(pathwaycomboIds))) {
    cohortIndex <- extractBitSum(x = pathwaycomboIds[i,]$comboIds)
    combisData <- dplyr::tibble(cohortIndex = cohortIndex) |>
      dplyr::mutate(comboId = pathwaycomboIds[i,]$comboIds) |>
      dplyr::mutate(targetCohortId = targetCohortId) |>
      dplyr::inner_join(eventCohortIdIndexMaps,
                        by = "cohortIndex") |>
      dplyr::inner_join(cohortDefinitionSet,
                        by = c("eventCohortId" = "cohortId")) |>
      dplyr::rename(eventCohortName = .data$cohortName)
    
    pathwayAnalysisCodesLong <- dplyr::bind_rows(combisData,
                                                 pathwayAnalysisCodesLong)
  }
  
  isCombo <- pathwayAnalysisCodesLong |>
    dplyr::select(targetCohortId,
                  comboId,
                  eventCohortId) |>
    dplyr::distinct() |>
    dplyr::group_by(.data$targetCohortId, .data$comboId) |>
    dplyr::summarise(numberOfEvents = dplyr::n()) |>
    dplyr::mutate(isCombo = dplyr::case_when(.data$numberOfEvents > 1 ~ 1, TRUE ~
                                               0))
  
  pathwayAnalysisCodesLong <- pathwayAnalysisCodesLong |>
    dplyr::inner_join(isCombo,
                      by = c("targetCohortId", "comboId")) |>
    tidyr::crossing(dplyr::tibble("pathwayAnalysisGenerationId" = generationIds)) |>
    dplyr::select(
      pathwayAnalysisGenerationId,
      comboId,
      targetCohortId,
      eventCohortId,
      eventCohortName,
      isCombo,
      numberOfEvents
    ) |>
    dplyr::rename("code" = comboId)
  
  pathwayAnalysisCodesData <- pathwayAnalysisCodesLong |>
    dplyr::select(
      pathwayAnalysisGenerationId,
      code,
      eventCohortName,
      isCombo
    ) |>
    dplyr::group_by(.data$pathwayAnalysisGenerationId,
                    .data$code,
                    .data$isCombo) |>
    dplyr::mutate(name = paste0(.data$eventCohortName, collapse = " + ")) |>
    dplyr::select(pathwayAnalysisGenerationId,
                  code,
                  name,
                  isCombo)
  
  readr::write_excel_csv(
    x = pathwayAnalysisStatsData |> SqlRender::camelCaseToSnakeCaseNames(),
    file = file.path(exportFolder, "pathwayAnalysisStats.csv"),
    na = "",
    append = FALSE
  )
  
  readr::write_excel_csv(
    x = pathwaysAnalysisPathsData |> SqlRender::camelCaseToSnakeCaseNames(),
    file = file.path(exportFolder, "pathwaysAnalysisPaths.csv"),
    na = "",
    append = FALSE
  )
  
  readr::write_excel_csv(
    x = pathwayAnalysisCodesData |> SqlRender::camelCaseToSnakeCaseNames(),
    file = file.path(exportFolder, "pathwayAnalysisCodes.csv"),
    na = "",
    append = FALSE
  )
  
  readr::write_excel_csv(
    x = pathwayAnalysisCodesLong |> SqlRender::camelCaseToSnakeCaseNames(),
    file = file.path(exportFolder, "pathwayAnalysisCodesLong.csv"),
    na = "",
    append = FALSE
  )
  
  if (!is.null(targetDatabaseSchema)) {
    DatabaseConnector::insertTable(
      connection = connection,
      databaseSchema = targetDatabaseSchema,
      tableName = pathwayAnalysisCodes,
      data = pathwayAnalysisCodesData,
      dropTableIfExists = FALSE,
      createTable = FALSE,
      tempTable = pathwayAnalysisCodesTableIsTemp,
      camelCaseToSnakeCase = TRUE
    )
  }
  
  delta <- Sys.time() - start
  
  message("Computing Cohort Pathways took ",
          signif(delta, 3),
          " ",
          attr(delta, "units"))
}
