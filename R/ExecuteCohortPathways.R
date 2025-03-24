# Copyright 2025 Observational Health Data Sciences and Informatics
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
#' This function reproduces the logic behind "Cohort Pathways" (sometimes referred to as
#' "Treatment Pathways") as defined in ATLAS. A _pathway_ is the sequence of events that occur
#' after a cohort entry, often used to compare or visualize patients' treatment patterns.
#' The function takes a set of target cohorts (the index cohorts) and event cohorts (the
#' “steps” in the pathway) and computes the frequency and order in which these events occur.
#'
#' @details
#' **Overview**
#' Cohort Pathways are commonly used to visualize how patients move through different treatments
#' or events over time. Within ATLAS, these pathways are depicted as “sunburst” diagrams and
#' flow charts. This function provides a programmatic way to replicate that logic outside of ATLAS,
#' especially useful when you need to run a pathway analysis in a fully scripted R environment.
#'
#' **Key Steps**
#' 1. **Identify Cohorts**: You must already have _target_ and _event_ cohorts instantiated in
#'    your results schema (i.e., the cohorts exist in the `cohortTableName` table).
#' 2. **Specify Pathway Parameters**: Options such as `allowRepeats`, `collapseWindow`,
#'    and `maxDepth` control how the event sequences are aggregated:
#'    \itemize{
#'      \item \code{allowRepeats}: If `TRUE`, an event cohort can appear more than once at
#'      different time points in the pathway sequence for the same person.
#'      \item \code{collapseWindow}: Events that occur within this many days of each other
#'      (default = 30 days) will be considered part of the same step, reducing the “noise” of very
#'      frequent transitions.
#'      \item \code{maxDepth}: The maximum length of a pathway (default = 5 steps). Longer
#'      sequences are truncated to keep the analysis and visualization more interpretable.
#'    }
#' 3. **Analyze**: The function creates internal temporary tables (or uses your specified
#'    \code{tempEmulationSchema} if necessary) to calculate how many people experience
#'    each distinct pathway. It then returns several data frames capturing:
#'    \itemize{
#'      \item \code{pathwayAnalysisStatsData}: Summary statistics by target cohort, including
#'            total counts and the distribution of event occurrences.
#'      \item \code{pathwaysAnalysisPathsData}: The distinct sequences (paths) observed,
#'            with counts of how many persons followed each sequence.
#'      \item \code{pathwaysAnalysisEventsData}: A more granular breakdown of how events
#'            contribute to different pathway steps.
#'      \item \code{pathwayAnalysisCodesData} and related tables: Helper mappings and indices
#'            that translate event cohorts into pathway “codes.”
#'    }
#'
#' **Cell Count Restrictions**
#' The argument `minCellCount` allows you to enforce a minimum cell count, consistent with
#' privacy rules. Any group or count below `minCellCount` can be masked or handled downstream
#' according to your organization’s policies.
#'
#' **Reference**
#' - _The Book of OHDSI_ (Chapter: Characterization, Section: Cohort Pathways in ATLAS)
#' - OHDSI Forums:
#'   \url{https://forums.ohdsi.org/t/cohort-pathways-in-atlas-faq/9511/2}
#'   \url{https://forums.ohdsi.org/t/reproducing-a-treatment-pathway-study-atlas-json-files/9450}
#'
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
#' @param tempEmulationSchema   Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param minCellCount          (Default = 5) The minimum cell count for fields contains person counts or fractions.
#' @param allowRepeats          (Default = FALSE) Allow cohort events/combos to appear multiple times in the same pathway.
#' @param maxDepth              (Default = 5) Maximum number of steps in a given pathway to be included in the sunburst plot
#' @param collapseWindow        (Default = 30) Any dates found within the specified collapse days will
#'                              be reassigned the earliest date. Collapsing dates reduces pathway
#'                              variation, leading to a reduction in 'noise' in the result.
#' @param connectionDetails   An object of type \code{\link[DatabaseConnector]{createConnectionDetails}}
#'                           (from the \pkg{DatabaseConnector} package) providing the details
#'                           to connect to the database. If \code{NULL}, then \code{connection}
#'                           must be provided.
#' @param connection          An existing database connection (as created by
#'                           \code{\link[DatabaseConnector]{connect}}). If \code{NULL}, a new
#'                           connection will be established using \code{connectionDetails} at
#'                           the start of the function, and closed upon completion.
#' @param cohortDatabaseSchema Schema where the target and event cohorts are stored.
#'                            For SQL Server, this should include both the database and
#'                            schema name (e.g., "mydatabase.dbo").
#' @param cohortTableName     Name of the table containing your cohorts (default = "cohort").
#' @param tempEmulationSchema For databases without true temp table support (e.g., Oracle),
#'                            specify a schema where you have write privileges to emulate temp
#'                            tables.
#' @param targetCohortIds     Integer vector of cohort definition IDs representing one or more
#'                            _target_ cohorts (the index populations whose pathways you want
#'                            to explore).
#' @param eventCohortIds      Integer vector of cohort definition IDs representing one or more
#'                            _event_ cohorts (the steps in the pathways, e.g., treatments or
#'                            clinical events).
#' @param minCellCount        The minimum cell count to display (default = 5). Any group or count
#'                            below this threshold should be redacted or masked if you're following
#'                            a strict privacy policy.
#' @param allowRepeats        Boolean; if \code{TRUE}, an event cohort can reappear multiple times
#'                            in the same patient's pathway sequence (default = \code{FALSE}).
#' @param maxDepth            Maximum number of steps in any pathway (default = 5). Pathways
#'                            longer than this threshold are truncated.
#' @param collapseWindow      An integer indicating the number of days within which events are
#'                            considered the same "step" (default = 30 days). This window
#'                            effectively groups closely occurring events to reduce spurious
#'                            variability.
#'
#' @return
#' A named list of data frames capturing key pathway results:
#' \describe{
#'   \item{\code{pathwayAnalysisStatsData}}{High-level summary statistics about the
#'                                          number of people in each target cohort
#'                                          and the distribution of events.}
#'   \item{\code{pathwaysAnalysisPathsData}}{Rows describing each distinct pathway
#'                                          (sequence of events) observed,
#'                                          along with counts of persons following each path.}
#'   \item{\code{pathwaysAnalysisEventsData}}{Detailed event-level information, allowing
#'                                           you to see how each event fits into a given path.}
#'   \item{\code{pathwayAnalysisCodesData}}{Mapping of event cohort IDs to pathway "codes"
#'                                          for easy interpretation of combinatorial steps.}
#'   \item{\code{pathwaycomboIds, pathwayAnalysisCodesLong, isCombo}}{Additional helper
#'                                                                    tables describing
#'                                                                    combos of events.}
#' }
#'
#' @examples
#' \dontrun{
#' # Example: Execute a pathway analysis for two target cohorts and three event cohorts.
#'
#' # 1) Create connection details or use an existing connection.
#' connDetails <- DatabaseConnector::createConnectionDetails(
#'   dbms = "postgresql",
#'   server = "myserver/mydb",
#'   user = "someuser",
#'   password = "DontSharePasswords"
#' )
#'
#' # 2) Run the function
#' result <- executeCohortPathways(
#'   connectionDetails = connDetails,
#'   cohortDatabaseSchema = "my_results_schema",
#'   cohortTableName = "my_cohort",
#'   targetCohortIds = c(123, 456), # e.g., 'diabetes' and 'hypertension' cohorts
#'   eventCohortIds = c(789, 1011, 1213), # e.g., 'metformin', 'insulin', 'ace_inhibitors'
#'   allowRepeats = TRUE,
#'   maxDepth = 4,
#'   collapseWindow = 14
#' )
#'
#' # 3) Inspect the returned data frames for pathway summaries and details
#' names(result)
#' head(result$pathwaysAnalysisPathsData)
#' }
#'
#' @export
executeCohortPathways <- function(connectionDetails = NULL,
                                  connection = NULL,
                                  cohortDatabaseSchema,
                                  cohortTableName = "cohort",
                                  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                  targetCohortIds,
                                  eventCohortIds,
                                  minCellCount = 5,
                                  allowRepeats = FALSE,
                                  maxDepth = 5,
                                  collapseWindow = 30) {
  start <- Sys.time()
  message(paste0("Run Cohort Pathways started at ", start))

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(
    x = cohortDatabaseSchema,
    min.len = 1,
    add = errorMessage
  )
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
  checkmate::reportAssertions(collection = errorMessage)

  # allow repeats is used as text 'true' or 'false' in sql
  if (allowRepeats) {
    allowRepeats <- "true"
  } else {
    allowRepeats <- "false"
  }

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  # perform checks on cohort database schema.
  tablesInCohortDatabaseSchema <-
    DatabaseConnector::getTableNames(
      connection = connection,
      databaseSchema = cohortDatabaseSchema
    ) |>
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

  pathwayAnalysisStatsData <- c()
  pathwaysAnalysisPathsData <- c()
  pathwaysAnalysisEventsData <- c()

  for (i in (1:length(instantiatedTargetCohortIds))) {
    targetCohortId <- instantiatedTargetCohortIds[[i]]

    generationId <-
      (as.integer(format(Sys.Date(), "%Y%m%d")) * 1000) +
      sample(
        x = 1:1000,
        size = 1,
        replace = FALSE
      )

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
      allow_repeats = allowRepeats,
      combo_window = collapseWindow,
      max_depth = maxDepth,
      pathway_target_cohort_id = targetCohortId,
      target_cohort_table = targetCohortTable,
      generation_id = generationId,
      event_cohort_id_index_map = eventCohortIdIndexMap
    )

    pathwayAnalysisStatsData[[i]] <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = "SELECT * FROM #pa_stats;",
        snakeCaseToCamelCase = TRUE
      ) |>
      dplyr::tibble()

    pathwaysAnalysisPathsData[[i]] <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = " SELECT * FROM #pa_paths;",
        snakeCaseToCamelCase = TRUE
      ) |>
      dplyr::tibble()

    pathwaysAnalysisEventsData[[i]] <-
      DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = " SELECT * FROM #pa_events;",
        snakeCaseToCamelCase = TRUE
      ) |>
      dplyr::tibble()

    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = " DROP TABLE IF EXISTS #pa_paths;
                DROP TABLE IF EXISTS #pa_stats;
                DROP TABLE IF EXISTS #pa_events;",
      profile = FALSE,
      progressBar = TRUE,
      reportOverallTime = FALSE,
      tempEmulationSchema = tempEmulationSchema
    )

    generationIds <- c(generationId, generationIds)
  }

  pathwayAnalysisStatsData <-
    dplyr::bind_rows(pathwayAnalysisStatsData)
  pathwaysAnalysisPathsData <-
    dplyr::bind_rows(pathwaysAnalysisPathsData)
  pathwaysAnalysisEventsData <-
    dplyr::bind_rows(pathwaysAnalysisEventsData)

  pathwaycomboIds <- pathwaysAnalysisPathsData |>
    dplyr::select(dplyr::starts_with("step")) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("step"),
      names_to = "names",
      values_to = "comboIds"
    ) |>
    dplyr::select("comboIds") |>
    dplyr::distinct() |>
    dplyr::filter(.data$comboIds > 0) |>
    dplyr::select("comboIds") |>
    dplyr::arrange(.data$comboIds)

  pathwayAnalysisCodesLong <- c()
  for (i in (1:nrow(pathwaycomboIds))) {
    cohortIndex <- extractBitSum(x = pathwaycomboIds[i, ]$comboIds)
    combisData <- dplyr::tibble("cohortIndex" = cohortIndex) |>
      dplyr::mutate("comboId" = pathwaycomboIds[i, ]$comboIds) |>
      dplyr::mutate("targetCohortId" = targetCohortId) |>
      dplyr::inner_join(eventCohortIdIndexMaps,
        by = "cohortIndex"
      )
    pathwayAnalysisCodesLong <- dplyr::bind_rows(
      combisData,
      pathwayAnalysisCodesLong
    )
  }

  isCombo <- pathwayAnalysisCodesLong |>
    dplyr::select(
      "targetCohortId",
      "comboId",
      "eventCohortId"
    ) |>
    dplyr::distinct() |>
    dplyr::group_by(.data$targetCohortId, .data$comboId) |>
    dplyr::summarise(numberOfEvents = dplyr::n()) |>
    dplyr::mutate(isCombo = dplyr::case_when(.data$numberOfEvents > 1 ~ 1, TRUE ~
      0))

  pathwayAnalysisCodesLong <- pathwayAnalysisCodesLong |>
    dplyr::inner_join(isCombo,
      by = c("targetCohortId", "comboId")
    ) |>
    tidyr::crossing(dplyr::tibble("pathwayAnalysisGenerationId" = generationIds)) |>
    dplyr::select(
      "pathwayAnalysisGenerationId",
      "comboId",
      "targetCohortId",
      "eventCohortId",
      "isCombo",
      "numberOfEvents"
    ) |>
    dplyr::rename("code" = "comboId")

  pathwayAnalysisCodesData <- pathwayAnalysisCodesLong |>
    dplyr::select(
      "pathwayAnalysisGenerationId",
      "code",
      "isCombo"
    ) |>
    dplyr::group_by(
      .data$pathwayAnalysisGenerationId,
      .data$code,
      .data$isCombo
    ) |>
    dplyr::select(
      "pathwayAnalysisGenerationId",
      "code",
      "isCombo"
    )

  allData <- c()
  allData$pathwayAnalysisStatsData <- pathwayAnalysisStatsData
  allData$pathwaysAnalysisPathsData <- pathwaysAnalysisPathsData
  allData$pathwaysAnalysisEventsData <- pathwaysAnalysisEventsData
  allData$pathwaycomboIds <- pathwaycomboIds
  allData$pathwayAnalysisCodesLong <- pathwayAnalysisCodesLong
  allData$isCombo <- isCombo
  allData$pathwayAnalysisCodesData <- pathwayAnalysisCodesData

  delta <- Sys.time() - start

  message(
    "Computing Cohort Pathways took ",
    signif(delta, 3),
    " ",
    attr(delta, "units")
  )
  return(allData)
}
