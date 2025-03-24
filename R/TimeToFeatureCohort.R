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


#' Calculate Time to Feature Cohort
#'
#' This function calculates the time (in days) from the start date of a target cohort
#' to the start date of a feature cohort for subjects in the specified cohorts.
#' It generates a summary table of the time differences as well as a violin plot visualizing
#' the distribution of these differences.
#'
#' @param targetCohortIds A vector of one or more Cohort Ids corresponding to target cohort(s).
#'        These records are collapsed to a unique combination of subject_id and cohort_start_date.
#' @param featureCohortIds A vector of one or more Cohort Ids corresponding to feature cohort(s).
#'        These records are collapsed to a unique combination of subject_id and cohort_start_date.
#' @param targetCohortTableName The name of the target cohort table.
#' @param featureCohortTableName The name of the feature cohort table. Default is the same as targetCohortTableName.
#' @param minDays (Optional) Minimum number of days between the target and feature cohort start dates.
#'        If provided, only records with a time difference greater than or equal to minDays are included.
#' @param maxDays (Optional) Maximum number of days between the target and feature cohort start dates.
#'        If provided, only records with a time difference less than or equal to maxDays are included.
#' @param tempEmulationationSchema Some database platforms (e.g., Oracle, Impala) do not support temporary tables natively.
#'        To emulate temp tables, provide a schema with write privileges where temporary tables can be created.
#'        Default is obtained from \code{getOption("sqlRenderTempEmulationSchema")}.
#'
#' @return A list with two components:
#' \describe{
#'   \item{summaryStatistics}{A data frame containing summary statistics for the time differences (in days)
#'                             grouped by the database identifier.}
#'   \item{violinPlot}{A violin plot (created by \code{createViolinPlot}) that visualizes the distribution
#'                     of the time differences.}
#' }
#'
#' @details
#' The function executes a SQL query that:
#' \itemize{
#'   \item Extracts unique subject start dates for the target cohort and assigns a sequence number for each subject.
#'   \item Joins the target cohort with the feature cohort to find the first occurrence (minimum start date)
#'         in the feature cohort that occurs on or after the target start date.
#'   \item Calculates the time difference (in days) between the target and feature cohort start dates.
#'   \item Applies optional filtering based on \code{minDays} and \code{maxDays}.
#' }
#' The resulting data is then transformed into a long format (expanding counts by the number of subjects)
#' and used to create both a violin plot and a table of summary statistics.
#'
#' @examples
#' \dontrun{
#' results <- timeToFeatureCohort(
#'   targetCohortIds = c(1, 2),
#'   featureCohortIds = c(3),
#'   targetCohortTableName = "target_cohort",
#'   featureCohortTableName = "feature_cohort",
#'   minDays = 30,
#'   maxDays = 365,
#'   tempEmulationationSchema = "temp_schema"
#' )
#'
#' # Access the summary statistics
#' summaryStats <- results$summaryStatistics
#'
#' # Display the violin plot
#' print(results$violinPlot)
#' }
#'
#' @export
timeToFeatureCohort <- function(targetCohortIds,
                                featureCohortIds,
                                targetCohortTableName,
                                featureCohortTableName = targetCohortTableName,
                                minDays = NULL,
                                maxDays = NULL,
                                tempEmulationationSchema = getOption("sqlRenderTempEmulationSchema")) {
  # Construct the SQL query with placeholders for table names, cohort IDs, and filtering options.
  sql <-
    "
      with target_cohort as
      (
              SELECT subject_id,
                      target_start_date,
                      row_number() over(partition by subject_id order by target_start_date) target_sequence
              FROM
              (
                  SELECT DISTINCT subject_id,
                          cohort_start_date target_start_date
                  FROM @cohort_database_schema.@target_cohort_table
                  WHERE cohort_definition_id IN (@target_cohort_ids)
              ) t1
      )
      SELECT target_sequence,
              DATEDIFF(day, target_start_date, feature_start_date) AS days_to_feature,
              COUNT(DISTINCT subject_id) subjects
      FROM (
              SELECT f1.subject_id,
                        t1.target_sequence,
                        t1.target_start_date,
                        min(f1.cohort_start_date) feature_start_date
              FROM @cohort_database_schema.@feature_cohort_table f1
              INNER JOIN target_cohort t1
              ON  t1.subject_id = f1.subject_id
                AND t1.target_start_date <= f1.cohort_start_date
              WHERE f1.cohort_definition_id IN (@feature_cohort_ids)
              GROUP BY f1.subject_id,
                        t1.target_sequence,
                        t1.target_start_date
            ) f
      WHERE target_sequence > 0
            {@min_days_is_not_null} ? {
              AND DATEDIFF(day, target_start_date, feature_start_date) >= @min_days
            }
            {@max_days_is_not_null} ? {
              AND DATEDIFF(day, target_start_date, feature_start_date) <= @max_days
            }
      GROUP BY target_sequence,
              DATEDIFF(day, target_start_date, feature_start_date);
        "

  # Execute the SQL query using DatabaseConnector's renderTranslateExecuteSql function,
  # which renders the SQL with the provided parameters and executes it.
  timeToFeatureCohort <-
    DatabaseConnector::renderTranslateExecuteSql(
      sql = sql,
      target_cohort_table = targetCohortTableName,
      feature_cohort_table = featureCohortTableName,
      target_cohort_ids = targetCohortIds,
      feature_cohort_ids = featureCohortIds,
      tempEmulationSchema = tempEmulationationSchema,
      min_days = minDays,
      max_days = maxDays,
      min_days_is_not_null = !is.null(minDays),
      max_days_is_not_null = !is.null(maxDays)
    )

  # Transform the result into long format for plotting and summary statistics.
  longData <- timeToFeatureCohort |>
    dplyr::rename(value = .data$daysToFeature) |>
    dplyr::select(.data$value, .data$subjects) |>
    tidyr::uncount(weights = .data$subjects) |>
    dplyr::arrange(.data$value) |>
    dplyr::relocate(.data$value)

  # Calculate summary statistics for the time differences.
  summaryStatistics <-
    longData |>
    dplyr::mutate(value = formatDecimalWithComma(number = .data$alue)) |>
    tidyr::pivot_wider(id_cols = "statistic", values_from = .data$value) |>
    dplyr::arrange(.data$statistic)

  return(summaryStatistics)
}
