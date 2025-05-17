#' Create a Sunburst Plot for Treatment Pathways
#'
#' This function creates an interactive sunburst plot to visualize treatment pathways
#' from CohortPathway analysis results. The plot shows the hierarchical structure of
#' treatment sequences, with each ring representing a step in the pathway.
#'
#' @param cohortPathwayResults A list containing the results from CohortPathway analysis.
#'        Must include 'pathwaysAnalysisPathsData' and 'isCombo' data frames.
#' @param cohortDefinitionSet A data frame containing information about cohorts
#'        that will be used to generate descriptive names for the events in the diagram and target names of the plot
#' @param numberOfPathsInteger specifying the maximum number of steps to include in the plot.
#' @param minCount Integer specifying the minimum count value for a path to be included.
#' @return An HTML widget object containing the interactive sunburst plot.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(CohortPathway)
#' sunburstPlot <- CohortPathways::createPathwaySunburst(cohortPathwayResults, cohortDefinitionSet)
#' }
createPathwaySunburst <- function(cohortPathwayResults,
                                  cohortDefinitionSet,
                                  numberOfPaths = 3,
                                  minCount = 5,
                                  plotWidth = "80%",
                                  plotHeight = 600) {
  rlang::check_installed("sunburstR")
  rlang::check_installed("htmlwidgets")
  rlang::check_installed("d3r")
  
  # Input validation
  checkmate::assertList(cohortPathwayResults, min.len = 7, types = "data.frame")
  
  # Extract required data
  pathwaysAnalysisPathsDatas <- purrr::pluck(cohortPathwayResults, "pathwaysAnalysisPathsData") |>
    dplyr::group_by(.data$targetCohortId) |>
    dplyr::group_split()
  
  isCombo <- purrr::pluck(cohortPathwayResults, "isCombo")
  
  checkmate::assertDataFrame(x = isCombo,
                             min.rows = 1,
                             min.cols = 1)
  
  # Get event names
  eventNames <- .splitEvenToPowers(df = isCombo, cohortDefinitionSet = cohortDefinitionSet)
  
  # Create final data frame with counts and full path name
  paths_data <- cohortPathwayResults$pathwaysAnalysisPathsData |>
    dplyr::select(-c(pathwayAnalysisGenerationId, targetCohortId)) |>
    dplyr::left_join(eventNames, by = c("step1" = "comboId")) |>
    dplyr::rename(step1Name = pathName) |>
    dplyr::left_join(eventNames, by = c("step2" = "comboId")) |>
    dplyr::rename(step2Name = pathName) |>
    dplyr::left_join(eventNames, by = c("step3" = "comboId")) |>
    dplyr::rename(step3Name = pathName) |>
    dplyr::left_join(eventNames, by = c("step4" = "comboId")) |>
    dplyr::rename(step4Name = pathName) |>
    dplyr::select(-c(1:10)) |>
    dplyr::filter(countValue > minCount)
  
  # Convert tabular data to JSON
  paths_data_json <- d3r::d3_nest(paths_data, value_cols = "countValue")
  
  # Create sunburst plot
  sunburst_plot <- sunburstR::sunburst(
    data = paths_data_json,
    width = plotWidth,
    height = plotHeight,
    valueField = "countValue",
    legend = list(
      w = 490,
      h = 50,
      r = 100,
      s = 5
    ),
    count = TRUE
  )
  
  return(sunburst_plot)
}


.prepareEventNames <- function(cohortDefinitionSet,
                               cohortPathwayResults) {
  event_names <- purrr::pluck(cohortPathwayResults, "pathwayAnalysisCodesLong") |>
    dplyr::select(.data$code, cohortId = .data$eventCohortId) |>
    dplyr::distinct() |>
    dplyr::inner_join(
      cohortDefinitionSet |>
        dplyr::select(.data$cohortId, .data$cohortName),
      by = dplyr::join_by(cohortId)
    ) |>
    dplyr::group_by(.data$code) |>
    dplyr::reframe(combination = paste(.data$cohortName, collapse = " & "))
  return(event_names)
}


# Function to split an even number into a chosen number of power-of-two summands.
.splitEvenToPowers <- function(df, cohortDefinitionSet) {
  # Set variables
  comboId <- df$comboId
  isCombo <- df$isCombo
  numberOfEvents <- df$numberOfEvents
  
  # Create empty data frame
  data_frame <- data.frame(
    comboId = integer(),
    isCombo = integer(),
    numberOfEvents = integer(),
    splitNumbers = character()
  )
  
  # Loop through rows of the data frame
  for (i in 1:nrow(df)) {
    # If isCombo is 0, simply return the input number without splitting
    if (isCombo[i] == 0) {
      # Create data frame
      data_frame_no_combos <- data.frame(
        comboId = comboId[i],
        numberOfEvents = numberOfEvents[i],
        isCombo = isCombo[i],
        splitNumbers = as.character(comboId[i])
      )
      
      # Append rows to data frame
      data_frame <- rbind(data_frame, data_frame_no_combos)
      
      
    } else {
      # Check if the number is even
      if (comboId[i] %% 2 != 0) {
        stop("Please input an even number!")
      }
      
      # Get the binary representation as a vector (least-significant bit first)
      bits <- as.integer(intToBits(comboId[i]))
      
      # Identify positions where the bit is 1 (subtract one for exponent)
      exponents <- which(bits == 1) - 1
      
      # For even numbers, ignore the 2^0 component (which equals 1)
      exponents <- exponents[exponents != 0]
      
      # Compute the corresponding powers of two from the exponents
      powers <- 2^exponents
      
      # Sort the summands in descending order (largest first)
      current_parts <- sort(powers, decreasing = TRUE)
      
      # Define minimal and maximum possible parts for a valid split
      min_parts <- length(current_parts)
      max_parts <- comboId[i] / 2  # since the smallest summand allowed is 2
      
      if (numberOfEvents[i] < min_parts) {
        stop(
          paste(
            "The minimal splitting has",
            min_parts,
            "numberOfEvents. Cannot merge components further."
          )
        )
      }
      
      if (numberOfEvents[i] > max_parts) {
        stop(paste(
          "The maximal splitting into powers >1 is",
          max_parts,
          "numberOfEvents."
        ))
      }
      
      # Iteratively split the summands until the desired number of parts is reached
      while (length(current_parts) < numberOfEvents[i]) {
        # We cannot split further if every summand is 2
        if (all(current_parts == 2)) {
          stop("Cannot further split without producing ones.")
        }
        
        # Choose the largest summand that is greater than 2
        candidates <- current_parts[current_parts > 2]
        idx <- which(current_parts == max(candidates))[1]
        value_to_split <- current_parts[idx]
        
        # Replace the chosen summand with two equal halves
        current_parts <- current_parts[-idx]   # Remove the selected summand
        current_parts <- c(current_parts, value_to_split / 2, value_to_split / 2)
        
        # Resort in descending order for consistency.
        current_parts <- sort(current_parts, decreasing = TRUE)
      }
      
      # Convert the numeric vector to a single string, with elements separated by commas
      result_string <- paste(current_parts, collapse = ",")
      
      # Create data frame
      data_frame_with_combos <- data.frame(
        comboId = comboId[i],
        numberOfEvents = numberOfEvents[i],
        isCombo = isCombo[i],
        splitNumbers = result_string
      )
      
      # Append values to data frame
      data_frame <- rbind(data_frame, data_frame_with_combos)
      
    }
  }
  
  # Maximum number of columns to create
  max_cols <- max(unique(data_frame$numberOfEvents))
  
  # Create a vector of column names
  new_colnames <- paste0("eventCohortCode_", 1:max_cols)
  
  # Convert the vector of column names to a data frame
  df <- setNames(data.frame(matrix(
    ncol = length(new_colnames), nrow = 0
  )), new_colnames)
  
  # Add NA in all rows (no. of rows is equal to the length of the original data frame)
  df[nrow(data_frame), ] <- NA
  
  # Bind empty data frame columns back to the original data frame
  data_frame <- cbind(data_frame, df)
  
  # Split "splitNumbers" column to multiple columns
  data_frame <- tidyr::separate(
    data_frame,
    col = splitNumbers,
    into = new_colnames,
    sep = ",",
    convert = TRUE,
    remove = FALSE
  )
  
  # Get event cohort ids of and code (comboId)
  eventCohortIdAndCode <- cohortPathwaysResults[["pathwayAnalysisCodesLong"]] |>
    dplyr::filter(isCombo == 0) |>
    dplyr::select(c(eventCohortId, code))
  
  
  # Join event cohort id to main data frame
  data_frame <- data_frame |>
    dplyr::left_join(eventCohortIdAndCode, by = c("eventCohortCode_1" = "code")) |>
    dplyr::rename(eventCohortId_1 = eventCohortId) |>
    dplyr::left_join(eventCohortIdAndCode, by = c("eventCohortCode_2" = "code")) |>
    dplyr::rename(eventCohortId_2 = eventCohortId) |>
    dplyr::left_join(eventCohortIdAndCode, by = c("eventCohortCode_3" = "code")) |>
    dplyr::rename(eventCohortId_3 = eventCohortId) |>
    dplyr::left_join(eventCohortIdAndCode, by = c("eventCohortCode_4" = "code")) |>
    dplyr::rename(eventCohortId_4 = eventCohortId)
  
  cohortDefinitionSet <- cohortDefinitionSet |>
    dplyr::select(c(cohortId, cohortName))
  
  # Join event cohort name to main data frame
  data_frame <- data_frame |>
    dplyr::left_join(cohortDefinitionSet, by = c("eventCohortId_1" = "cohortId")) |>
    dplyr::rename(eventCohortName_1 = cohortName) |>
    dplyr::left_join(cohortDefinitionSet, by = c("eventCohortId_2" = "cohortId")) |>
    dplyr::rename(eventCohortName_2 = cohortName) |>
    dplyr::left_join(cohortDefinitionSet, by = c("eventCohortId_3" = "cohortId")) |>
    dplyr::rename(eventCohortName_3 = cohortName) |>
    dplyr::left_join(cohortDefinitionSet, by = c("eventCohortId_4" = "cohortId")) |>
    dplyr::rename(eventCohortName_4 = cohortName)
  
  # Create path name and comboId map
  data_frame <- data_frame |>
    tidyr::unite(
      col = "pathName",
      eventCohortName_1:eventCohortName_4,
      sep = " | ",
      na.rm = TRUE
    ) |>
    dplyr::select(c(comboId, pathName))
  
  
  return(data_frame)
}