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
#' @param numberOfPaths Integer specifying the maximum number of steps to include in the plot.
#' @param minCount Integer specifying the minimum count value for a path to be included.
#' @return An HTML widget object containing the interactive sunburst plot.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(CohortPathway)
#' sunburstPlot <- CohortPathways::createPathwaySunburst(cohortPathwayResults, cohortsToCreate)
#' }
createPathwaySunburst <- function(cohortPathwayResults,
                                  cohortDefinitionSet,
                                  numberOfPaths = 3,
                                  minCount = 5) {
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
  eventNames <- .prepareEventNames(cohortDefinitionSet, cohortPathwayResults)
  
  # Create a mapping of codes to event names
  codeToName <- rlang::set_names(eventNames$combination, as.character(eventNames$code))
  
  # Create a mapping of comboIds to isCombo values
  comboMap <- rlang::set_names(isCombo$isCombo, as.character(isCombo$comboId))
  
  # Create a mapping of comboIds to numberOfEvents
  eventsMap <- rlang::set_names(isCombo$numberOfEvents, as.character(isCombo$comboId))
  
  # Function to get a descriptive name for a code
  get_name <- function(code) {
    if (code == "Start") {
      return("Start")
    }
    
    # Check if we have a custom name for this event
    if (code %in% names(codeToName)) {
      name <- codeToName[code]
      
      # Add combo information if available
      if (code %in% names(comboMap) &&
          !is.na(comboMap[code]) && comboMap[code] == 1) {
        if (code %in% names(eventsMap) && !is.na(eventsMap[code])) {
          name <- paste0(name, " (", eventsMap[code], " events)")
        }
      }
      return(name)
    } else {
      # Use default formatting if no custom name is available
      if (code %in% names(comboMap) &&
          !is.na(comboMap[code]) && comboMap[code] == 1) {
        if (code %in% names(eventsMap) && !is.na(eventsMap[code])) {
          return(paste0("Combo ", code, " (", eventsMap[code], " events)"))
        } else {
          return(paste0("Combo ", code))
        }
      } else {
        return(paste0("Event ", code))
      }
    }
  }
  
  targetNames <- purrr::map_chr(seq_along(pathwaysAnalysisPathsDatas), function(xx) {
    tId <- purrr::pluck(pathwaysAnalysisPathsDatas[[xx]], "targetCohortId") |>
      unique()
    cohortDefinitionSet |>
      dplyr::filter(.data$cohortId %in% tId) |>
      dplyr::pull(.data$cohortName) |>
      unique()
  })
  
  .plots <- lapply(seq_along(pathwaysAnalysisPathsDatas), function(.x) {
    pathwaysAnalysisPathsData <- pathwaysAnalysisPathsDatas[[.x]]
    
    
    # Prepare data for sunburst plot in the format required by sunburstR
    sequences <- dplyr::tibble(pathId = integer(),
                               step = integer(),
                               name = character())
    # Process each pathway row
    for (i in 1:nrow(pathwaysAnalysisPathsData)) {
      # Start with "root" for each pathway
      
      sequences <- rbind(sequences, dplyr::tibble(
        pathId = i,
        step = 0,
        name = "root"
      ))
      # Add "Start" as the first step
      sequences <- rbind(sequences,
                         dplyr::tibble(
                           pathId = i,
                           step = 1,
                           name = "Start"
                         ))
      
      # Add subsequent steps
      step_count <- 2 # Start from step 2 (after "Start")
      for (j in 1:numberOfPaths) {
        col_name <- paste0("step", j)
        if (col_name %in% colnames(pathwaysAnalysisPathsData) &&
            !is.na(pathwaysAnalysisPathsData[[col_name]][i])) {
          code <- as.character(pathwaysAnalysisPathsData[[col_name]][i])
          name <- get_name(code)
          
          sequences <- rbind(sequences,
                             dplyr::tibble(
                               pathId = i,
                               step = step_count,
                               name = name
                             ))
          step_count <- step_count + 1
        }
      }
    }
    sunburstData <- dplyr::tibble(sequence = character(), value = numeric())
    
    # Process each pathway
    for (i in unique(sequences$pathId)) {
      # Get steps for this pathway
      path_steps <- sequences[sequences$pathId == i, ]
      path_steps <- path_steps[order(path_steps$step), ]
      
      # Skip root, start with actual steps
      if (nrow(path_steps) > 1) {
        # Create sequence string (skip "root")
        seq_names <- path_steps$name[path_steps$name != "root"]
        sequence <- paste(seq_names, collapse = "-")
        
        # Get count value for this pathway
        count_value <- pathwaysAnalysisPathsData$countValue[i]
        if (!is.na(count_value) && count_value >= minCount) {
          sunburstData <- rbind(sunburstData,
                                dplyr::tibble(sequence = sequence, value = count_value))
        }
      }
    }
    # Ensure we have data
    if (nrow(sunburstData) == 0) {
      stop("No pathways meet the minimum count threshold.")
    }
    
    # Aggregate identical sequences
    sunburstData <- sunburstData |>
      dplyr::group_by(.data$sequence) |>
      dplyr::summarize(value = sum(.data$value), .groups = "drop")
    
    # Create the sunburst plot using sunburstR
    sunburst <- sunburstR::sunburst(
      data = sunburstData,
      count = TRUE,
      withD3 = TRUE,
      legend = list(
        w = 490,
        h = 50,
        r = 100,
        s = 5
      )
    )
    sunburst <- htmlwidgets::onRender(
      sunburst,
      "function(el, x) {
      // Make legend visible by default
      d3.select(el).select('.sunburst-togglelegend').property('checked', true);
      d3.select(el).select('.sunburst-legend').style('visibility', '');
    }"
    )
    return(sunburst)
  })
  return(.plots |> rlang::set_names(targetNames))
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