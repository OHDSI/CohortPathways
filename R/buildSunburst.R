#' Create a Sunburst Plot for Treatment Pathways
#'
#' This function creates an interactive sunburst plot to visualize treatment pathways
#' from CohortPathway analysis results. The plot shows the hierarchical structure of
#' treatment sequences, with each ring representing a step in the pathway.
#'
#' @param cpResults A list containing the results from CohortPathway analysis.
#'        Must include 'pathwaysAnalysisPathsData' and 'isCombo' data frames.
#' @param generationSet A data frame containing information about event cohorts
#'        that will be used to generate descriptive names for the events in the diagram.
#' @param nPaths Integer specifying the maximum number of steps to include in the plot.
#' @param minCount Integer specifying the minimum count value for a path to be included.
#' @param debug Logical indicating whether to print debug information.
#'
#' @return An HTML widget object containing the interactive sunburst plot.
#' @export
createPathwaySunburst <- function(
    cpResults,
    generationSet,
    nPaths = 3,
    minCount = 5,
    debug = TRUE) {
  
  # Input validation
  checkmate::assertList(
    cpResults,
    min.len = 7,
    types = 'data.frame'
  )
  
  # Extract required data
  pathwaysAnalysisPathsData <- purrr::pluck(
    cpResults, 'pathwaysAnalysisPathsData'
  )
  
  isCombo <- purrr::pluck(
    cpResults, 'isCombo'
  )
  
  # Check inputs
  checkmate::assertDataFrame(
    x = pathwaysAnalysisPathsData,
    min.rows = 1,
    min.cols = 12
  )
  checkmate::assertDataFrame(
    x = isCombo,
    min.rows = 1,
    min.cols = 1
  )
  
  if(debug) {
    cat("Number of pathway rows:", nrow(pathwaysAnalysisPathsData), "\n")
    cat("First few rows of pathwaysAnalysisPathsData:\n")
    print(head(pathwaysAnalysisPathsData))
  }
  
  # Get event names
  eventNames <- .prepareEventNames(generationSet, cpResults)
  
  if(debug) {
    cat("Number of event names:", nrow(eventNames), "\n")
    cat("First few event names:\n")
    print(head(eventNames))
  }
  
  # Create a mapping of codes to event names
  code_to_name <- stats::setNames(
    eventNames$combination, 
    as.character(eventNames$code)
  )
  
  # Create a mapping of comboIds to isCombo values
  combo_map <- stats::setNames(isCombo$isCombo, as.character(isCombo$comboId))
  
  # Create a mapping of comboIds to numberOfEvents
  events_map <- stats::setNames(isCombo$numberOfEvents, as.character(isCombo$comboId))
  
  # Function to get a descriptive name for a code
  get_name <- function(code) {
    if (code == "Start") return("Start")
    
    # Check if we have a custom name for this event
    if (code %in% names(code_to_name)) {
      name <- code_to_name[code]
      
      # Add combo information if available
      if (code %in% names(combo_map) && !is.na(combo_map[code]) && combo_map[code] == 1) {
        if (code %in% names(events_map) && !is.na(events_map[code])) {
          name <- paste0(name, " (", events_map[code], " events)")
        }
      }
      return(name)
    } else {
      # Use default formatting if no custom name is available
      if (code %in% names(combo_map) && !is.na(combo_map[code]) && combo_map[code] == 1) {
        if (code %in% names(events_map) && !is.na(events_map[code])) {
          return(paste0("Combo ", code, " (", events_map[code], " events)"))
        } else {
          return(paste0("Combo ", code))
        }
      } else {
        return(paste0("Event ", code))
      }
    }
  }
  
  # Prepare data for sunburst plot in the format required by sunburstR
  sequences <- data.frame(
    pathId = integer(),
    step = integer(),
    name = character(),
    stringsAsFactors = FALSE
  )
  
  # Process each pathway row
  for (i in 1:nrow(pathwaysAnalysisPathsData)) {
    # Start with "root" for each pathway
    sequences <- rbind(
      sequences,
      data.frame(
        pathId = i,
        step = 0,
        name = "root",
        stringsAsFactors = FALSE
      )
    )
    
    # Add "Start" as the first step
    sequences <- rbind(
      sequences,
      data.frame(
        pathId = i,
        step = 1,
        name = "Start",
        stringsAsFactors = FALSE
      )
    )
    
    # Add subsequent steps
    step_count <- 2  # Start from step 2 (after "Start")
    for (j in 1:nPaths) {
      col_name <- paste0("step", j)
      if (col_name %in% colnames(pathwaysAnalysisPathsData) && 
          !is.na(pathwaysAnalysisPathsData[[col_name]][i])) {
        code <- as.character(pathwaysAnalysisPathsData[[col_name]][i])
        name <- get_name(code)
        
        sequences <- rbind(
          sequences,
          data.frame(
            pathId = i,
            step = step_count,
            name = name,
            stringsAsFactors = FALSE
          )
        )
        step_count <- step_count + 1
      }
    }
  }
  
  if(debug) {
    cat("Number of sequence rows:", nrow(sequences), "\n")
    cat("First few sequence rows:\n")
    print(head(sequences))
  }
  
  # Create sequence strings and values dataframe
  sunburst_data <- data.frame(
    sequence = character(),
    value = numeric(),
    stringsAsFactors = FALSE
  )
  
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
        sunburst_data <- rbind(
          sunburst_data,
          data.frame(
            sequence = sequence,
            value = count_value,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  
  if(debug) {
    cat("Number of sunburst data rows:", nrow(sunburst_data), "\n")
    cat("First few sunburst data rows:\n")
    print(head(sunburst_data))
  }
  
  # Ensure we have data
  if (nrow(sunburst_data) == 0) {
    stop("No pathways meet the minimum count threshold.")
  }
  
  # Aggregate identical sequences
  sunburst_data <- sunburst_data |>
    dplyr::group_by(sequence) |>
    dplyr::summarise(value = sum(value), .groups = "drop")
  
  if(debug) {
    cat("Number of aggregated sunburst data rows:", nrow(sunburst_data), "\n")
    cat("First few aggregated sunburst data rows:\n")
    print(head(sunburst_data))
  }
  
  # Try a simpler approach using d3r
  # Convert to hierarchical format
  hierarchy <- d3r::d3_nest(
    sunburst_data,
    value_cols = "value",
    root = "Treatment Pathways"
  )
  
  if(debug) {
    cat("Hierarchy structure:\n")
    str(hierarchy)
  }
  
  # Create the sunburst plot using sunburstR
  sunburst <- sunburstR::sunburst(
    data = sunburst_data,
    count = TRUE
  )
  return(sunburst)
}