#' Create a Sankey Diagram for Treatment Pathways
#'
#' This function creates an interactive Sankey diagram to visualize treatment pathways
#' from CohortPathway analysis results. The diagram shows the flow of patients between
#' different treatments or treatment combinations.
#'
#' @param cpResults A list containing the results from CohortPathway analysis.
#'        Must include 'pathwaysAnalysisPathsData' and 'isCombo' data frames.
#'        'pathwaysAnalysisPathsData' should have columns named 'step1', 'step2', etc.,
#'        and a 'countValue' column indicating the number of patients following each path.
#'        'isCombo' should have columns 'comboId', 'isCombo', and 'numberOfEvents'.
#' @param generationSet A  data frame  containing information about event cohorts
#'        that will be used to generate descriptive names for the events in the diagram.        
#'
#' @return An HTML widget object containing the interactive Sankey diagram.
#' @export
#'
#' @examples
#' \dontrun{
#' # With generation set for event names
#' sankey <- createPathwaySankey(
#'   cpResults = pathwayResults,
#'   generationSet = cohortDefinitions
#' )
#'
#' # Display the diagram
#' sankey
#'
#' # Save the diagram to an HTML file
#' htmlwidgets::saveWidget(sankey, "pathway_diagram.html", selfcontained = TRUE)
#' }
createPathwaySankey <- function(
    cpResults,
    generationSet,
    nPaths = 3) {
  checkmate::assertList(
    cpResults,
    min.len = 7,
    types = 'data.frame'
  )
  pathwaysAnalysisPathsData <- purrr::pluck(
    cpResults, 'pathwaysAnalysisPathsData'
  ) |> 
    filter(targetCohortId == 4)
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
  
  eventNames <- .prepareEventNames(generationSet, cpResults)
  
  # Extract steps and create links
  links <- tibble(
    source = character(), 
    target = character(), 
    value = numeric()
  )
  
  # Process each pathway row
  for(i in 1:nrow(pathwaysAnalysisPathsData)) {
    # Get steps for this pathway
    steps <- c()
    for(j in 1:nPaths) {  
      col_name <- paste0("step", j)
      if(col_name %in% colnames(pathwaysAnalysisPathsData) && !is.na(pathwaysAnalysisPathsData[[col_name]][i])) {
        steps <- c(steps, as.character(pathwaysAnalysisPathsData[[col_name]][i]))
      }
    }
    
    # Remove duplicates to prevent cycles
    steps <- unique(steps)
    
    # If we have steps, create links
    if(length(steps) > 0) {
      # Add start to first step
      links <- rbind(links, dplyr::tibble(
        source = "Start",
        target = steps[1],
        value = pathwaysAnalysisPathsData$countValue[i]
      ))
      
      # Add links between consecutive steps, but only in forward direction
      if(length(steps) > 1) {
        for(k in 1:(length(steps)-1)) {
          # Check if this is a forward progression (no recursion)
          # We consider it forward if the step index is higher
          links <- rbind(links, tibble(
            source = steps[k],
            target = steps[k+1],
            value = pathwaysAnalysisPathsData$countValue[i]
          ))
        }
      }
    }
  }
  
  # Aggregate links with the same source and target
  links <- links %>%
    dplyr::group_by(.data$source, .data$target) %>%
    summarize(value = sum(.data$value), .groups = 'drop') |> 
    dplyr::filter(.data$value > 5)
  
  # Create nodes dataframe
  all_nodes <- unique(c(links$source, links$target))
  
  nodes <- data.frame(name = unique(c(links$source, links$target)))
  nodes <- data.table::data.table(nodes)
  nodes$name <- sub('__[0-9]+$', '', nodes$name)
  # Add group information (start, combo, single)
  nodes$group <- "single"  # Default
  nodes$group[nodes$name == "Start"] <- "start"
  
  # Create a mapping of comboIds to isCombo values
  combo_map <- stats::setNames(isCombo$isCombo, as.character(isCombo$comboId))
  
  # Create a mapping of comboIds to numberOfEvents
  events_map <- stats::setNames(isCombo$numberOfEvents, as.character(isCombo$comboId))
  
  # Update nodes with combo information
  for(i in 1:nrow(nodes)) {
    if(nodes$name[i] != "Start") {
      # Check if this node is in the combo map
      if(nodes$name[i] %in% names(combo_map)) {
        is_combo_val <- combo_map[nodes$name[i]]
        if(!is.na(is_combo_val) && is_combo_val == 1) {
          nodes$group[i] <- "combo"
          
          # Add number of events if available
          if(nodes$name[i] %in% names(events_map)) {
            num_events <- events_map[nodes$name[i]]
            if(!is.na(num_events)) {
              nodes$events[i] <- num_events
            }
          }
        }
      }
    }
  }
  
  # Create better labels using custom event names if provided
  nodes$label <- nodes$name
  for(i in 1:nrow(nodes)) {
    if(nodes$name[i] == "Start") {
      nodes$label[i] <- "Start"
    } else {
      # Get the event code
      event_code <- nodes$name[i]
      
      # Check if we have a custom name for this event
      if(!is.null(eventNames) && event_code %in% eventNames$code) {
        custom_name <- eventNames[eventNames$code == event_code, ]$combination
        
        # Format based on whether it's a combo or not
        if(nodes$group[i] == "combo" && !is.na(nodes$events[i])) {
          nodes$label[i] <- paste0(custom_name, " (", nodes$events[i], " events)")
        } else {
          nodes$label[i] <- custom_name
        }
      } else {
        # Use default formatting if no custom name is available
        if(nodes$group[i] == "combo" && !is.na(nodes$events[i])) {
          nodes$label[i] <- paste0("Combo ", event_code, " (", nodes$events[i], " events)")
        } else {
          nodes$label[i] <- paste0(ifelse(nodes$group[i] == "combo", "Combo ", "Event "), event_code)
        }
      }
    }
  }
  
  # Create node index mapping
  node_indices <- 0:(nrow(nodes) - 1)
  names(node_indices) <- nodes$name
  
  # Convert source and target to indices
  links$source <- node_indices[links$source]
  links$target <- node_indices[links$target]
  
  # Create color scale
  color_scale <- htmlwidgets::JS(
    paste0('d3.scaleOrdinal()
           .domain(["start", "single", "combo"])
           .range(["#2ca02c", "#1f77b4", "#ff7f0e"])')
  )
  Nodes <- dplyr::tibble(name = nodes$label, group = nodes$group)
  
  links$type <- sub(' .*', '',
                    as.data.frame(nodes)[links$source + 1, 'name'])
  
  label <- unique(links$type)
  label2 <- paste0("'", paste(label, collapse = "','"), "',", "'end'")
  
  kelly_colors <- unname(grafify::graf_palettes$kelly)[-1]
  
  col <- kelly_colors[seq_along(label)]
  col2 <- paste0("'", paste(col, collapse = "','"), "',", "'#1B1919FF'")
  
  myCol <- glue::glue('d3.scaleOrdinal() .domain([{label2}]) .range([{col2}])')
  
  # Create Sankey diagram
  sankey <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = Nodes,
    Source = 'source',
    Target = 'target',
    Value = 'value',
    NodeID = 'name',
    NodeGroup = "group",
    colourScale = myCol,
    fontSize = 12,
    nodeWidth = 30,
    nodePadding = 15,
    height = 600,
    width = 1000,
    sinksRight = FALSE
  )
  
  # Add styling and legend
  sankey <- htmlwidgets::onRender(
    sankey,
    '
    function(el, x) {
      // Add title
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d3.select(el).select("svg").attr("width") / 2)
        .attr("y", 30)
        .attr("text-anchor", "middle")
        .style("font-size", "18px")
        .style("font-weight", "bold")
        .text("Patient Treatment Pathway Analysis");
      
      // Format numbers with commas
      var formatNumber = d3.format(",.0f");
      
      // Add tooltips to links
      d3.select(el).selectAll(".link")
        .append("title")
        .text(function(d) { 
          return d.source.name + " → " + d.target.name + "\\n" + formatNumber(d.value) + " patients";
        });
        
      // Add legend
      var svg = d3.select(el).select("svg");
      var legend = svg.append("g")
        .attr("class", "legend")
        .attr("transform", "translate(20, 60)");
        
      // Legend items
      var legendItems = [
        {label: "Start", color: "#2ca02c"},
        {label: "Single Event", color: "#1f77b4"},
        {label: "Combination Event", color: "#ff7f0e"}
      ];
      
      // Add legend rectangles and text
      legendItems.forEach(function(item, i) {
        legend.append("rect")
          .attr("x", 0)
          .attr("y", i * 20)
          .attr("width", 15)
          .attr("height", 15)
          .style("fill", item.color);
          
        legend.append("text")
          .attr("x", 20)
          .attr("y", i * 20 + 12)
          .text(item.label)
          .style("font-size", "12px");
      });
    }
    '
  )
  return(sankey)
}


.prepareEventNames <- function(generationSet, cpResults) {
  event_names <- purrr::pluck(
    cpResults, 'pathwayAnalysisCodesLong'
  ) |> select(.data$code, .data$eventCohortId) |> 
    dplyr::distinct() |> 
    inner_join(generationSet |> 
                 select(cohortId, cohortName), by = join_by(
                   eventCohortId == cohortId
                 )) |> 
    group_by(code) |>
    dplyr::reframe(
      combination = paste(cohortName, collapse = '-and-')
    )
  return(event_names)
}
