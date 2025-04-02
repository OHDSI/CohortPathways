# library(dplyr)
# 
# # Load the pathways data
# pathways <- r$pathwaysAnalysisPathsData
# 
# # Reshape the data so that we can create a hierarchical structure
# sunburst_data <- pathways %>%
#   # Unite all steps into a single hierarchical "path"
#   tidyr::unite(path, step1, step2, step3, step4, step5, step6, step7, step8, step9, step10, 
#                sep = "-", na.rm = TRUE) %>%
#   # Count values for each unique path
#   group_by(path) %>%
#   summarise(countValue = sum(countValue), .groups = "drop")
# 
# # View processed data
# print(sunburst_data)
# 
# 
# library(plotly)
# 
# # Construct the sunburst data in three columns: labels, parents, and values (countValue)
# library(dplyr)
# library(tidyr)
# 
# # Create paths_split by splitting `path` into hierarchy levels
# sunburst_data <- sunburst_data %>%
#   mutate(paths_split = strsplit(as.character(path), "-"))  # Split the `path` column into hierarchical levels
# # Unnest hierarchy into levels
# 
# sunburst_data <- sunburst_data %>%
#   tidyr::unnest_wider(paths_split, 
#                       names_sep = "_")  # Create hierarchical columns (e.g., paths_split_1, paths_split_2, ...)
# 
# # Create hierarchical structure for Sunburst
# sunburst_df <- sunburst_data %>%
#   tidyr::pivot_longer(
#     cols = starts_with("paths_split"),    # Hierarchical columns
#     names_to = "level",                   # Level column
#     values_to = "label",                  # Node labels
#     values_drop_na = TRUE                 # Drop NA labels
#   ) %>%
#   group_by(path) %>%
#   mutate(
#     parent = lag(label, default = NA),    # Set parent relationships
#     value = first(countValue)            # Carry over the countValue for all rows in the path
#   ) %>%
#   ungroup() %>%
#   select(label, parent, value) %>%
#   filter(!is.na(label))                  # Ensure no empty labels
# 
# library(plotly)
# sunburst_df %>% 
#   distinct(label, parent, value) %>% 
#   print()
# 
# sunburst_df %>% 
#   filter(is.na(label)) %>% 
#   print()  
# 
# sunburst_df %>% 
#   filter(is.na(parent)) %>% 
#   print()
# 
# library(plotly)
# 
# # Generate Sunburst plot
# fig <- plot_ly(
#   data = sunburst_df,
#   type = 'sunburst',
#   labels = ~label,   # Node labels (steps in the pathway hierarchy)
#   parents = ~parent, # Parent nodes (hierarchical relationships)
#   values = ~value,   # Numeric flow sizes
#   branchvalues = 'total'  # Nodes show cumulative values
# )
# 
# # Add plot configuration and display
# fig <- fig %>% layout(title = "Cohort Pathways Sunburst Chart")
# fig
