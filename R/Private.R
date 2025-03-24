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

#' Create a Folder if It Does Not Exist
#'
#' This function creates a folder (directory) at a specified location if it does not already exist.
#' It also verifies that the folder exists and is accessible by asserting its execute permissions.
#' Any issues or error messages can be collected using an AssertCollection from the \code{checkmate} package.
#'
#' @param type A character string specifying the type of entity to create. Currently, only `"folder"` is supported.
#' @param name A character string specifying the name or path where the folder should be created.
#' @param recursive A logical value indicating whether to create the directory recursively (i.e., creating any necessary parent directories). Defaults to \code{TRUE}.
#' @param errorMessage An optional AssertCollection object (from the \code{checkmate} package) used to collect error messages. If \code{NULL} or not a valid AssertCollection, a new one is created.
#'
#' @return Invisibly returns the AssertCollection object containing any collected error messages.
#'
#' @details
#' The function first ensures that an appropriate AssertCollection object is available. If \code{type} is not \code{NULL}, it checks if
#' the \code{name} parameter is non-empty. If the \code{type} is `"folder"`, the function removes any trailing slashes from \code{name}
#' and checks if the directory exists. If the directory does not exist, it is created using \code{dir.create} with the specified \code{recursive}
#' setting. A message is printed to indicate the creation of the folder. Finally, it asserts that the directory exists and is executable.
#'
#' @examples
#' \dontrun{
#' # Create a folder "data" if it does not already exist
#' createIfNotExist("folder", "data")
#' }
#'
createIfNotExist <- function(type,
                             name,
                             recursive = TRUE,
                             errorMessage = NULL) {
  if (is.null(errorMessage) |
    !inherits(errorMessage, "AssertCollection")) {
    errorMessage <- checkmate::makeAssertCollection()
  }
  if (!is.null(type)) {
    if (length(name) == 0) {
      stop(message("Must specify ", name))
    }
    if (type %in% c("folder")) {
      if (!file.exists(gsub("/$", "", name))) {
        dir.create(
          path = name,
          showWarnings = FALSE,
          recursive = recursive
        )
        message("Created ", type, " at ", name)
      }
    }
    checkmate::assertDirectory(
      x = name,
      access = "x",
      add = errorMessage
    )
  }
  invisible(errorMessage)
}


#' Extract Bit Sum
#'
#' This function takes a positive integer and returns a numeric vector of bit positions
#' (using 0-indexing) that represent the powers of 2 summing to the input integer.
#' Essentially, it decomposes the number into its binary representation and returns
#' the indices of the bits that are set to 1.
#'
#' @param x A positive integer. The number to be decomposed into a sum of powers of 2.
#'
#' @return A numeric vector containing the bit positions (0-indexed) corresponding to
#'         the powers of 2 that sum up to \code{x}.
#'
#' @details
#' The function first computes an upper bound for the exponent needed to represent \code{x}
#' in binary using \code{round(log(x, base = 2) + 2)}. It then creates a vector \code{series}
#' containing powers of 2 from \eqn{2^0} up to \eqn{2^{lengthVar}}. In the loop, it repeatedly
#' finds the largest power of 2 (from the series) that is less than or equal to the current
#' remainder, subtracts that value, and records the corresponding bit position (adjusted for 0-indexing).
#'
#' @examples
#' extractBitSum(10)
#' # Returns: c(3, 1) because 10 = 2^3 + 2^1 (binary 1010)
#'
extractBitSum <- function(x) {
  # Calculate an upper bound for the exponent needed (adding 2 for safety).
  lengthVar <- round(log(x = x, base = 2) + 2)

  # Create a vector of powers of 2: 2^0, 2^1, ..., 2^(lengthVar)
  series <- c(2^(0:lengthVar))

  # Initialize remainder to x and an empty vector to store the bit positions.
  remainder <- x
  combination <- c()

  # While there is still a value to decompose:
  while (remainder != 0) {
    # Identify the index of the smallest power in 'series' that is greater than the remainder.
    # Subtracting 1 gives the index corresponding to the largest power of 2 <= remainder.
    component <- match(TRUE, series > remainder) - 1

    # Subtract the corresponding power of 2 from the remainder.
    remainder <- remainder - series[component]

    # Append the adjusted index (component - 1) to the result.
    # This adjustment converts the index to 0-based, matching standard binary digit positions.
    combination[length(combination) + 1] <- component - 1
  }

  # Return the vector of bit positions.
  return(combination)
}



#' Format a Number with Commas and Specified Decimal Places
#'
#' This function formats a numeric value into a string by inserting commas as thousand separators
#' and formatting the decimal portion to a specified number of digits. It can either round or truncate
#' the decimal part based on the \code{round} parameter.
#'
#' @param number A numeric value to be formatted.
#' @param decimalPlaces An integer specifying the number of digits to display after the decimal point.
#'   Defaults to \code{1}.
#' @param round A logical value indicating whether the decimal portion should be rounded. If \code{FALSE},
#'   the decimal part will be truncated instead. Defaults to \code{TRUE}.
#'
#' @return A character string representing the formatted number with commas as thousand separators and
#'   a period separating the integer and decimal parts.
#'
#' @details
#' The function separates the number into its integer and decimal components. The integer part is formatted
#' with commas using \code{formatC}, while the decimal part is either rounded or truncated according to the
#' \code{decimalPlaces} parameter. The decimal portion is then extracted from the formatted string and concatenated
#' with the formatted integer part to produce the final output.
#'
#' @examples
#' # Example with rounding (default)
#' formatDecimalWithComma(1234567.8912)
#' # Might return "1,234,567.9"
#'
#' # Example with truncation and two decimal places
#' formatDecimalWithComma(1234567.8912, decimalPlaces = 2, round = FALSE)
#' # Might return "1,234,567.89"
#'
formatDecimalWithComma <-
  function(number,
           decimalPlaces = 1,
           round = TRUE) {
    integerPart <- floor(number)
    decimalPart <- number - integerPart

    if (round) {
      decimalPart <- round(decimalPart, decimalPlaces)
    } else {
      decimalPart <-
        trunc(decimalPart * 10^decimalPlaces) / 10^decimalPlaces
    }

    formattedIntegerPart <-
      formatC(integerPart, format = "d", big.mark = ",")
    decimalPartAsString <-
      formatC(decimalPart, format = "f", digits = decimalPlaces)
    formattedDecimalPart <-
      substr(decimalPartAsString, 3, nchar(decimalPartAsString))

    return(paste(formattedIntegerPart, formattedDecimalPart, sep = "."))
  }




#' Calculate Summary Statistics by Group
#'
#' This function computes a variety of summary statistics for a specified numeric column in a data frame. It
#' calculates the mean, standard deviation, median, several
#' quantiles, the mode, the total count, and the count of distinct values. The final output is returned in a
#' long format for easier inspection and downstream processing.
#'
#' @param df A data frame containing the data for which the summary statistics will be computed.
#' @param value A character string specifying the name of the numeric column to summarize.
#'   Defaults to \code{"value"}.
#'
#' @return A tibble (data frame) in long format with two columns:
#'   \item{statistic}{The name of the summary statistic (e.g., mean, sd, median, quantiles, mode, count, count_distinct).}
#'   \item{value}{The value of the corresponding summary statistic.}
#'
#' @details
#'
#' A helper function \code{calculateMode} is defined to determine the mode of the numeric column, i.e., the most
#' frequently occurring value.
#'
#' The data is grouped by the specified \code{group} column, and summary statistics are computed for the \code{value}
#' column including:
#'
#' \itemize{
#'   \item \code{mean}: The average of the values.
#'   \item \code{sd}: The standard deviation of the values.
#'   \item \code{median}: The median value.
#'   \item \code{p01}: The 1st percentile.
#'   \item \code{p05}: The 5th percentile.
#'   \item \code{p25}: The 25th percentile.
#'   \item \code{p75}: The 75th percentile.
#'   \item \code{p95}: The 95th percentile.
#'   \item \code{p99}: The 99th percentile.
#'   \item \code{mode}: The most frequently occurring value.
#'   \item \code{count}: The number of observations.
#'   \item \code{count_distinct}: The number of distinct values.
#' }
#'
#' Finally, the summary table is reshaped into a long format using \code{tidyr::pivot_longer}, so that each
#' row corresponds to a single statistic for each group.
#'
#' @examples
#' \dontrun{
#' # Create example data
#' df <- data.frame(
#'   value = rnorm(200)
#' )
#'
#' summary_stats <- calculateSummaryStatistics(df, value = "value")
#' print(summary_stats)
#' }
#'
#' @export
calculateSummaryStatistics <- function(df, value = "value") {
  # Select the target column and rename it to "my_value"
  dataFrame <- df |>
    dplyr::select(my_value = dplyr::all_of(value))

  # Helper function to calculate mode
  calculateMode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  # Calculate summary statistics, explicitly referencing the column via .data[["my_value"]]
  output <- dataFrame |>
    dplyr::summarize(
      mean = mean(.data[["my_value"]], na.rm = TRUE),
      sd = sd(.data[["my_value"]], na.rm = TRUE),
      median = median(.data[["my_value"]], na.rm = TRUE),
      p01 = quantile(.data[["my_value"]], 0.01, na.rm = TRUE),
      p05 = quantile(.data[["my_value"]], 0.05, na.rm = TRUE),
      p25 = quantile(.data[["my_value"]], 0.25, na.rm = TRUE),
      p75 = quantile(.data[["my_value"]], 0.75, na.rm = TRUE),
      p95 = quantile(.data[["my_value"]], 0.95, na.rm = TRUE),
      p99 = quantile(.data[["my_value"]], 0.99, na.rm = TRUE),
      mode = calculateMode(.data[["my_value"]]),
      count = dplyr::n(),
      count_distinct = dplyr::n_distinct(.data[["my_value"]])
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "statistic",
      values_to = "value"
    )

  return(output)
}
