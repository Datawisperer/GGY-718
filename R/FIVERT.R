part_diam <- read.csv('Data/phi_data.csv', header = TRUE)


#' Converting millimeter to phi using FIVERT
#'
#' @param dframe
#' @param diameter
#'
#' A column of the calculated phi values based on your observations in millimeters.
#' @export
#'
#' phi_values <- FIVERT(df, df$colName)

FIVERT <- function(dframe, diameter){ # Creating a function with the parameters dframe for your data
                                      # frame and diameter for the column containing the variables.

  dframex <- data.frame(dplyr::mutate(dframe, phi_value = 'NA')) # Creating a local data frame with an
                                             # empty column for the newly calculated values.

  for (i in 1:nrow(dframex)) { # Iterating through every row in your data frame.
    dframex$phi_value[i] <-  -log2(diameter[i]) # Assigning the calculated values to each phi_value cell.
  }

  return(dframex)}  # Returning the local data frame.


