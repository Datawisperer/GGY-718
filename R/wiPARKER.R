al_prop <- read.csv("Data/Alkaline proportions.csv", header = TRUE)

library(dplyr)
#' Calculating the weathering index of Parker.
#'
#' @param dframe
#' @param a
#' @param b
#' @param c
#' @param d
#'
#' Return = numeric values representing level of weathering in your samples.
#' @export
#'
#' WIP <- wiPARKER(df, df$colName, df$colName, df$colName, df$colName)

wiPARKER <- function(dframe, a, b, c, d){ # Creating the function, wiPARKER, with your data frame and the
  # four columns, a = NA, b = Mg, C = K, d= Ca, as the parameters.
  dframe2 <- data.frame(dplyr::mutate(dframe, WID = 'NA')) # Creating a local data frame with a column of
  # temporarily empty cells.

  for (i in 1:nrow(dframe2)) { # Iterating through every row of the local data frame.
    dframe2$WID[i] <- (((a[i]/0.35) + (b[i]/0.9) + (c[i]/0.25) + (d[i]/0.7)) * 100) # Calculating the WIP
    # values according to the Parker(1970) formula.

  }
return(dframe2)} # Returning the data frame.








