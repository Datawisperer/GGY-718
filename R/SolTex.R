soilTexture <- read.csv('Data/SoilTexture.csv', header = TRUE) # Importing a test data set.


#' Determining the soil textures of your observations
#'
#' @param df
#' @param SAND
#' @param SILT
#' @param CLAY
#'
#' A column containing strings describing the soil texture of each observation.
#' @export
#'
#' S_texture_classes <- SolTex(df, df$colName, df$colName, df$colName)

SolTex <- function(df, SAND, SILT, CLAY){  # Inserting the parameters to the function, df = data frame,
  # SAND = proportion sand, SILT = proportion SILT, CLAY = proportion clay.

  df$textureClass <-  "NA"    # Creating an empty column for the imported data frame.

  for (i in 1:nrow(df)) {     # Iterating through each row of the imported data frame.
    if(SAND[i] >= 86 && SILT[i] <= 14 && CLAY[i] <= 10){ # Setting the parameters for the sand class
      df$textureClass[i] <-  'sand'                      # using logical operators.
    }
    if(between(SAND[i], 70, 86) && SILT[i] <= 30 && CLAY[i] <= 15){ # Setting the parameters for the
      df$textureClass[i] <-  'loamy sand'                 # loamy sand class with logical operators.
    }
    if(between(SAND[i], 50, 70) && SILT[i] <= 50 && CLAY[i] <= 20){ # Setting the parameters for the
      df$textureClass[i] <-  'sandy loam' # sandy loam class using logical operators.
    }
    if(between(SAND[i], 23, 52) && between(SILT[i], 28, 50) && between(CLAY[i], 7, 27)){ # Setting the
      df$textureClass[i] <-  'loam' # parameters for the loam class using logical operators.
    }
    if(between(SAND[i], 20, 50) && between(SILT[i], 74, 88) && CLAY[i] <= 27){ # Setting the parameters
      df$textureClass[i] <-  'silty loam' # for the silty loam class using logical operators.
    }
    if(SAND[i] <= 20 && SILT[i] >= 88 && CLAY[i] <= 12){ # Setting the parameters for the silt class
      df$textureClass[i] <-  'silt' # with logical operators.
    }
    if(between(SAND[i], 20, 45) && between(SILT[i], 15, 52) && between(CLAY[i], 27, 40)){ # Setting the
      df$textureClass[i] <- 'clay loam' # parameters for the clay loam class with logical operators.
    }
    if(between(SAND[i], 45, 80) && SILT[i] <= 28 && between(CLAY[i], 20, 35)){ # Setting the parameters
      df$textureClass[i] <- 'sandy clay loam' # for the sandy loam class with logical operators.
    }
    if(SAND[i] <= 20 && between(SILT[i], 40, 73) && between(CLAY[i], 27, 40)){ # Setting the parameters
      df$textureClass[i] <- 'silty clay loam' # for the silty loam class with logical operators.
    }
    if(between(SAND[i], 45, 65) && df $SILT[i] <= 20 && between(CLAY[i], 35, 55)){ # Setting the
      df$textureClass[i] <- 'sandy clay' # parameters for the sandy clay class with logical operators.
    }
    if(SAND[i] <= 20 && between(SILT[i], 40, 60) && between(CLAY[i], 40, 60)){ # Setting the parameters
      df$textureClass[i] <- 'silty clay' # for the silty clay class with logical operators.
    }
    if(SAND[i] <= 45 && SILT[i] <= 40 && CLAY[i] >= 40){ # Setting the parameters for the clay class
      df$textureClass[i] <- 'clay' # with logical operators.
    }
  }
  return(df)} # Returning the data frame.

SolTex(df = SoilTest, SoilTest$Sand, SoilTest$silt, SoilTest$clay) # Populating the function parameters
# with a data frame.

