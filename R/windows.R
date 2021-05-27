
#' Assess if window (dataframe) share certain features across all observations
#'
#' For a given window (dataframe of observations where rows are observations),
#' evaluate whether all observations in the window share the same values for
#' specified columns.
#'
#' Given a dataframe, look at the values in each of the specified column and
#' sees if there is only one level in the specified column. If there is only
#' one level, then this means rows share the same value for that column.
#' Do this for all specified columns and return true if each column only
#' contains one value. If all observations share the same value for the
#' specified columns, then the window is considered a homogeneous window.
#'
#'
#' @param inputted.data A dataframe.
#' @param names.of.columns.to.look.at A vector of strings with each string being the name of a column in the datafarame to look at.
#'
#' @return Boolean (true/false) indicating if window is homogeneous.
#'
#' @export
#'
#' @examples
#'
#' col.one <- c(1, 2, 3, 4, 5)
#' col.two <- c("a", "a", "a", "a", "a")
#' col.three <- c(1, 1, 1, 1, 1)
#'
#' single.window.data <- data.frame(col.one, col.two, col.three)
#'
#' #Example of inhomogeneous window if looking at col.one and col.two because
#' #col.one does not only have a single unique value.
#' result <- FindHomogeneousWindows(single.window.data , c("col.one", "col.two"))
#'
#' result
#'
#' #Example of homogeneous window if looking at col.two and col.three because
#' #col.two and col.three both only have a single unique value.
#' result <- FindHomogeneousWindows(single.window.data , c("col.two", "col.three"))
#'
#' result
#'
#'
FindHomogeneousWindows <- function(inputted.data, names.of.columns.to.look.at){

  #TRUE that columns specified only have one level unless this
  #is proven to be false.
  output <- TRUE

  #Go through each specified column and check if there is only one unique value
  #in each column.
  for(i in 1:length(names.of.columns.to.look.at))
  {
    names.in.column <- levels(as.factor(inputted.data[,names.of.columns.to.look.at[i]]))

    if(length(names.in.column) > 1){
      output <- FALSE
    }
  }

  return(output)

}


#' Get windows (dataframes) that have the same specified column values for all observations
#'
#' For a given dataframe where the windows are specified by a column in the dataframe,
#' evaluate whether all observations in each window share the same values for
#' specified columns.
#'
#' Function takes a single dataframe with each row as observations. This dataframe
#' needs a column that specifies which window each observation belongs
#' to. For each window, the observations within the window is evaluated to see
#' if all observations share the same values for specified columns of the dataframe.
#' Windows where the specified columns have the same values across all observations
#' are labeled as "homogeneous" windows and are captured and outputted in a list,
#' where each element is a window (dataframe). This function uses the
#' FindHomogeneousWindows() function to determine whether each window is
#' homogeneous. As the code executes, it outputs number indicating how many
#' homogeneous windows have been found so far in the inputted.data.
#'
#'
#' @param inputted.data A dataframe that needs a column that labels which window each observation belongs to.
#' @param window.ID.col.name A string that specifies the column name of the column that provides the window name.
#' @param observation.vals.to.compare A vector of strings with each string being the name of a column in the datafarame to look at.
#'
#' @return List where each object is a homogeneous window (dataframe) that has observations sharing the same values for the
#' observation.vals.to.compare column(s).
#'
#'
#' @export
#'
#' @examples
#'
#' #Example using a dataframe with 3 windows.
#'
#' #Windows 20 and 30 are homogeneous if looking at col.two and col.three values.
#' window.name.column <- c(10, 10, 10, 20, 20, 20, 30, 30, 30, 30)
#' col.two <- c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
#' col.three <- c(1, 1, 0, 1, 1, 1, 2, 2, 2, 2)
#'
#' multi.window.data <- data.frame(window.name.column, col.two, col.three)
#'
#' result <- GetHomogeneousWindows(multi.window.data, "window.name.column", c("col.two", "col.three"))
#'
#' #As expected, it looks like two windows are homogeneous.
#' str(result)
#'
#' #Output the two windows that are homogeneous:
#'
#' result[[1]]
#'
#' result[[2]]
#'
#'
GetHomogeneousWindows <- function(inputted.data, window.ID.col.name, observation.vals.to.compare){

  #Testing conditions. 3514 should produce no windows. 3515 should produce one window.
  #window.ID.col.name <- "window_index"
  #inputted.data <- subset(working.data, working.data[,window.ID.col.name]==3515) #3514 is het. 3515 is hmz
  #observation.vals.to.compare <- c("TASK_ID", "RTR_AMP")

  #Testing
  #inputted.data <- working.data
  #observation.vals.to.compare <- c("TASK_ID", "RTR_AMP")

  unique.window.IDs <- levels(as.factor(inputted.data[,window.ID.col.name]))

  windows.captured <- list()
  windows.captured.index <- 1

  for(i in 1:length(unique.window.IDs)){

    print(i)

    window.subset <- subset(inputted.data, inputted.data[,window.ID.col.name]==unique.window.IDs[i])
    #If window has only one amplitude and task, then add to list.
    if(FindHomogeneousWindows(window.subset, observation.vals.to.compare)){

      windows.captured[[windows.captured.index]] <- window.subset
      windows.captured.index <- windows.captured.index + 1

    }

  }

  return(windows.captured)

}


#' Select only windows (dataframes) where a specified column matches a specified value
#'
#' Looks at all the windows (dataframes) in a list of homogeneous windows. And
#' only selects the windows where the column value contains a specified value.
#'
#' Takes a List of windows (dataframes) where each window is a homogeneous window,
#' which means in each window, there is only one unique value in the specified
#' column. This function looks through the homogeneous windows in the List, selects
#' the windows that have a specified column value in the specified column, then
#' puts these windows into a new List and outputs the new List of windows.
#'
#'
#' @param list.of.windows A list of windows (dataframes) and each window can only have a single unique value for the name.of.column.to.look.at.in.window column.
#' @param name.of.column.to.look.at.in.window String that specifies which column to look at.
#' @param value.to.match.to String that specifies what value to look for in name.of.column.to.look.at.in.window.
#'
#' @return List containing only selected windows (windows that have value.to.match.to value in the name.of.column.to.look.at.in.window column).
#'
#'
#' @export
#'
#' @examples
#'
#'#Example using a dataframe with 3 windows.
#'
#' #Windows 20 and 30 are homogeneous if looking at col.two and col.three values.
#' window.name.column <- c(10, 10, 10, 20, 20, 20, 30, 30, 30, 30)
#' col.two <- c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a")
#' col.three <- c(1, 1, 0, 1, 1, 1, 2, 2, 2, 2)
#'
#' multi.window.data <- data.frame(window.name.column, col.two, col.three)
#'
#' list.of.homogeneous.windows <- GetHomogeneousWindows(multi.window.data,
#' "window.name.column", c("col.two", "col.three"))
#'
#' #Get a subset of windows where col.three has a value of 2
#' subset.list.of.homogeneous.windows <- GetSubsetOfWindows(list.of.homogeneous.windows,
#' "col.three", "2")
#'
#' str(subset.list.of.homogeneous.windows)
#'
#' subset.list.of.homogeneous.windows[[1]]
#'
GetSubsetOfWindows <- function(list.of.windows, name.of.column.to.look.at.in.window, value.to.match.to){

  selected.windows <- list()
  selected.windows.index <- 1

  for(i in 1:length(list.of.windows)){

    single.window <- list.of.windows[[i]]

    #This function assumes that the window has observations that are homogeneous
    #for the specified column. So we only need to look at the first value to
    #see what the value is for the specified column in all the observations
    #of the window.
    value.in.window <- single.window[1, name.of.column.to.look.at.in.window]

    if(value.in.window == value.to.match.to){

      selected.windows[[selected.windows.index]] <- single.window

      selected.windows.index <- selected.windows.index + 1

    }

  }

  return(selected.windows)

}



#' Create a contingency table to display how many windows (dataframes) fall into particular categories
#'
#' Given a List of homogeneous windows (dataframes where the two selected columns in each dataframe only
#' have one unique value), make a contingency table to show how many windows fall into each of the
#' categories in level1 and level2.
#'
#' A List of homogeneous windows is inputted. For each window in the list, the columns specified by
#' level1.column.name and level2.column.name can only have one value (definition of homogeneous
#' window). The value of the level1.column and level2.column for each window is evaluated
#' and the number of windows in each category is captured and outputted as a 2D matrix
#' with level1 as the row labels and level2 as the column labels.
#'
#' @param list.of.windows A list of windows (dataframes) and each window can only have a single unique value for the two specified columns.
#' @param level1.column.name A String that specifies the column name to use for the first level of the contingency table. This column should only contain one unique value within each window.
#' @param level2.column.name A String that specifies the column name to use for the second level of the contingency table. This column should only contain one unique value within each window.
#' @param level1.categories A vector that specifies the possible labels in the level1 column. This will be used as the rows of the outputted matrix.
#' @param level2.categories A vector that specifies the possible labels in the level2 column. This will be used as the columns of the outputted matrix.
#'
#' @return A matrix which is formatted as a contingency table.
#'
#' @export
#'
#' @examples
#'
#' #Example using a dataframe with 5 homogeneous windows.
#'
#' #Windows are homogeneous if looking at col.two and col.three values.
#' window.name.column <- c(10, 10, 10, 20, 20, 20, 30, 30, 30, 30, 40, 40, 50, 50, 50, 50)
#' col.two <- c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "a", "a", "a", "a")
#' col.three <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3)
#'
#' multi.window.data <- data.frame(window.name.column, col.two, col.three)
#'
#' list.of.homogeneous.windows <- GetHomogeneousWindows(multi.window.data,
#' "window.name.column", c("col.two", "col.three"))
#'
#' matrix <- CountWindows(list.of.homogeneous.windows, "col.two", "col.three",
#' c("a", "b"), c("1", "2", "3"))
#'
#' matrix
#'
#'
CountWindows <- function(list.of.windows, level1.column.name, level2.column.name,
                         level1.categories, level2.categories){

  #testing conditions
  # list.of.windows <- windows
  # level1.column.name <- "RTR_AMP"
  # level2.column.name <- "TASK_ID"
  # level1.categories <- levels(as.factor(working.data$RTR_AMP))
  # level2.categories <- levels(as.factor(working.data$TASK_ID))

  #Make blank table
  matrix.of.counts <- matrix(0, length(level1.categories), length(level2.categories))
  rownames(matrix.of.counts) <- level1.categories
  colnames(matrix.of.counts) <- level2.categories

  #Goes through each window and see which categories the window falls
  #into and then increment the table
  for(i in 1:length(list.of.windows)){

    window.to.look.at <- list.of.windows[[i]]

    #Looks at the level1 assignment
    level1.value <- window.to.look.at[1,level1.column.name]
    level1.index <- which(level1.categories == level1.value)

    #Looks at the level2 assignment
    level2.value <- window.to.look.at[1,level2.column.name]
    level2.index <- which(level2.categories == level2.value)

    #Combines level1 and level2 and makes increment in table
    matrix.of.counts[level1.index, level2.index] <- matrix.of.counts[level1.index, level2.index] + 1

  }

  return(matrix.of.counts)

}


#' Select only windows (dataframes) where a two specified columns must match specified values
#'
#' Looks at all the windows (dataframes) in a list of homogeneous windows. And
#' only selects the windows where the column values for two columns matches
#' the specified values.
#'
#' Takes a List of windows (dataframes) where each window is a homogeneous window,
#' which means in each window, there is only one unique value in the specified
#' column. This function looks through the homogeneous windows in the List, selects
#' the windows that have specified column value(s) in the first specified column, then
#' from the windows selected based on the first column, windows are further selected
#' to have specified column value(s) in the second specified column. Puts these windows
#' into a new List and outputs the new List of windows. Uses the
#' GetSubsetOfWindows() function.
#'
#'
#' @param list.of.windows A list of windows (dataframes) and each window can only have a single unique value for the two specified columns.
#' @param level1.column.name A String that specifies the column name to use for the first level. This column should only contain one unique value within each window.
#' @param level2.column.name A String that specifies the column name to use for the second level. This column should only contain one unique value within each window.
#' @param level1.categories A vector that specifies the possible labels in the level1 column.
#' @param level2.categories A vector that specifies the possible labels in the level2 column.
#'
#' @return List containing only selected windows
#'
#'
#' @export
#'
#' @examples
#' #Example using a dataframe with 5 homogeneous windows.
#'
#' #Windows are homogeneous if looking at col.two and col.three values.
#' window.name.column <- c(10, 10, 10, 20, 20, 20, 30, 30, 30, 30, 40, 40, 50, 50, 50, 50)
#' col.two <- c("a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "a", "a", "a", "a")
#' col.three <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3)
#'
#' multi.window.data <- data.frame(window.name.column, col.two, col.three)
#'
#' list.of.homogeneous.windows <- GetHomogeneousWindows(multi.window.data,
#' "window.name.column", c("col.two", "col.three"))
#'
#' result <- GetSubsetOfWindowsTwoLevels(list.of.homogeneous.windows, "col.two", "col.three",
#' c("a"), c("1", "2"))
#'
#' #Should contain windows 10, 20, 30 because col.two is "a" and col.three can be "1" or "2"
#' result
#'
GetSubsetOfWindowsTwoLevels <- function(list.of.windows, level1.column.name, level2.column.name,
                                        level1.categories, level2.categories){

  selected.windows <- list()

  windows.level1 <- list()

  #Get windows for first level
  for(i in 1:length(level1.categories)){

    windows.level1.temp <- GetSubsetOfWindows(list.of.windows, level1.column.name, level1.categories[[i]])

    windows.level1 <- c(windows.level1, windows.level1.temp)

  }

  #Using windows that satisfy first level, find the ones that satisfy second level too
  for(i in 1:length(level2.categories)){

    windows.level2.temp <- GetSubsetOfWindows(windows.level1, level2.column.name, level2.categories[[i]])

    selected.windows <- c(selected.windows, windows.level2.temp)

  }


  return(selected.windows)

}

