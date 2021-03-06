% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/windows.R
\name{FindHomogeneousWindows}
\alias{FindHomogeneousWindows}
\title{Assess if window (dataframe) share certain features across all observations}
\usage{
FindHomogeneousWindows(inputted.data, names.of.columns.to.look.at)
}
\arguments{
\item{inputted.data}{A dataframe.}

\item{names.of.columns.to.look.at}{A vector of strings with each string being the name of a column in the datafarame to look at.}
}
\value{
Boolean (true/false) indicating if window is homogeneous.
}
\description{
For a given window (dataframe of observations where rows are observations),
evaluate whether all observations in the window share the same values for
specified columns.
}
\details{
Given a dataframe, look at the values in each of the specified column and
sees if there is only one level in the specified column. If there is only
one level, then this means rows share the same value for that column.
Do this for all specified columns and return true if each column only
contains one value. If all observations share the same value for the
specified columns, then the window is considered a homogeneous window.
}
\examples{

col.one <- c(1, 2, 3, 4, 5)
col.two <- c("a", "a", "a", "a", "a")
col.three <- c(1, 1, 1, 1, 1)

single.window.data <- data.frame(col.one, col.two, col.three)

#Example of inhomogeneous window if looking at col.one and col.two because
#col.one does not only have a single unique value.
result <- FindHomogeneousWindows(single.window.data , c("col.one", "col.two"))

result

#Example of homogeneous window if looking at col.two and col.three because
#col.two and col.three both only have a single unique value.
result <- FindHomogeneousWindows(single.window.data , c("col.two", "col.three"))

result


}
