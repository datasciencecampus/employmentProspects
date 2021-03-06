% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatter.R
\name{calculateQuintiles}
\alias{calculateQuintiles}
\title{Calculate quintiles}
\usage{
calculateQuintiles(data, col)
}
\arguments{
\item{data}{The dataframe}

\item{col}{The column on which to do the calculation}
}
\value{
The function returns the original dataframe with an additional column
containing a factor variables of the quintile ranges.
}
\description{
Calculate the quintiles of a given column in a dataframe.
}
\author{
Nathan Eastwood
}
