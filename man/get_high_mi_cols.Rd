% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/x2y.R
\name{get_high_mi_cols}
\alias{get_high_mi_cols}
\title{filter columns of a dx2y dataframe result based on a minimal mutual-information}
\usage{
get_high_mi_cols(dx2y_df, threshold = 99, keep = "first")
}
\arguments{
\item{dx2y_df}{dataframe result of a dx2y function}

\item{threshold}{minimum mutual-information value to keep a column}

\item{keep}{method to choose the column to keeep. only "first" is implemented}
}
\value{
a vector of column names from the original dataframe
}
\description{
filter columns of a dx2y dataframe result based on a minimal mutual-information
}
\examples{

}
