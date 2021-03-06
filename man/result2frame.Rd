% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/result2frame.R
\name{result2frame}
\alias{result2frame}
\title{result2frame}
\usage{
result2frame(...)
}
\arguments{
\item{...}{Query-result}
}
\value{
Return result from query as dataframe
}
\description{
Converts the query-result to a frame. The query-result is either a list (sequence) or an array.
    If it is a list, 'cols' is needed to determine the number of columns.
}
