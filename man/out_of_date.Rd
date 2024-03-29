% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/out_of_date.R
\name{out_of_date}
\alias{out_of_date}
\title{Check if targets are out-of-date vis-a-vis their dependencies}
\usage{
out_of_date(targets, dependencies, packages = NULL)
}
\arguments{
\item{targets}{A character vector of paths to files}

\item{dependencies}{A character vector of paths to files which the \code{targets}
depend on}

\item{packages}{A character vector of names of packages which \code{targets}
depend on}
}
\value{
\code{TRUE} if any of \code{targets} are older than any of \code{dependencies} or if
any of \code{targets} do not exist; \code{FALSE} otherwise
}
\description{
Check if targets are out-of-date vis-a-vis their dependencies
}
\examples{
\dontrun{
out_of_date("data/processed_data.Rds", "data/raw_data.Rds")
}
}
