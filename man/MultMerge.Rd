\name{MultMerge}
\alias{MultMerge}
\title{Merge all files in a path into one data frame}
\usage{
MultMerge(mypath)
}
\arguments{
  \item{mypath}{the path containing files to load (all
  files will be loaded)}
}
\description{
This function opens all files in the given directory and
merges them into one data. Files must meet the following
requirement:\ - Merge value in first column - Must have
headers - Header value for data to be merged must be unique
}
\keyword{merge}

