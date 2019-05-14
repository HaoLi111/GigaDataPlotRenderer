\name{sample_data_frame}
\alias{sample_data_frame}
\title{
Uniform Random Sampling for Large Datasets
}
\description{
Subset big data to small data if certain tool not compatible with big data scale is preferred. Not recommended
}
\usage{
sample_data_frame(x,size=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A dataframe
}
}
\author{
Hao Li
}
\note{
This is not a S3 method.
}

\seealso{
 \code{\link{sample}}
}
\examples{
sample_data.frame(iris,10)
}
