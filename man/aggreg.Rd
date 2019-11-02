\name{aggreg}
\alias{aggreg}
\title{Aggregate Data to Subject Level}
\description{Function that aggregates a dataset to the subject level.}
\usage{
aggreg(data, id, vars, na.rm=TRUE)
}
\arguments{
   \item{data}{data frame to aggregate.}
   \item{id}{vector to specify a subject id variable.}
   \item{vars}{either a character vector giving the names of the variables to aggregate or a numeric vector giving the position of the columns in the data frame corresponding to the items.}
   \item{na.rm}{logical indicating whether missing values should be removed before aggregating the variables (default is \code{TRUE}).}
}
\details{
   The function aggregates a dataset in the long format to the subject level. For numeric, integer, and logical variables, the subject-level means are computed. For factors and character variables, the first (non-missing) value is returned.
}
\value{
   A data frame.
}
\author{
   Wolfgang Viechtbauer \email{wvb@wvbauer.com} \cr
}
\examples{
# illustrative dataset
dat <- data.frame(subj=rep(1:4, each=5),
                  sex = rep(c("male", "female"), each=2*5),
                  obs = 1:5,
                  age = rep(c(20,31,27,22), each=5),
                  stress = c(2,3,NA,4,2, 3,3,NA,3,NA, 1,1,2,6,4, 1,2,1,3,1))
dat

# aggregate the dataset
aggreg(dat, subj)

# aggregate the dataset for selected variables
aggreg(dat, subj, vars=c("subj", "stress"))

# aggregate the dataset for selected variables
aggreg(dat, subj, vars=1:2)
}
\keyword{manip}
