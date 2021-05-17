\name{rename}
\alias{rename}
\title{Rename a Variable in a Data Frame}
\description{Function to rename a variable in a data frame.}
\usage{
rename(old, new, data)
}
\arguments{
   \item{old}{name of the variable to rename.}
   \item{new}{new name of the variable.}
   \item{data}{data frame that contains the variable to be renamed.}
}
\details{
   The function can be used to rename a variable in a data frame.
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
                  obs = 1:5,
                  age = rep(c(20,31,27,22), each=5),
                  stress = c(2,3,1,4,2, 3,3,3,3,3, 1,1,2,6,4, 1,2,1,3,1))
dat

# rename variable age to years
dat <- rename("age", "years", dat)
dat
}
\keyword{manip}
