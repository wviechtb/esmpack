\name{calc.mean}
\alias{calc.mean}
\title{Calculate the Mean of a Variable for Each Subject}
\description{Function that calculates the mean of a particular variable for each subject.}
\usage{
calc.mean(x, id, data, na.rm=TRUE, expand=FALSE)
}
\arguments{
   \item{x}{vector to specify the variable.}
   \item{id}{vector to specify a subject id variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{na.rm}{logical indicating whether missing values should be removed before computing the means (default is \code{TRUE}).}
   \item{expand}{logical indicating whether to expand the returned vector so that it can be added back to the dataset as a new variable (default is \code{FALSE}).}
}
\details{
   The function computes the mean of a particular variable for each subject.

   When \code{expand=TRUE}, the mean for each subject is repeated in such a way that the returned vector can be added back to the dataset as a new variable. See \sQuote{Examples}.
}
\value{
   A vector.
}
\author{
   Wolfgang Viechtbauer \email{wvb@wvbauer.com} \cr
}
\seealso{
   \code{\link{calc.center}}
}
\examples{
# illustrative dataset
dat <- data.frame(subj=rep(1:4, each=5),
                  obs = 1:5,
                  age = rep(c(20,31,27,22), each=5),
                  stress = c(2,3,NA,4,2, 3,3,NA,3,NA, 1,1,2,6,4, 1,2,1,3,1))
dat

# calculate the subject-level means of the stress variable
calc.mean(stress, subj, data=dat)

# add this variable back to the original dataset
dat$mstress <- calc.mean(stress, subj, data=dat, expand=TRUE)
dat
}
\keyword{manip}
