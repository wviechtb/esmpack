\name{calc.mcent}
\alias{calc.mcent}
\title{Calculate the Mean-Centered Values of a Variable for Each Subject}
\description{Function that calculates the mean-centered values of a particular variable for each subject.}
\usage{
calc.mcent(x, id, data, na.rm=TRUE)
}
\arguments{
   \item{x}{argument to specify the variable.}
   \item{id}{argument to specify a subject id variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{na.rm}{logical indicating whether missing values should be removed before computing the means (default is \code{TRUE}).}
}
\details{
   The function computes the mean-centered values of a particular variable for each subject, also known as \sQuote{within-person mean centering}.
}
\value{
   A vector.
}
\author{
   Wolfgang Viechtbauer \email{wvb@wvbauer.com} \cr
}
\seealso{
   \code{\link{calc.mean}}
}
\examples{
# illustrative dataset
dat <- data.frame(subj=rep(1:4, each=5),
                  obs = 1:5,
                  age = rep(c(20,31,27,22), each=5),
                  stress = c(2,3,NA,4,2, 3,3,NA,3,NA, 1,1,2,6,4, 1,2,1,3,1))
dat

# calculate the mean-centered values of the stress variable
dat$cstress <- calc.mcent(stress, subj, data=dat)
dat

# calculate the subject-level means of the stress variable
dat$mstress <- calc.mean(stress, subj, data=dat, expand=TRUE)
dat
}
\keyword{manip}
