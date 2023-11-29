\name{calc.zcent}
\alias{calc.zcent}
\title{Calculate the Standardized Values of a Variable for Each Subject}
\description{Function that calculates the standardized values of a particular variable for each subject.}
\usage{
calc.zcent(x, id, data, na.rm=TRUE)
}
\arguments{
   \item{x}{argument to specify the variable.}
   \item{id}{argument to specify a subject id variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{na.rm}{logical indicating whether missing values should be removed before computing the means and standard deviations for computing the standardized values (default is \code{TRUE}).}
}
\details{
   The function computes the standardized values of a particular variable for each subject. Note that the values are standardized within subjects (i.e., based on the subject-level means and standard deviations), so this is analogous to \sQuote{within-person mean centering}, except that the centered values are also divided by the person-level standard deviations.
}
\value{
   A vector.
}
\author{
   Wolfgang Viechtbauer \email{wvb@wvbauer.com} \cr
}
\seealso{
   \code{\link{calc.mcent}}
}
\examples{
# illustrative dataset
dat <- data.frame(subj=rep(1:4, each=5),
                  obs = 1:5,
                  age = rep(c(20,31,27,22), each=5),
                  stress = c(2,3,NA,4,2, 3,3,NA,3,NA, 1,1,2,6,4, 1,2,1,3,1))
dat

# calculate the standardized values of the stress variable
dat$cstress <- calc.zcent(stress, subj, data=dat)
dat
}
\keyword{manip}
