\name{calc.fun}
\alias{calc.fun}
\title{Calculate some Summary Statistic of a Variable for Each Subject}
\description{Function that calculates the summary statistic for a particular variable for each subject.}
\usage{
calc.fun(x, id, data, FUN, expand=FALSE, \dots)
}
\arguments{
   \item{x}{argument to specify the variable.}
   \item{id}{argument to specify a subject id variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{FUN}{function that computes the summary statistic of interest.}
   \item{expand}{logical indicating whether to expand the returned vector so that it can be added back to the dataset as a new variable (default is \code{FALSE}).}
   \item{\dots}{other arguments passed on to the function specified via \code{FUN}.}
}
\details{
   The function computes the summary of a particular variable for each subject.

   When \code{expand=TRUE}, the value of the statistic for each subject is repeated in such a way that the returned vector can be added back to the dataset as a new variable. See \sQuote{Examples}.
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

# calculate the subject-level SDs of the stress variable
calc.fun(stress, subj, data=dat, FUN=sd)

# remove the missings before computing the SDs
calc.fun(stress, subj, data=dat, FUN=sd, na.rm=TRUE)

# add this variable back to the original dataset
dat$sdstress <- calc.fun(stress, subj, data=dat, FUN=sd, na.rm=TRUE, expand=TRUE)
dat
}
\keyword{manip}
