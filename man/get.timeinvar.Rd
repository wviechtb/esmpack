\name{get.timeinvar}
\alias{get.timeinvar}
\title{Get the Value of a Time-Invariant Variable for Each Subject}
\description{Function that returns the value of a time-invariant variable for each subject.}
\usage{
get.timeinvar(x, id, data, na.rm=TRUE)
}
\arguments{
   \item{x}{vector to specify the time-invariant variable.}
   \item{id}{vector to specify a subject id variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{na.rm}{logical indicating whether missing values should be removed (default is \code{TRUE}).}
}
\details{
   The function returns the value of a time-invariant variable for each subject from a dataset in long format.
}
\value{
   The function returns a vector.
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

# get age of each subject
get.timeinvar(age, subj, data=dat)

# age value is sometimes NA for the 2nd subject (not a problem)
dat$age[6:7] <- NA
dat
get.timeinvar(age, subj, data=dat)

# age value is completely missing for the 2nd subject
dat$age[6:10] <- NA
dat
get.timeinvar(age, subj, data=dat)

# also show subjects with missing value for time-invariant variable
get.timeinvar(age, subj, data=dat, na.rm=FALSE)
}
\keyword{manip}
