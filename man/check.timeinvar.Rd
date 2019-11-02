\name{check.timeinvar}
\alias{check.timeinvar}
\title{Check If Variable is Time-Invariant for Each Subject}
\description{Function that checks if a variable is time-invariant for each subject.}
\usage{
check.timeinvar(x, id, data, out=1, na.rm=TRUE)
}
\arguments{
   \item{x}{vector to specify the variable to check.}
   \item{id}{vector to specify a subject id variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{out}{either a string or an integer (1 = \code{"logical"}, 2 = \code{"id"}, 3 = \code{"data"}) indicating what information should be returned in case there are subjects where the variable is not time-invariant.}
   \item{na.rm}{logical indicating whether missing values should be removed before checking (default is \code{TRUE}).}
}
\details{
   The function checks if the values of a variable are constant (i.e., time-invariant) for each subject.

   When \code{na.rm=TRUE} (the default), missing values are ignored. When setting \code{na.rm=FALSE}, then missing values are treated as distinct values from any non-missing values. See \sQuote{Examples}.
}
\value{
   When \code{out = 1} or \code{out = "logical"}, the function simply returns a logical (i.e., \code{TRUE} or \code{FALSE}, depending on whether the variable is time-invariant within each subject or not.

   When \code{out = 2} or \code{out = "id"}, the function returns a vector with the ids of those subjects where the variable is not time-invariant.

   When \code{out = 3} or \code{out = "data"}, the function returns the data for those subjects where the variable is not time-invariant.
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

# check the age variable
check.timeinvar(age, subj, data=dat)

# check the stress variable
check.timeinvar(stress, subj, data=dat)

# for which subjects is stress non-constant
check.timeinvar(stress, subj, data=dat, out=2)

# show the data for those subjects
check.timeinvar(stress, subj, data=dat, out=3)

# by default missings are ignored
dat$age[2] <- NA
check.timeinvar(age, subj, data=dat)

# treat NAs as distinct values
check.timeinvar(age, subj, data=dat, na.rm=FALSE)
check.timeinvar(age, subj, data=dat, na.rm=FALSE, out=2)
check.timeinvar(age, subj, data=dat, na.rm=FALSE, out=3)
}
\keyword{manip}
