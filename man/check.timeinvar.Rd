\name{check.timeinvar}
\alias{check.timeinvar}
\title{Check if Value of a Variable is Time-Invariant for Each Subject}
\description{Function that checks if a variable is time-invariant variable for each subject.}
\usage{
check.timeinvar(x, id, data, which=FALSE)
}
\arguments{
   \item{x}{vector to specify the variable to check.}
   \item{id}{vector to specify a subject id variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{which}{logical indicating whether the function should return the ids of those subjects where the variable is not time-invariant (default is \code{FALSE}).}
}
\details{
   The function checks if the values of a variable are constant (i.e., time-invariant) for each subject.

   Setting \code{na.rm} to \code{TRUE} has two effects. First, the first non-missing value will be returned per subject. Second, if all values of the variable of interest are missing, the resulting \code{NA} will be removed from the returned vector. See \sQuote{Examples}.
}
\value{
   The function returns a logical (if \code{which=FALSE}) or it returns a vector with the ids of those subjects where the variable is not time-invariant (if \code{which=TRUE})
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

# check age variable
check.timeinvar(age, subj, data=dat)

# check stress variable
check.timeinvar(stress, subj, data=dat)

# for which subjects is stress non-constant
check.timeinvar(stress, subj, data=dat, which=TRUE)
}
\keyword{manip}
