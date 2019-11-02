\name{check.nomiss}
\alias{check.nomiss}
\title{Check If Variable has No Missings for Each Subject}
\description{Function that checks if a variable has no missing values for each subject.}
\usage{
check.nomiss(x, id, data, out=1)
}
\arguments{
   \item{x}{vector to specify the variable to check.}
   \item{id}{vector to specify a subject id variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{out}{either a string or an integer (\code{1} = \code{"logical"}, \code{2} = \code{"id"}, \code{3} = \code{"data"}) indicating what information should be returned in case there are subjects where the variable is not time-invariant.}
}
\details{
   The function checks if the values of a variable do not contain any missing values for each subject.
}
\value{
   When \code{out = 1} or \code{out = "logical"}, the function simply returns a logical (i.e., \code{TRUE} or \code{FALSE}), depending on whether the variable contains no missing values within each subject.

   When \code{out = 2} or \code{out = "id"}, the function returns a vector with the ids of those subjects where the variable contains missing values.

   When \code{out = 3} or \code{out = "data"}, the function returns the data for those subjects where the variable contains missing values.
}
\author{
   Wolfgang Viechtbauer \email{wvb@wvbauer.com} \cr
}
\examples{
# illustrative dataset
dat <- data.frame(subj=rep(1:4, each=5),
                  obs = 1:5,
                  age = rep(c(20,31,27,22), each=5),
                  stress = c(2,3,1,4,2, 3,NA,3,3,3, 1,1,2,6,4, 1,2,1,3,1))
dat

# check if the age variable contains no missings
check.nomiss(age, subj, data=dat)

# check if the stress variable contains no missings
check.nomiss(stress, subj, data=dat)

# for which subjects are there missing values?
check.nomiss(stress, subj, data=dat, out=2)

# show the data for those subjects
check.nomiss(stress, subj, data=dat, out=3)
}
\keyword{manip}
