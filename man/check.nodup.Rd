\name{check.nodup}
\alias{check.nodup}
\title{Check if Variable has No Duplicated Values for Each Subject}
\description{Function that checks if a variable has no duplicated values for each subject.}
\usage{
check.nodup(x, id, data, out=1, na.rm=TRUE)
}
\arguments{
   \item{x}{argument to specify the variable to check.}
   \item{id}{argument to specify a subject id variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{out}{either a string or an integer (\code{1} = \code{"logical"}, \code{2} = \code{"id"}, \code{3} = \code{"data"}) indicating what information should be returned in case there are subjects where the variable has duplicates.}
   \item{na.rm}{logical indicating whether missing values should be removed before checking (default is \code{TRUE}).}
}
\details{
   The function checks if a variable has no duplicated values for each subject.

   When \code{na.rm=TRUE} (the default), missing values are ignored. When setting \code{na.rm=FALSE}, then missing values are treated as distinct values from any non-missing values. See \sQuote{Examples}.
}
\value{
   When \code{out = 1} or \code{out = "logical"}, the function simply returns a logical (i.e., \code{TRUE} or \code{FALSE}), depending on whether the variable has no duplicated values within each subject.

   When \code{out = 2} or \code{out = "id"}, the function returns a vector with the ids of those subjects where the variable has duplicated values.

   When \code{out = 3} or \code{out = "data"}, the function returns the data for those subjects where the variable has duplicated values.
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

# check that variable obs has no duplicated values within subjects
check.nodup(obs, subj, data=dat)

# introduce a duplicated value for the third subject
dat$obs[13] <- 2

# check that variable obs has no duplicated values within subjects
check.nodup(obs, subj, data=dat)

# for which subjects are there duplicated values?
check.nodup(obs, subj, data=dat, out=2)

# show the data for those subjects
check.nodup(obs, subj, data=dat, out=3)
}
\keyword{manip}
