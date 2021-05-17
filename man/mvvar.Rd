\name{mvvar}
\alias{mvvar}
\title{Move Variable in a Data Frame}
\description{Function to move a variable to a different position in a data frame.}
\usage{
mvvar(var, where, after=TRUE, data)
}
\arguments{
   \item{var}{variable to move.}
   \item{where}{variable next to which \code{var} will be moved.}
   \item{after}{logical indicating whether to position \code{var} after or before variable \code{where} (default is \code{after=TRUE}).}
   \item{data}{data frame that contains the variables specified above.}
}
\details{
   The function can be used to move a variable to a different position in a data frame.
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

# move variable age after subj
dat <- mvvar("age", "subj", data=dat)
dat

# move variable obs before age
dat <- mvvar("obs", "age", after=FALSE, data=dat)
dat
}
\keyword{manip}
