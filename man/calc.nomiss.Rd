\name{calc.nomiss}
\alias{calc.nomiss}
\title{Calculate the Number of Non-Missing Values for Each Subject}
\description{Function that calculates the number of non-missing values for a particular variable for each subject.}
\usage{
calc.nomiss(x, id, data, prop=FALSE, expand=FALSE)
}
\arguments{
   \item{x}{vector to specify the variable.}
   \item{id}{vector to specify a subject id variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{prop}{logical indicating whether to return proportions instead of counts (default is \code{FALSE}).}
   \item{expand}{logical indicating whether to expand the returned vector so that it can be added back to the dataset as a new variable (default is \code{FALSE}).}
}
\details{
   The function computes the number (or proportion if \code{prop=TRUE}) of non-missing values for a particular variable for each subject.

   When \code{expand=TRUE}, the number of non-missing values for each subject is repeated in such a way that the returned vector can be added back to the dataset as a new variable. See \sQuote{Examples}.
}
\value{
   A vector.
}
\author{
   Wolfgang Viechtbauer \email{wvb@wvbauer.com} \cr
}
\examples{
# illustrative dataset
dat <- data.frame(subj=rep(1:4, each=5),
                  obs = 1:5,
                  age = rep(c(20,31,27,22), each=5),
                  stress = c(2,3,NA,4,2, 3,3,NA,3,NA, 1,1,2,6,4, 1,2,1,3,1))
dat

# calculate the number of non-missing values per subject for the stress variable
calc.nomiss(stress, subj, data=dat)

# add this variable back to the original dataset
dat$compliance <- calc.nomiss(stress, subj, data=dat, expand=TRUE)
dat

# add the proportion of non-missing values to the dataset
dat$compliance <- calc.nomiss(stress, subj, data=dat, prop=TRUE, expand=TRUE)
dat
}
\keyword{manip}
