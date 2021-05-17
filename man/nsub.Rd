\name{nsub}
\alias{nsub}
\title{Number of Subjects}
\description{Function to compute the number of subjects.}
\usage{
nsub(id, data)
}
\arguments{
   \item{id}{vector to specify a subject id variable.}
   \item{data}{optional data frame that contains the variable specified above.}
}
\details{
   The function simply computes the number of subjects in a long-format dataset (i.e., the number of unique values of the id variable).
}
\value{
   A scalar.
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

# compute the number of subjects
nsub(subj, dat)

# or specify the subject variable this way
nsub(dat$subj)
}
\keyword{manip}
