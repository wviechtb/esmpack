\name{lagvar}
\alias{lagvar}
\title{Create a Lagged Variable}
\description{Function to create a lagged variable.}
\usage{
lagvar(x, obs, id, day, data, lag=1)
}
\arguments{
   \item{x}{vector to specify the variable to be lagged.}
   \item{obs}{numeric vector that indicates the observation number of the values specified via the \code{x} argument.}
   \item{id}{optional vector to specify a subject id variable.}
   \item{day}{optional vector to specify a day number variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{lag}{scalar specifying the lag (default is \code{1}).}
}
\details{
   The function can be used to create a lagged version of the variable specified via the \code{x} argument. The \code{obs} argument is used to specify a variable that indicates the observation number of the values specified via the \code{x} argument. There should not be repeated values of \code{obs} (within the same subject, in case the dataset includes multiple subjects).

   The (optional) \code{id} argument is used to specify a subject id variable. If not specified, all observations are assumed to come from the same subject.

   The (optional) \code{day} variable is used to specify a day number variable. If not specified, then values of the variable to be lagged are allowed to be lagged across days (in case there are multiple observation days). If the \code{day} variable is specified, then lagged values across different days are not allowed (i.e., are set to missing).
}
\value{
   The function returns a vector with the lagged values (that will typically be added back to the data frame as a new variable).
}
\author{
   Wolfgang Viechtbauer \email{wvb@wvbauer.com} \cr
}
\examples{
# illustrative dataset (5 beeps for 2 days for 2 subjects)
# note: beeps not responded to are not included in this dataset
dat <- read.table(header=TRUE, text = "
subject dayno beep stress
1 1 1  3
1 1 2  2
1 1 3  4
1 1 4  1
1 1 5  2
1 2 6  5
1 2 7  2
1 2 9  3
1 2 10 3
2 1 1  2
2 1 2  3
2 1 3  2
2 2 6  1
2 2 7  4
2 2 8  5
2 2 10 2")
dat

# lag the stress variable
dat$stress_lag <- lagvar(stress, subject, beep, data=dat)
dat

# but this also allows lagged values across days
# this can be avoided by specifying the day variable
dat$stress_lag <- lagvar(stress, subject, beep, dayno, data=dat)
dat

# lag the stress variable by two observations
dat$stress_lag2 <- lagvar(stress, subject, beep, dayno, data=dat, lag=2)
dat

# illustrative dataset as above, but with missing beeps included
dat <- read.table(header=TRUE, text = "
subject dayno beep stress
1 1 1  3
1 1 2  2
1 1 3  4
1 1 4  1
1 1 5  2
1 2 6  5
1 2 7  2
1 2 8  NA
1 2 9  3
1 2 10 3
2 1 1  2
2 1 2  3
2 1 3  2
2 1 4  NA
2 1 5  NA
2 2 6  1
2 2 7  4
2 2 8  5
2 2 9  NA
2 2 10 2")
dat

# lag the stress variable
dat$stress_lag <- lagvar(stress, subject, beep, dayno, data=dat)
dat
}
\keyword{manip}
