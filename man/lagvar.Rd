\name{lagvar}
\alias{lagvar}
\title{Create a Lagged Variable}
\description{Function to create a lagged variable.}
\usage{
lagvar(x, id, obs, day, data, lag=1)
}
\arguments{
   \item{x}{vector to specify the variable to be lagged.}
   \item{id}{optional vector to specify a subject id variable.}
   \item{obs}{optional numeric vector that indicates the observation number of the values specified via the \code{x} argument.}
   \item{day}{optional vector to specify a day number variable.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{lag}{scalar specifying the lag (default is \code{1}). Can also be a vector with multiple lag values that are permitted. See \sQuote{Details}.}
}
\details{
   The function can be used to create a lagged version of the variable specified via the \code{x} argument.

   The (optional) \code{id} argument is used to specify a subject id variable. If not specified, all observations are assumed to come from the same subject.

   The (optional) \code{obs} argument is used to specify a variable that indicates the observation number of the values specified via the \code{x} argument. There should not be repeated values of \code{obs} (within the same subject, in case the dataset includes multiple subjects). If not specified, then consecutive observations from the same subjects are assumed to be one lag apart.

   The (optional) \code{day} variable is used to specify a day number variable. If not specified, then values of the variable to be lagged are allowed to be lagged across days (in case there are multiple observation days). If the \code{day} variable is specified, then lagged values across different days are not allowed (i.e., are set to missing).

   The \code{lag} argument is used to specify the number of lags (i.e., the lagged value of the variable at a particular observation number \eqn{t} is the value of the variable observed at observation number \eqn{t - lag}). If no lagged value is available at observation number \eqn{t - lag} (either because the dataset does not include that observation number or the value at that observation number is missing), then the lagged value will be missing.

   One can also specify multiple values for the \code{lag} argument (usually a consecutive range, such as \code{1:3}). In that case, the function will find the lagged value with the smallest lag and also indicate the lag of each lagged value. See \sQuote{Examples}.
}
\value{
   If \code{lag} is a single number, the function returns a vector with the lagged values (that will typically be added back to the data frame as a new variable).

   If multiple values are specified for \code{lag}, the function returns a data frame with elements \code{xl} for the lagged values and \code{lag} for the lag of those values.
}
\author{
   Wolfgang Viechtbauer \email{wvb@wvbauer.com} \cr
}
\examples{
# illustrative dataset (5 beeps for 2 days for 2 subjects)
# note: beeps not responded to are not included in this dataset
# in this case, specifying the 'obs' variable (i.e., 'beep') is crucial
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
2 2 9  5
2 2 10 2")
dat

# lag the stress variable
dat$stress_lag <- lagvar(stress, id=subject, obs=beep, data=dat)
dat

# but this also allows lagged values across days
# this can be avoided by specifying the day variable
dat$stress_lag <- lagvar(stress, id=subject, obs=beep, day=dayno, data=dat)
dat

# lag the stress variable by two observations
dat$stress_lag <- lagvar(stress, id=subject, obs=beep, day=dayno, data=dat, lag=2)
dat

# lag the stress variable by up to 5 observations
# note: this creates two variables (xl and lag), which are both added to the dataset
dat$stress_lag <- lagvar(stress, id=subject, obs=beep, day=dayno, data=dat, lag=1:5)$xl
dat$lags <- lagvar(stress, id=subject, obs=beep, day=dayno, data=dat, lag=1:5)$lag
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
2 2 7  NA
2 2 8  NA
2 2 9  5
2 2 10 2")
dat

# lag the stress variable
dat$stress_lag <- lagvar(stress, id=subject, obs=beep, day=dayno, data=dat)
dat

# in this example, consecutive observations from the same subject are each
# one lag apart, so one can leave out the 'obs' (i.e., 'beep') argument here
dat$stress_lag <- lagvar(stress, id=subject, day=dayno, data=dat)
dat

# lag the stress variable by up to 5 observations
dat$stress_lag <- lagvar(stress, id=subject, day=dayno, data=dat, lag=1:5)$xl
dat$lags <- lagvar(stress, id=subject, day=dayno, data=dat, lag=1:5)$lag
dat
}
\keyword{manip}
