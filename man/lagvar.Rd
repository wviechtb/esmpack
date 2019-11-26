\name{lagvar}
\alias{lagvar}
\title{Create a Lagged Variable}
\description{Function to create a lagged variable.}
\usage{
lagvar(x, id, obs, day, time, data, lag=1)
}
\arguments{
   \item{x}{vector to specify the variable to be lagged.}
   \item{id}{optional vector to specify a subject id variable.}
   \item{obs}{optional numeric vector that indicates the observation number of the values specified via the \code{x} argument.}
   \item{day}{optional vector to specify a day number variable.}
   \item{time}{optional vector to specify a variable that indicates the actual date and time of the observations.}
   \item{data}{optional data frame that contains the variables specified above.}
   \item{lag}{scalar specifying the lag (default is \code{1}). Can also be a vector with multiple lag values that are permitted. See \sQuote{Details}.}
}
\details{
   The function can be used to create a lagged version of the variable specified via the \code{x} argument.

   The (optional) \code{id} argument is used to specify a subject id variable. If not specified, all observations are assumed to come from the same subject. If the dataset includes multiple subjects, then this variable needs to be specified so that observations are not lagged across subjects.

   The (optional) \code{obs} argument is used to specify a variable that indicates the observation number of the values specified via the \code{x} argument. There should not be repeated values of \code{obs} (within the same subject, in case the dataset includes multiple subjects). If not specified, then consecutive observations from the same subject are assumed to be one lag apart.

   The (optional) \code{day} variable is used to specify a day number variable. If not specified, then values of the variable to be lagged are allowed to be lagged across days (in case there are multiple observation days). If the \code{day} variable is specified, then lagged values across different days are not allowed (i.e., are set to missing).

   The (optional) \code{time} variable is used to specify a variable that indicates the actual date and time of the observations. Ideally this should be a \code{\link{POSIXct}} variable, but can also be numeric as long as this variable indicates the passage of time (e.g., in minutes) since some reference point (e.g., the first observation of each subject).

   The \code{lag} argument is used to specify the number of lags (i.e., the lagged value of the variable at a particular observation number \eqn{t} is the value of the variable observed at observation number \eqn{t - lag}). If no lagged value is available at observation number \eqn{t - lag} (either because the dataset does not include that observation number or the value at that observation number is missing), then the lagged value will be missing.

   One can also specify multiple values for the \code{lag} argument (usually a consecutive range, such as \code{1:3}). In that case, the function will find the lagged value with the smallest lag and also indicate the lag of each lagged value. See \sQuote{Examples}.
}
\value{
   If \code{lag} is a single number, the function returns a vector with the lagged values (that will typically be added back to the data frame as a new variable).

   If a \code{time} variable is also specified, then the function returns a data frame with elements \code{xl} for the lagged values and \code{timelag} for the time lag between the observations (in seconds if the \code{time} variable was of class \code{POSIXct} and otherwise in the units of the \code{time} variable).

   If multiple values are specified for \code{lag}, the function returns a data frame with elements \code{xl} for the lagged values and \code{lag} for the lag of those values (and again \code{timelag} if a \code{time} variable was also specified).
}
\author{
   Wolfgang Viechtbauer \email{wvb@wvbauer.com} \cr
}
\examples{
# illustrative dataset (10 beeps for 2 days for 2 subjects)
dat <- structure(list(subject = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), dayno = c(1L, 1L, 1L, 1L, 1L,
1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L,
1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), beep =
c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L,
17L, 18L, 19L, 20L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L,
14L, 15L, 16L, 17L, 18L, 19L, 20L), beeptime = structure(c(1571551380,
1571558400, 1571563080, 1571567640, 1571573760, 1571580900, 1571582820,
1571590860, 1571595360, 1571599320, 1571640120, 1571643180, 1571650680,
1571656140, 1571660760, 1571666100, 1571671080, 1571675280, 1571681880,
1571688480, 1571812380, 1571815080, 1571823360, 1571828220, 1571832780,
1571838900, 1571842320, 1571851380, 1571854380, 1571859600, 1571900280,
1571903700, 1571910060, 1571915340, 1571922360, 1571925660, 1571931360,
1571938680, 1571943960, 1571948160), class = c("POSIXct", "POSIXt")),
relaxed = c(2, 6, 6, 6, 5, 6, 6, 6, NA, 5, 1, NA, 6, 6, 6, NA, 6, 6, 6, 4,
2, 7, 7, NA, NA, 7, 7, 1, 7, 6, NA, 7, 6, NA, NA, NA, 7, 4, 7, 7)),
row.names = c(NA, 40L), class = "data.frame")
dat

# lag the relaxed variable
dat$relaxed_lag <- lagvar(relaxed, id=subject, obs=beep, data=dat)
dat

# in this example, consecutive observations from the same subject are each
# one lag apart, so one can leave out the 'obs' (i.e., 'beep') argument here
dat$relaxed_lag <- lagvar(relaxed, id=subject, data=dat)
dat

# but this also allows lagged values across days (e.g., the value '5' from
# beep 10 on day 1 becomes the lagged value for beep 11 on day 2); this can
# be avoided by specifying the day variable
dat$relaxed_lag <- lagvar(relaxed, id=subject, obs=beep, day=dayno, data=dat)
dat

# lag the variable by two observations
dat$relaxed_lag <- lagvar(relaxed, id=subject, obs=beep, day=dayno, data=dat, lag=2)
dat

# lag the variable by up to 5 observations (taking whatever lag is smallest)
# note: this creates two variables (xl and lag), which we can add back to the dataset
tmp <- lagvar(relaxed, id=subject, obs=beep, day=dayno, data=dat, lag=1:5)
dat$relaxed_lag <- tmp$xl
dat$lag <- tmp$lag
dat

# remove those new variables
dat$relaxed_lag <- NULL
dat$lag <- NULL
dat

# lag the relaxed variable and also get time difference
tmp <- lagvar(relaxed, id=subject, obs=beep, day=dayno, time=beeptime, data=dat)
dat$relaxed_lag <- tmp$xl
dat$timelag <- tmp$timelag
dat

# timelag in minutes
dat$timelag <- as.numeric(tmp$timelag, units="mins")
dat

# remove those new variables
dat$relaxed_lag <- NULL
dat$timelag <- NULL
dat

# suppose the 'dayno' variable is not available
dat$dayno <- NULL
dat

# we can recreate this based on the 'beeptime' variable
dat$dayno <- unsplit(
   lapply(split(dat$beeptime, dat$subject), function(x) {
      z <- format(x, format="\%Y-\%m-\%d")
      as.numeric(difftime(z, z[1], units="days")) + 1
   }), dat$subject)
dat

# now suppose the dataset does not include beeps that were not responded to
dat <- dat[!is.na(dat$relaxed),]
dat

# in this case, specifying the 'obs' variable (i.e., 'beep') is crucial
# note: the lagged value for beep 10 is missing, since beep 9 is missing
dat$relaxed_lag <- lagvar(relaxed, id=subject, obs=beep, day=dayno, data=dat)
dat
}
\keyword{manip}
