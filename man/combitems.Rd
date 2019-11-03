\name{combitems}
\alias{combitems}
\title{Combine Several Items}
\description{Function to combine several items (e.g., by taking their mean).}
\usage{
combitems(items, data, grep=FALSE, fun=mean, na.rm=TRUE, min.k, verbose=TRUE)
}
\arguments{
   \item{items}{either a character vector (giving the names of the variables to be combined) or a numeric vector (giving the position of the columns in the data frame corresponding to the items).}
   \item{data}{data frame that contains the items to be combined.}
   \item{grep}{logical indicating whether item names should be matched using \code{\link{grep}} (default is \code{FALSE}).}
   \item{fun}{the function to be applied to the items (default is \code{mean}).}
   \item{na.rm}{logical indicating whether missing values should be removed before applying the function (default is \code{TRUE}).}
   \item{min.k}{integer specifying the minimum number of items that must be available (i.e., not missing). If not specified, no minimum is required.}
   \item{verbose}{logical indicating whether information about the items that were combined should be reported (default is \code{TRUE}).}
}
\details{
   The function can be used to combine several items (usually by taking their mean or their sum) into a new variable (the default is to take the mean).

   The items to be combined are specified via the \code{items} argument, either as a character vector with the variable names or as a numeric vector giving the item position in the data frame. One can also just specify substrings for the item names, which are matched (if possible) against the variable names in the data frame. See \sQuote{Examples}.

   The function to be applied to the items is specified via the \code{fun} argument and must be a function that returns a single number.

   At times, one may require that a certain number of items must be available (i.e., not missing) before computing the function. This can be specified via the \code{min.k} argument.
}
\value{
   The function returns a vector (that will typically be added back to the data frame as a new variable).
}
\author{
   Wolfgang Viechtbauer \email{wvb@wvbauer.com} \cr
}
\examples{
# illustrative dataset
dat <- structure(list(id=1:10,
   mood_cheerf  = c(5, 6, 6, 6,  7, 6,  7, 7, 6, 7),
   mood_relaxed = c(6, 6, 6, 7,  7, NA, 5, 5, 5, 6),
   mood_satisfi = c(6, 6, 5, NA, 7, NA, 6, 6, 5, 6)),
   row.names = 1:10, class = "data.frame")
dat

# compute the average of the three mood items and add
# this as a new variable (called 'pa') to the dataset
dat$pa <- combitems(c("mood_cheerf", "mood_relaxed", "mood_satisfi"), data=dat)
dat

# can also grep for the item names
dat$pa <- combitems(c("cheerf", "relaxed", "satisfi"), data=dat, grep=TRUE)
dat

# even shorter
dat$pa <- combitems("mood", data=dat, grep=TRUE)
dat

# can also just specify the position of the items in the dataset
dat$pa <- combitems(2:4, data=dat)
dat

# can specify the minimum number of required non-missing items
# note: the mean for id=6 is NA because only one item is non-missing
dat$pa <- combitems("mood", data=dat, grep=TRUE, min.k=2)
dat
}
\keyword{manip}
