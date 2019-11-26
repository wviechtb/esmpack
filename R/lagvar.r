lagvar <- function(x, id, obs, day, time, data, lag=1) {

   # check if 'data' argument has been specified

   if (missing(data))
      data <- NULL

   if (is.null(data)) {
      data <- sys.frame(sys.parent())
   } else {
      if (!is.data.frame(data))
         data <- data.frame(data)
   }

   mf <- match.call()

   # get 'x', 'id', 'obs', 'day', and 'time' arguments (will be NULL when unspecified)

   mf.x    <- mf[[match("x",    names(mf))]]
   mf.id   <- mf[[match("id",   names(mf))]]
   mf.obs  <- mf[[match("obs",  names(mf))]]
   mf.day  <- mf[[match("day",  names(mf))]]
   mf.time <- mf[[match("time", names(mf))]]
   x    <- eval(mf.x,    data, enclos=sys.frame(sys.parent()))
   id   <- eval(mf.id,   data, enclos=sys.frame(sys.parent()))
   obs  <- eval(mf.obs,  data, enclos=sys.frame(sys.parent()))
   day  <- eval(mf.day,  data, enclos=sys.frame(sys.parent()))
   time <- eval(mf.time, data, enclos=sys.frame(sys.parent()))

   # if 'id' is not specified, assume data are from a single subject

   if (is.null(id))
      id <- rep(1, length(x))

   # if 'obs' is not specified, set it to consecutive integers within subjects

   if (is.null(obs))
      obs <- unsplit(lapply(split(id, id), seq_along), id)

   # if 'day' is not specified, set it to 1 for all observations
   # that way, lags across days are possible / permitted

   if (is.null(day))
      day <- rep(1, length(x))

   # if 'time' is not specified, set it to 1

   timespec <- TRUE
   if (is.null(time)) {
      timespec <- FALSE
      time <- rep(1, length(x))
   }

   # check that 'x' has been specified

   if (is.null(x))
      stop("Argument 'x' must be specified.")

   # check that lengths of 'x', 'id', 'obs', 'day', and 'time' match

   if (any(length(x) != c(length(id), length(obs), length(day), length(time))))
      stop("Arguments 'x' and 'id' (and 'obs', 'day', and 'time' if specified) must be of the same length.")

   # checks on 'obs' argument

   if (!is.numeric(obs))
      stop("Argument 'obs' must be a numeric vector.")

   #if (any(obs != as.integer(obs)))
   #   stop("")

   # check for no duplicated obs within id

   obs.not.unique <- sapply(split(obs, id), function(x) length(x) != length(unique(x)))

   if (any(obs.not.unique))
      stop("Argument 'obs' must not have repeated values within subjects.")

   # checks on 'lag' argument

   if (!is.numeric(lag))
      stop("Argument 'lag' must be numeric.")

   lag <- sort(unique(as.integer(lag)))

   if (any(lag < 1L))
      stop("Argument 'lag' must be >= 1.")

   # number of lag values specified

   m <- length(lag)

   #########################################################################

   dat <- data.frame(x=x, id=id, day=day, obs=obs, time=time)

   res <- lapply(split(dat, dat$id), function(sub) {

      n <- nrow(sub)
      xl <- rep(NA, n)
      ll <- rep(NA, n)
      tl <- rep(NA, n)
      if (inherits(sub$time, "POSIXct")) {
         class(tl) <- "difftime"
         attr(tl, "units") <- "secs"
      }

      for (i in 1:n) {

         for (j in 1:m) {

            if (sub$obs[i] - lag[j] <= 0)
               break

            pos <- which(sub$obs == sub$obs[i]-lag[j])

            if (length(pos) > 0 && !is.na(sub$x[pos])) {

               if (sub$day[i] != sub$day[pos]) {
                  break
               } else {
                  xl[i] <- sub$x[pos]
                  ll[i] <- lag[j]
                  if (inherits(sub$time, "POSIXct")) {
                     tl[i] <- difftime(sub$time[i], sub$time[pos], units="secs")
                  } else {
                     tl[i] <- sub$time[i] - sub$time[pos]
                  }
                  break
               }

            }

         }

      }

      return(data.frame(xl=xl, ll=ll, tl=tl))

   })

   xl <- unsplit(lapply(res, function(x) x$xl), dat$id)
   ll <- unsplit(lapply(res, function(x) x$ll), dat$id)
   tl <- unsplit(lapply(res, function(x) x$tl), dat$id)

   #########################################################################

   if (timespec) {
      if (m == 1) {
         return(data.frame(xl=xl, timelag=tl))
      } else {
         return(data.frame(xl=xl, lag=ll, timelag=tl))
      }
   } else {
      if (m == 1) {
         return(xl)
      } else {
         return(data.frame(xl=xl, lag=ll))
      }
   }

}
