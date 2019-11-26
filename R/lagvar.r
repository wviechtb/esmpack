lagvar <- function(x, id, obs, day, data, lag=1) {

   # check if data argument has been specified

   if (missing(data))
      data <- NULL

   if (is.null(data)) {
      data <- sys.frame(sys.parent())
   } else {
      if (!is.data.frame(data))
         data <- data.frame(data)
   }

   mf <- match.call()

   # get x, id, obs, and day arguments (will be NULL when unspecified)

   mf.x   <- mf[[match("x",   names(mf))]]
   mf.id  <- mf[[match("id",  names(mf))]]
   mf.obs <- mf[[match("obs", names(mf))]]
   mf.day <- mf[[match("day", names(mf))]]
   x      <- eval(mf.x,   data, enclos=sys.frame(sys.parent()))
   id     <- eval(mf.id,  data, enclos=sys.frame(sys.parent()))
   obs    <- eval(mf.obs, data, enclos=sys.frame(sys.parent()))
   day    <- eval(mf.day, data, enclos=sys.frame(sys.parent()))

   # if 'id' is not specified, assume data are from a single subject

   if (is.null(id))
      id <- rep(1, length(x))

   # if obs variable is missing, set it to consecutive integers within subjects

   if (is.null(obs))
      obs <- unsplit(lapply(split(id, id), seq_along), id)

   # if day variable is missing, set it to 1 for all observations
   # that way, lags across days are possible / permitted

   if (is.null(day))
      day <- rep(1, length(x))

   # check that x has been specified

   if (is.null(x))
      stop("Argument 'x' must be specified.")

   # check that lengths of x, id, obs, and day match

   if (any(length(x) != c(length(id), length(obs), length(day))))
      stop("Arguments 'x' and 'id' (and 'obs' and 'day' if specified) must be of the same length.")

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

   dat <- data.frame(x=x, id=id, day=day, obs=obs)

   res <- lapply(split(dat, dat$id), function(sub) {

      n <- nrow(sub)
      xl <- rep(NA, n)
      ll <- rep(NA, n)

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
                  break
               }

            }

         }

      }

      return(data.frame(xl=xl, ll=ll))

   })

   xl <- unsplit(lapply(res, function(x) x$xl), dat$id)
   ll <- unsplit(lapply(res, function(x) x$ll), dat$id)

   #########################################################################

   if (m == 1) {
      return(xl)
   } else {
      return(data.frame(xl=xl, lag=ll))
   }

}
