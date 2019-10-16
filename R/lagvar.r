lagvar <- function(x, obs, id, day, data, lag=1) {

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

   # if day variable is missing, set it to 1 for all observations
   # that way, lags across days are possible / permitted

   dayspec <- TRUE
   if (is.null(day)) {
      dayspec <- FALSE
      day <- rep(1, length(x))
   }

   # check that x and obs have been specified

   if (is.null(x))
      stop("Argument 'x' must be specified.")
   if (is.null(obs))
      stop("Argument 'obs' must be specified.")

   # if 'id' is not specified, assume data are from a single subject

   if (is.null(id))
      id <- rep(1, length(x))

   # check that lengths of x, id, obs, and day match

   if (any(length(x) != c(length(id), length(obs), length(day)))) {
      if (dayspec) {
         stop("Arguments 'x', 'id', 'obs', and 'day' must be of the same length.")
      } else {
         stop("Arguments 'x', 'id', and 'obs' must be of the same length.")
      }
   }

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

   if (length(lag) != 1L)
      stop("Argument 'lag' must be of length 1.")

   if (!is.numeric(lag))
      stop("Argument 'lag' must be a number.")

   lag <- as.integer(lag)

   if (lag < 1L)
      stop("Argument 'lag' must be >= 1.")

   #########################################################################

   dat <- data.frame(x=x, id=id, day=day, obs=obs)

   xl <- lapply(split(dat, dat$id), function(sub) {

      n <- nrow(sub)
      xl <- rep(NA, n)

      for (i in 1:n) {

         if (sub$obs[i] - lag <= 0)
            next

         pos <- which(sub$obs == sub$obs[i]-lag)

         if (any(pos)) {

            if (sub$day[i] != sub$day[pos]) {
               next
            } else {
               xl[i] <- sub$x[pos]
            }

         }

      }

      return(xl)

   })

   xl <- unsplit(xl, dat$id)

   #########################################################################

   return(xl)

}
