calc.fun <- function(x, id, data, FUN, expand=FALSE, ...) {

   # check if 'data' argument has been specified

   if (missing(FUN))
      stop("Argument 'FUN' must be specified.")

   if (missing(data))
      data <- NULL

   no.data <- is.null(data)

   if (no.data) {
      data <- sys.frame(sys.parent())
   } else {
      if (!is.data.frame(data))
         data <- data.frame(data)
   }

   mf <- match.call()

   # get 'x' and 'id' arguments (will be NULL when unspecified)

   mf.x  <- mf[[match("x",  names(mf))]]
   mf.id <- mf[[match("id", names(mf))]]
   x     <- eval(mf.x,  data, enclos=sys.frame(sys.parent()))
   id    <- eval(mf.id, data, enclos=sys.frame(sys.parent()))

   # check that 'x' and 'id' have been specified

   if (is.null(x))
      stop("Argument 'x' must be specified.")
   if (is.null(id))
      stop("Argument 'id' must be specified.")

   # check that 'id' has no missings

   if (any(is.na(id)))
      stop("Argument 'id' should not contain any NAs.")

   #########################################################################

   if (expand) {
      # as in ave(), but changed so ... can be used to pass additional arguments
      g <- interaction(id)
      split(x, g) <- lapply(split(x, g), FUN, ...)
      return(x)
   } else {
      res <- tapply(x, id, FUN=FUN, ...)
      return(res)
   }

}
