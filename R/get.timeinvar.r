get.timeinvar <- function(x, id, data, na.rm=TRUE) {

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

   # get x and id arguments (will be NULL when unspecified)

   mf.x  <- mf[[match("x",  names(mf))]]
   mf.id <- mf[[match("id", names(mf))]]
   x     <- eval(mf.x,  data, enclos=sys.frame(sys.parent()))
   id    <- eval(mf.id, data, enclos=sys.frame(sys.parent()))

   # check that x and id have been specified

   if (is.null(x))
      stop("Argument 'x' must be specified.")
   if (is.null(id))
      stop("Argument 'id' must be specified.")

   # check that id has no missings

   if (any(is.na(id)))
      stop("Argument 'id' should not contain any NAs.")

   # check that x is time-invariant within subjects

   is.const <- check.timeinvar(x, id, out=2)

   if (!is.null(is.const))
      warning("Variable 'x' is not time-invariant for the following subjects:\n  ", paste(is.const, collapse=", "))

   #########################################################################

   firstnotna <- function(x)
      c(na.omit(x)[1])

   #x <- tapply(x, id, firstnotna) # this drops factors (if x is a factor)
   x <- sapply(split(x, id), firstnotna)

   if (na.rm)
      x <- x[!is.na(x)]

   return(x)

}
