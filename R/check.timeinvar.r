check.timeinvar <- function(x, id, data, which=FALSE) {

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

   is.constant <- function(x) {
      if (all(is.na(x))) {
         TRUE
      } else {
         all(na.omit(x) == na.omit(x)[1])
      }
   }

   is.const <- tapply(x, id, is.constant)

   all.const <- all(is.const)

   if (which) {
      if (all.const) {
         NULL
      } else {
         # get subject ids
         id <- unname(tapply(id, id, function(x) x[1]))
         id[!is.const]
      }
   } else {
      if (all.const) {
         TRUE
      } else {
         FALSE
      }
   }

}
