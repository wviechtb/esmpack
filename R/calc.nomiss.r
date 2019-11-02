calc.nomiss <- function(x, id, data, prop=FALSE, expand=FALSE) {

   # check if 'data' argument has been specified

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

   # check that 'id' has been specified

   if (is.null(id))
      stop("Argument 'id' must be specified.")

   # check that 'id' has no missings

   if (any(is.na(id)))
      stop("Argument 'id' should not contain any NAs.")

   #########################################################################

   if (is.null(x))
      x <- rep(1,length(id))

   nomiss <- function(x, prop) {
      if (prop) {
         mean(!is.na(x))
      } else {
         sum(!is.na(x))
      }
   }

   if (expand) {
      res <- ave(x, id, FUN=function(x) nomiss(x, prop=prop))
   } else {
      res <- tapply(x, id, FUN=nomiss, prop=prop)
   }

   return(res)

}
