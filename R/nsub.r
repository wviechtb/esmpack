nsub <- function(id, data) {

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

   # get 'id' argument (will be NULL when unspecified)

   mf.id <- mf[[match("id", names(mf))]]
   id    <- eval(mf.id, data, enclos=sys.frame(sys.parent()))

   # check that 'id' have been specified

   if (is.null(id))
      stop("Argument 'id' must be specified.")

   # check that 'id' has no missings

   if (any(is.na(id)))
      stop("Argument 'id' should not contain any NAs.")

   return(length(unique(id)))

}
