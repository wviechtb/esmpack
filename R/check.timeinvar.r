check.timeinvar <- function(x, id, data, out=1, na.rm=TRUE) {

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

   # check that 'x' and 'id' have been specified

   if (is.null(x))
      stop("Argument 'x' must be specified.")
   if (is.null(id))
      stop("Argument 'id' must be specified.")

   # check that 'id' has no missings

   if (any(is.na(id)))
      stop("Argument 'id' should not contain any NAs.")

   # make sure id is a character variable

   id <- as.character(id)

   # check 'out' argument

   if (is.character(out)) {
      out <- pmatch(out, c("logical", "id", "data"))
      if (is.na(out))
         stop("Argument 'out' must be either set to 'logical', 'id', or 'data'.")
   }

   if (!(out %in% 1:3))
      stop("Argument 'out' must be set to 1, 2, 3.")

   #########################################################################

   # check if 'x' is time-invariant within subjects

   is.constant <- function(x, na.rm) {
      if (all(is.na(x))) {
         TRUE
      } else {
         res <- all(x == na.omit(x)[1], na.rm=na.rm)
         res[is.na(res)] <- FALSE
         res
      }
   }

   const <- tapply(x, id, is.constant, na.rm)

   # check if 'x' is time-invariant for all subjects

   all.const <- all(const)

   # get subject ids

   ids <- unname(tapply(id, id, function(x) x[1]))

   #########################################################################

   # prepare output

   if (out == 1) {
      if (all.const) {
         return(TRUE)
      } else {
         return(FALSE)
      }
   }

   if (out == 2) {
      if (all.const) {
         return(NULL)
      } else {
         return(ids[!const])
      }
   }

   if (out == 3) {
      if (all.const) {
         return(NULL)
      } else {
         ids <- ids[!const]
         if (no.data) {
            return(data.frame(id, x)[id %in% ids,,drop=FALSE])
         } else {
            return(data[id %in% ids,,drop=FALSE])
         }
      }
   }

}
