check.nodup <- function(x, id, data, out=1, na.rm=TRUE) {

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

   # check if 'x' has no duplicated values within subjects

   no.dup <- function(x, na.rm) {
      if (all(is.na(x))) {
         TRUE
      } else {
         if (na.rm) {
            !any(duplicated(na.omit(x)))
         } else {
            !any(duplicated(x))
         }
      }
   }

   nodup <- tapply(x, id, no.dup, na.rm)

   # check if 'x' has no duplicated values for all subjects

   all.nodup <- all(nodup)

   # get subject ids

   ids <- unname(tapply(id, id, function(x) x[1]))

   #########################################################################

   # prepare output

   if (out == 1) {
      if (all.nodup) {
         return(TRUE)
      } else {
         return(FALSE)
      }
   }

   if (out == 2) {
      if (all.nodup) {
         return(NULL)
      } else {
         return(ids[!nodup])
      }
   }

   if (out == 3) {
      if (all.nodup) {
         return(NULL)
      } else {
         ids <- ids[!nodup]
         if (no.data) {
            return(data.frame(id, x)[id %in% ids,,drop=FALSE])
         } else {
            return(data[id %in% ids,,drop=FALSE])
         }
      }
   }

}
