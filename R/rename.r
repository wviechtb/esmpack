rename <- function(old, new, data) {

   # check if 'data' argument has been specified

   if (missing(data))
      stop("Must specify 'data' argument.")

   if (missing(old) || missing(new))
      stop("Must specify 'old' and 'new' arguments.")

   if (length(old) != length(new))
      stop("Length of 'old' and 'new' arguments must be the same.")

   if (anyDuplicated(old) || anyDuplicated(new))
      stop("Argument 'old' or 'new' contains duplicates.")

   # if 'data' is not a data frame, turn it into one

   if (!(is.data.frame(data)))
      data <- data.frame(data)

   # get variable names in data frame

   varnames <- names(data)

   # get position(s) of 'old'

   pos.old <- charmatch(old, varnames)

   if (any(is.na(pos.old))) {
      if (sum(is.na(pos.old)) > 1L) {
         stop("Variables '", paste(old[is.na(pos.old)], collapse=", "), "' not found in the data set.")
      } else {
         stop("Variable '", old[is.na(pos.old)], "' not found in the data set.")
      }
   }

   if (any(pos.old == 0)) {
      if (sum(pos.old == 0) > 1L) {
         stop("Multiple matches found for variables '", paste(old[pos.old == 0], collapse=", "), "' found in the data set.")
      } else {
         stop("Multiple matches found for variable '", old[pos.old == 0], "' in the data set.")
      }
   }

   # get position(s) of 'new'

   pos.new <- match(new, varnames)

   if (any(!is.na(pos.new))) {
      if (sum(!is.na(pos.new)) > 1L) {
         warning("The dataset already contains variables named '", paste(new[!is.na(pos.new)], collapse=", "), "'.")
      } else {
         warning("The dataset already contains a variable named '", new[!is.na(pos.new)], "'.")
      }
   }

   names(data)[pos.old] <- new

   return(data)

}
