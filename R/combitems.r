combitems <- function(items, data, grep=FALSE, fun=mean, na.rm=TRUE, min.k, verbose=TRUE) {

   # if 'data' is not a data frame, turn it into one

   if (!(is.data.frame(data)))
      data <- data.frame(data)

   # get variable names in data frame

   varnames <- names(data)

   # number of variables in the data frame

   nvars <- length(varnames)

   # check that 'items' argument is either a character or a numeric vector

   if (!(is.character(items) | is.numeric(items)))
      stop("Argument 'items' must either be a character or a numeric vector.")

   if (is.character(items)) {

      if (grep) {

         items.pos <- lapply(items, function(x) {
            pos <- grep(x, varnames, fixed = TRUE) # integer(0) if no match
            if (length(pos) == 0L)
               stop("Item '", x, "' not found in the data frame.", call.=FALSE)
            return(pos) # might be multiple values
         })

      } else {

         items.pos <- lapply(items, function(x) {
            pos <- charmatch(x, varnames) # 0 if multiple matches, NA if no match
            if (is.na(pos))
               stop("Item '", x, "' not found in the data frame.", call.=FALSE)
            if (pos == 0L)
               stop("Multiple matches for item '", x, "' in the data frame.", call.=FALSE)
            return(pos)
         })

      }

      items.pos <- unique(unlist(items.pos))

   } else {

      items.pos <- unique(round(items))

      if (min(items.pos) < 1 | max(items.pos) > nvars)
         stop("Item positions must be between 1 and ", nvars, ".")

   }

   # get items

   items <- data[items.pos]

   # apply function

   x <- apply(items, 1, FUN = fun, na.rm = na.rm)

   # replace NaN () with NA

   x[is.nan(x)] <- NA

   # in case min.k is specified, apply the 'minimum k' rule

   if (!missing(min.k)) {

      if (length(min.k) != 1)
         stop("Argument 'min.k' must be of length 1.")

      if (!is.numeric(min.k))
         stop("Argument 'min.k' must be a number.")

      min.k <- as.integer(min.k)

      if (min.k < 1L || min.k > length(items.pos))
         stop("Argument 'min.k' should be a number between 1 and the number of items to be combined.")

      k.not.na <- apply(items, 1, FUN = function(x) sum(!is.na(x)))
      x[k.not.na < min.k] <- NA
   }

   if (verbose)
      message(paste0("Computed '", as.character(substitute(fun)), "' for these items: ", paste0(varnames[items.pos], collapse = ", "), "."))

   return(x)

}
