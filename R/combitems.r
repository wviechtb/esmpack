combitems <- function(items, data, fun = mean, na.rm = TRUE, min.k, verbose = TRUE) {

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

      #items.pos <- charmatch(items, varnames)

      items.pos <- lapply(items, function(x) {
         pos <- grep(x, varnames, fixed = TRUE)
         if (length(pos) == 0L) {
            return(NA)
         } else {
            return(pos)
         }
      })

      if (any(is.na(items.pos)))
         stop(ifelse(sum(is.na(items.pos)) == 1L, "Item", "Items"), " not found in the data frame: ", paste(items[is.na(items.pos)], collapse = ", "), ".")

      items.pos <- unique(unlist(items.pos))

      #if (any(items.pos == 0L))
      #   stop("No ambiguous match found for ", ifelse(sum(items.pos == 0L) == 1L, "item ", "items "), "(", paste(items[items.pos == 0], collapse = ", "), ").")

   } else {

      items.pos <- unique(round(items))

      if (min(items.pos) < 1 | max(items.pos) > nvars)
         stop("Item positions must be between 1 and ", nvars, ".")

   }

   # get items

   items <- data[items.pos]

   # apply function

   x <- apply(items, 1, FUN = fun, na.rm = na.rm)

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
