aggitems <- function(items, data, min.k, na.rm=TRUE, fun=mean, verbose=TRUE) {

   ### if 'data' is not a data frame, turn it into one

   if (!(is.data.frame(data)))
      data <- data.frame(data)

   ### get variable names in data frame

   varnames <- names(data)

   ### number of variables in the data frame

   nvars <- length(varnames)

   ### check that 'items' argument is either a character or a numeric vector

   if (!(is.character(items) | is.numeric(items)))
      stop(mstyle$stop("Argument 'items' must either be a character or a numeric vector."))

   if (is.character(items)) {

      items.pos <- charmatch(items, varnames)

      if (any(is.na(items.pos)))
         stop(ifelse(sum(is.na(items.pos)) == 1L, "Item ", "Items "), "(", paste(items[is.na(items.pos)], collapse=", "), ") not found in the data frame.")

      if (any(items.pos == 0L))
         stop("No ambiguous match found for ", ifelse(sum(items.pos == 0L) == 1L, "item ", "items "), "(", paste(items[items.pos == 0], collapse=", "), ").")

   } else {

      items.pos <- round(items)

      if (min(items.pos) < 1 | max(items.pos) > nvars)
         stop("Item positions must be between 1 and ", nvars, ".")

   }

   ### get items

   items <- data[items.pos]

   ### apply function

   x <- apply(items, 1, FUN = fun, na.rm = na.rm)

   ### in case min.k is specified, apply the 'minimum k' rule

   if (!missing(min.k)) {
      k.not.na <- apply(items, 1, FUN=function(x) sum(!is.na(x)))
      x[k.not.na < min.k] <- NA
   }

   #if (verbose)
   #   message(paste0("I just took the average of items ",

   return(x)

}
