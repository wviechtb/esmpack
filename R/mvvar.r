mvvar <- function(var, where, after=TRUE, data) {

   # check if 'data' argument has been specified

   if (missing(data))
      stop("Must specify 'data' argument.")

   # if 'data' is not a data frame, turn it into one

   if (!(is.data.frame(data)))
      data <- data.frame(data)

   # get variable names in data frame

   varnames <- names(data)

   # number of variables in the data frame

   nvars <- length(varnames)

   # get position of 'var' and 'where'

   pos.var   <- charmatch(var, varnames)
   pos.where <- charmatch(where, varnames)

   # checks on 'pos.var' and 'pos.where'

   if (is.na(pos.var))
      stop("Variable '", var, "' not found in the data set.")

   if (is.na(pos.where))
      stop("Variable '", where, "' not found in the data set.")

   if (pos.var == 0)
      stop("Multiple matches found for variable '", var, "' in the data set.")

   if (pos.where == 0)
      stop("Multiple matches found for variable '", where, "' in the data set.")

   #########################################################################

   if (pos.where == pos.var)
      return(data)

   if (pos.where == 1 && !after)
      return(data[c(pos.var, setdiff(1:nvars, pos.var))])

   if (pos.where == nvars && after)
      return(data[c(setdiff(1:nvars, pos.var), pos.var)])

   pos.before <- setdiff(1:(pos.where + ifelse(after, 0, -1)), pos.var)
   pos.after  <- setdiff((pos.where + ifelse(after, 1, 0)):nvars, pos.var)

   return(data[c(pos.before, pos.var, pos.after)])

   #########################################################################

}
