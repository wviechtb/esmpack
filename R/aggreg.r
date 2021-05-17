aggreg <- function(data, id, vars, grep=FALSE, na.rm=TRUE) {

   # check if 'data' argument has been specified

   if (missing(data))
      stop("Argument 'data' must be specified.")

   if (!is.data.frame(data))
      data <- data.frame(data)

   # get variable names in data frame

   varnames <- names(data)

   # number of variables in the data frame

   nvars <- length(varnames)

   mf <- match.call()

   # get 'id' argument (will be NULL when unspecified)

   mf.id <- mf[[match("id", names(mf))]]
   id    <- eval(mf.id, data, enclos=sys.frame(sys.parent()))

   # check that 'id' has been specified

   if (is.null(id))
      stop("Argument 'id' must be specified.")

   # check that 'id' has no missings

   if (any(is.na(id)))
      stop("Argument 'id' should not contain any NAs.")

   if (missing(vars)) {
      vars.pos <- 1:nvars
   } else {

      # check that 'vars' argument is either a character or a numeric vector

      if (!(is.character(vars) | is.numeric(vars)))
         stop("Argument 'vars' must either be a character or a numeric vector.")

      if (is.character(vars)) {

         if (grep) {

            vars.pos <- lapply(vars, function(x) {
               pos <- grep(x, varnames, fixed = TRUE) # integer(0) if no match
               if (length(pos) == 0L)
                  stop("Variable '", x, "' not found in the data frame.", call.=FALSE)
               return(pos) # might be multiple values
            })

         } else {

            vars.pos <- lapply(vars, function(x) {
               pos <- charmatch(x, varnames) # 0 if multiple matches, NA if no match
               if (is.na(pos))
                  stop("Variable '", x, "' not found in the data frame.", call.=FALSE)
               if (pos == 0L)
                  stop("Multiple matches for variable '", x, "' in the data frame.", call.=FALSE)
               return(pos)
            })

         }

         vars.pos <- unique(unlist(vars.pos))

      } else {

         vars.pos <- unique(round(vars))

         if (min(vars.pos) < 1 | max(vars.pos) > nvars)
            stop("Variable positions must be between 1 and ", nvars, ".")

      }

   }

   data <- data[vars.pos]

   #########################################################################

   res <- stats::aggregate(data, by = list(id),
      FUN = function(x, rm.na=na.rm) {
         if (inherits(x, c("numeric","integer","logical"))) {
            mean(x, na.rm=rm.na)
         } else if (class(x) == "factor") {
            if (rm.na) droplevels(na.omit(x)[1]) else droplevels(x[1])
         } else {
            if (rm.na) na.omit(x)[1] else x[1]
         }
      })

   res <- res[-1]

   return(res)

}
