#' Return the value of the named environment variable(s), or a default value if NA
#'
#' The common use-case for this is when it is expected to setup execution-specific
#' variable values from a calling environment but if the script is run in a
#' standalone/testing mode that it can make use of the defaults to still
#' operate in a controlled manner.
#'
#' While bulk lookups can be accomplished readability will be hampered if
#' extremely long vectors are used.  In that situation multiple single-pair
#' calls is recommended.  This also has the beneficial property of not attaching
#' names to the resultant vector which makes variable assignment easier.
#'
#' Wraps a call to Sys.getenv
#'
#' @param envvar_names A vector of envrionment variable names to retrieve
#' @param values_if_missing A vector of strings, matched by position, to use as defaults
#' @return A vector (named if length > 1) of a matching length (> 0) with the conditioned values
#' @examples
#' env_or_default("UNKNOWN", "DEFAULT")
#' env_or_default(c("HOME", "UNKNOWN"), c("/tmp", "DEFAULT"))
#'
#' @name env_or_default
#' @export
env_or_default <- function(envvar_names, values_if_missing) {
  stopifnot(length(envvar_names) == length(values_if_missing),
            length(envvar_names) > 0)
  
  vals <- Sys.getenv(envvar_names, unset = NA)
  results <- ifelse(is.na(vals), values_if_missing, vals)  
  return(results)
}
