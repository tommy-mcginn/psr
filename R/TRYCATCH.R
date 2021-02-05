TE_error_handle <- function(code) {

  tryCatch(expr = TE(subject, trial, ...),
           error = function(e) {
             print("Error: all of the metrics must be numeric vectors")
           },
           warning = function(w) {
             print("")
           },
           finally = {
             print("")
           }
  )
}
