set_globals <- function(var, val, style = DEFAULT) {
  index <- which(var == globals$param)
  if (0 == length(index)) {
    stop("TODO")
  } else if (1 == length(index)) {
    globals[index, style] <<- val # TODO Fix global assignment
  } else stop("TODO")
}