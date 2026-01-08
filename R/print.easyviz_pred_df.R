#' @export
print.easyviz.pred.df <- function(x, ...) {
  if (!is.null(attr(x, "re_form_warning"))) {
    warning(attr(x, "re_form_warning"), call. = FALSE)
  }
  NextMethod()
}