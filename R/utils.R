#' Critical value z(alpha/2) for a given confidence level
#'
#' @param conf Numeric confidence level, e.g. `0.95`.
#' @return Numeric critical value from the standard normal distribution.
#' @keywords internal
z_critico <- function(conf) {
  if (!is.numeric(conf) || length(conf) != 1L || conf <= 0 || conf >= 1) {
    stop("`conf` deve ser um numero entre 0 e 1 (ex.: 0.95).", call. = FALSE)
  }
  stats::qnorm(1 - (1 - conf) / 2)
}

#' Validate that a numeric scalar is finite and within an optional range
#'
#' @param x Value to validate.
#' @param nome Character. Name of the argument, used in error messages.
#' @param min Optional numeric lower bound (inclusive).
#' @param max Optional numeric upper bound (inclusive).
#' @param permitir_inf Logical. If `TRUE`, `Inf` is accepted as a valid
#'   value (used for arguments like population size `N` where `Inf`
#'   deliberately represents an unknown/very large population).
#' @return `x`, invisibly, if valid; otherwise raises an error.
#' @keywords internal
validar_escalar <- function(x, nome, min = -Inf, max = Inf, permitir_inf = FALSE) {
  if (is.null(x)) return(invisible(x))
  if (permitir_inf && is.numeric(x) && length(x) == 1L && is.infinite(x) && x > 0) {
    return(invisible(x))
  }
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop(sprintf("`%s` deve ser um numero finito unico.", nome), call. = FALSE)
  }
  if (x < min || x > max) {
    stop(sprintf("`%s` deve estar entre %s e %s (valor informado: %s).",
                  nome, min, max, x), call. = FALSE)
  }
  invisible(x)
}

#' Validate a proportion (must be between 0 and 1, inclusive)
#'
#' @param p Value to validate.
#' @param nome Character. Name of the argument, used in error messages.
#' @return `p`, invisibly, if valid; otherwise raises an error.
#' @keywords internal
validar_proporcao <- function(p, nome = "p") {
  validar_escalar(p, nome, min = 0, max = 1)
}

#' Format a short bullet-point calculation step
#'
#' @param ... Passed to [sprintf()] to build the message.
#' @return A character scalar.
#' @keywords internal
passo <- function(...) sprintf(...)
