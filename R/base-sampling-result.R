#' Construct a sampling_result object
#'
#' Internal constructor for the S3 class shared by every estimation and
#' sample-size function in the package. It bundles the point estimate,
#' variance/standard error, confidence interval, a step-by-step record of
#' the calculation, and a short pedagogical note into a single object so
#' that every function in `sampleone` returns a consistent, richly
#' documented result rather than a bare number.
#'
#' @param metodo Character. Short label for the sampling method/formula
#'   used (e.g. `"AAS sem reposicao - media"`).
#' @param estimativa Named list with the main numeric result(s), e.g.
#'   `list(media = 20, n = 47)`.
#' @param variancia Optional named list with variance/standard-error
#'   components.
#' @param intervalo_confianca Optional numeric vector of length 2, or
#'   `NULL` when not applicable.
#' @param nivel_confianca Numeric confidence level used, e.g. `0.95`.
#' @param memoria_calculo Character vector with a human-readable,
#'   step-by-step trace of the calculation.
#' @param interpretacao Character scalar with a plain-language
#'   interpretation of the result.
#' @param insight Character scalar with a short pedagogical suggestion
#'   (e.g. pointing to a related method worth comparing).
#' @param referencias Character vector of bibliographic references backing
#'   the formula used, beyond the course notes.
#' @param formula_apostila Character scalar identifying the equation
#'   number(s) in the source course notes (e.g. `"(3.15)"`).
#' @param dados_entrada Named list with the (validated) inputs actually
#'   used by the function, for transparency and reproducibility.
#'
#' @return An object of class `sampling_result`.
#' @keywords internal
new_sampling_result <- function(metodo,
                                 estimativa,
                                 variancia = NULL,
                                 intervalo_confianca = NULL,
                                 nivel_confianca = NULL,
                                 memoria_calculo = character(),
                                 interpretacao = NULL,
                                 insight = NULL,
                                 referencias = character(),
                                 formula_apostila = character(),
                                 dados_entrada = list()) {
  structure(
    list(
      metodo = metodo,
      estimativa = estimativa,
      variancia = variancia,
      intervalo_confianca = intervalo_confianca,
      nivel_confianca = nivel_confianca,
      memoria_calculo = memoria_calculo,
      interpretacao = interpretacao,
      insight = insight,
      referencias = referencias,
      formula_apostila = formula_apostila,
      dados_entrada = dados_entrada
    ),
    class = "sampling_result"
  )
}

#' Print method for sampling_result objects
#'
#' @param x A `sampling_result` object.
#' @param ... Further arguments (unused, kept for S3 consistency).
#' @return `x`, invisibly.
#' @export
print.sampling_result <- function(x, ...) {
  cat("<sampleone> ", x$metodo, "\n", sep = "")
  if (length(x$formula_apostila)) {
    cat("Formula(s): ", paste(x$formula_apostila, collapse = ", "), "\n", sep = "")
  }
  cat(strrep("-", 60), "\n", sep = "")

  if (length(x$estimativa)) {
    cat("Estimativa:\n")
    for (nm in names(x$estimativa)) {
      cat(sprintf("  %-20s %s\n", nm, format(x$estimativa[[nm]], digits = 6)))
    }
  }

  if (length(x$variancia)) {
    cat("Variancia / erro padrao:\n")
    for (nm in names(x$variancia)) {
      cat(sprintf("  %-20s %s\n", nm, format(x$variancia[[nm]], digits = 6)))
    }
  }

  if (!is.null(x$intervalo_confianca)) {
    conf_pct <- if (!is.null(x$nivel_confianca)) paste0(100 * x$nivel_confianca, "%") else ""
    cat(sprintf("IC %s: [%s ; %s]\n",
                conf_pct,
                format(x$intervalo_confianca[1], digits = 6),
                format(x$intervalo_confianca[2], digits = 6)))
  }

  if (!is.null(x$interpretacao)) {
    cat("\nInterpretacao:\n  ", x$interpretacao, "\n", sep = "")
  }

  if (!is.null(x$insight)) {
    cat("\nInsight pedagogico:\n  ", x$insight, "\n", sep = "")
  }

  invisible(x)
}

#' Summary method for sampling_result objects
#'
#' Prints the print method output plus the full step-by-step calculation
#' trace and bibliographic references.
#'
#' @param object A `sampling_result` object.
#' @param ... Further arguments (unused, kept for S3 consistency).
#' @return `object`, invisibly.
#' @export
summary.sampling_result <- function(object, ...) {
  print(object)
  if (length(object$memoria_calculo)) {
    cat("\nMemoria de calculo:\n")
    for (linha in object$memoria_calculo) cat("  ", linha, "\n", sep = "")
  }
  if (length(object$referencias)) {
    cat("\nReferencias:\n")
    for (r in object$referencias) cat("  - ", r, "\n", sep = "")
  }
  invisible(object)
}
