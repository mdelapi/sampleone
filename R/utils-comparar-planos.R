#' Compare the variance of different sampling plans for the same problem
#'
#' Convenience function that lines up the estimated variance obtained
#' under different sampling designs (as returned by `sampling_result`
#' objects from this package) so they can be compared directly, computing
#' the sampling-plan effect (EPA / design effect) of each one relative to
#' a chosen baseline. This operationalises the comparisons illustrated
#' throughout the course notes (Section 3.7.7, Examples 4.4-4.5, and the
#' design-effect discussion in Section 6.1.5).
#'
#' @param ... Two or more named `sampling_result` objects (e.g.
#'   `aas = aas_estima_media(...)`, `estratificada = estr_estima_media(...)`),
#'   each with a `variancia` element containing `var_media` or `var_p`.
#' @param referencia Name of the plan to use as the baseline (denominator
#'   of the EPA ratio). Defaults to the first plan supplied.
#'
#' @return A `sampling_result` object with a comparison table of
#'   variances and EPA (design effect) ratios relative to `referencia`.
#' @examples
#' x <- rnorm(30, 50, 5)
#' plano_aas <- aas_estima_media(x, N = 300)
#' # (em um caso real, plano_estrat viria de estr_estima_media() com os
#' #  mesmos dados organizados em estratos)
#' amostragem_comparar_planos(aas = plano_aas)
#' @export
amostragem_comparar_planos <- function(..., referencia = NULL) {
  planos <- list(...)
  if (length(planos) < 1) stop("Forneca ao menos um plano (sampling_result nomeado).", call. = FALSE)
  if (is.null(names(planos)) || any(names(planos) == "")) {
    stop("Todos os planos devem ser passados com nome, ex.: aas = aas_estima_media(...).", call. = FALSE)
  }

  extrai_var <- function(p) {
    if (!is.null(p$variancia$var_media)) return(p$variancia$var_media)
    if (!is.null(p$variancia$var_p)) return(p$variancia$var_p)
    if (!is.null(p$variancia$var_total)) return(p$variancia$var_total)
    NA_real_
  }
  variancias <- vapply(planos, extrai_var, numeric(1))

  if (is.null(referencia)) referencia <- names(planos)[1]
  if (!referencia %in% names(planos)) stop("`referencia` deve ser um dos nomes fornecidos em `...`.", call. = FALSE)
  var_ref <- variancias[[referencia]]
  epa <- variancias / var_ref

  tabela <- data.frame(plano = names(planos), variancia = variancias, EPA_vs_referencia = epa)
  melhor <- names(which.min(variancias))

  new_sampling_result(
    metodo = "Comparacao de planos amostrais (EPA / design effect)",
    estimativa = list(tabela = tabela, referencia = referencia, mais_eficiente = melhor),
    memoria_calculo = passo("Referencia: %s (variancia = %s)", referencia, format(var_ref, digits = 6)),
    interpretacao = sprintf("O plano mais eficiente (menor variancia) entre os comparados e '%s'.", melhor),
    insight = "EPA < 1 significa que o plano e mais eficiente que a referencia; EPA > 1 significa menos eficiente. Ganhos de precisao devem ser ponderados contra o custo de coleta de cada plano.",
    referencias = "Bolfarine & Bussab (2005); Silva (2001).",
    dados_entrada = list(planos = names(planos), referencia = referencia)
  )
}
