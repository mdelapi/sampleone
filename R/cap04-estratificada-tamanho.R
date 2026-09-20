#' Sample size for stratified sampling: mean (proportional / Neyman / cost)
#'
#' Computes the total sample size and, when `alocacao` is not `"nenhuma"`,
#' the per-stratum allocation, to estimate a population mean under
#' stratified sampling, dispatching automatically between the
#' proportional/Neyman formula (Eq. 4.5) and the cost-based formula
#' (Eq. 4.8) depending on whether `ck` is supplied. Extra arguments not
#' needed for the chosen formula are ignored.
#'
#' @param Nk Numeric vector with the population size of each stratum.
#' @param sigmak2 Numeric vector with the population variance of each
#'   stratum.
#' @param d Tolerable margin of error.
#' @param conf Confidence level, default `0.95`.
#' @param ck Optional numeric vector with the sampling cost per unit in
#'   each stratum. If supplied, the cost-based formula (Eq. 4.8) is used;
#'   otherwise the standard formula (Eq. 4.5) is used.
#' @param alocacao How to allocate `n` across strata once computed:
#'   `"proporcional"` (default), `"neyman"`, `"custo"` (requires `ck`), or
#'   `"nenhuma"` (only the total `n` is returned).
#' @param arredondar Either `"cima"` (default, always round up -- the
#'   conventional, conservative practice) or `"proximo"` (round to the
#'   nearest integer, reproducing the course notes' worked examples,
#'   which use a 2-decimal critical value and round to the nearest
#'   integer).
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the total sample size and,
#'   unless `alocacao = "nenhuma"`, the per-stratum allocation.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley, cap. 5.
#' @examples
#' estr_tamanho_media(Nk = c(1540, 770, 385), sigmak2 = c(7, 9, 11), d = 0.5, conf = 0.95)
#' @export
estr_tamanho_media <- function(Nk, sigmak2, d, conf = 0.95, ck = NULL,
                                alocacao = c("proporcional", "neyman", "custo", "nenhuma"),
                                arredondar = c("cima", "proximo"),
                                ...) {
  alocacao <- match.arg(alocacao)
  arredondar <- match.arg(arredondar)
  N <- sum(Nk)
  Wk <- Nk / N
  z <- z_critico(conf)
  d_star <- d^2 / z^2

  if (!is.null(ck)) {
    num <- sum(Nk * sqrt(sigmak2) / sqrt(ck)) * sum(Nk * sqrt(sigmak2) * sqrt(ck))
    den <- N^2 * d_star + sum(Nk * sigmak2)
    n_bruto <- num / den
    formula_usada <- "(4.8)"
  } else if (alocacao == "neyman") {
    # Tamanho total sob alocacao otima de Neyman (Eq. 4.21) -- distinto da
    # formula (4.5), usada para alocacao proporcional.
    sigmak <- sqrt(sigmak2)
    num <- (sum(Nk * sigmak))^2
    den <- N^2 * d_star + sum(Nk * sigmak2)
    n_bruto <- num / den
    formula_usada <- "(4.21)"
  } else {
    num <- sum(Nk^2 * sigmak2 / Wk)
    den <- N^2 * d_star + sum(Nk * sigmak2)
    n_bruto <- num / den
    formula_usada <- "(4.5)"
  }
  n <- if (arredondar == "cima") ceiling(n_bruto) else round(n_bruto)

  alocacao_result <- switch(alocacao,
    proporcional = estr_alocacao_proporcional(n, Nk),
    neyman = estr_alocacao_neyman(n, Nk, sigmak = sqrt(sigmak2)),
    custo = estr_alocacao_custo(n, Nk, sigmak = sqrt(sigmak2), ck = ck),
    nenhuma = NULL
  )

  nk_out <- if (!is.null(alocacao_result)) alocacao_result$estimativa$nk else NULL

  new_sampling_result(
    metodo = "Estratificada - tamanho de amostra para a media",
    estimativa = list(n_bruto = n_bruto, n = n, nk = nk_out),
    memoria_calculo = c(
      passo("d* = d^2/z^2 = %s", format(d_star, digits = 6)),
      passo("n = %s", format(n_bruto, digits = 6)),
      passo("n arredondado para cima = %d", n)
    ),
    interpretacao = sprintf("Sao necessarias %d unidades amostrais no total%s.",
                             n, if (!is.null(nk_out)) paste0(", alocadas como: ", paste(nk_out, collapse = ", ")) else ""),
    insight = "Compare o n total obtido aqui com aas_tamanho_media() usando a variancia media ponderada, para visualizar o ganho da estratificacao.",
    referencias = "Cochran (1977), cap. 5.",
    formula_apostila = formula_usada,
    dados_entrada = list(Nk = Nk, sigmak2 = sigmak2, d = d, conf = conf, ck = ck, alocacao = alocacao)
  )
}

#' Sample size for stratified sampling: proportion (proportional / Neyman / cost)
#'
#' Analogous to [estr_tamanho_media()] but for a population proportion,
#' using Eq. (4.12), with per-stratum proportions substituting for
#' variances (`sigmak2 = pk*(1-pk)`).
#'
#' @param Nk Numeric vector with the population size of each stratum.
#' @param pk Numeric vector with the population proportion of each
#'   stratum.
#' @param d Tolerable margin of error.
#' @param conf Confidence level, default `0.95`.
#' @param ck Optional numeric vector with the sampling cost per unit in
#'   each stratum (Eq. 4.13).
#' @param alocacao How to allocate `n` across strata: `"proporcional"`
#'   (default), `"neyman"`, `"custo"` (requires `ck`), or `"nenhuma"`.
#' @param arredondar Either `"cima"` (default) or `"proximo"` -- see
#'   [estr_tamanho_media()].
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the total sample size and,
#'   unless `alocacao = "nenhuma"`, the per-stratum allocation.
#' @examples
#' estr_tamanho_proporcao(Nk = c(1540, 770, 385), pk = c(0.75, 0.50, 0.25), d = 0.05)
#' @export
estr_tamanho_proporcao <- function(Nk, pk, d, conf = 0.95, ck = NULL,
                                    alocacao = c("proporcional", "neyman", "custo", "nenhuma"),
                                    arredondar = c("cima", "proximo"),
                                    ...) {
  alocacao <- match.arg(alocacao)
  arredondar <- match.arg(arredondar)
  sigmak2 <- pk * (1 - pk)
  N <- sum(Nk)
  Wk <- Nk / N
  z <- z_critico(conf)
  d_star <- d^2 / z^2

  if (!is.null(ck)) {
    num <- sum(Nk * pk * (1 - pk) / ck)
    den <- N^2 * d_star + sum(Nk * sigmak2)
    n_bruto <- num / den
    formula_usada <- "(4.13)"
  } else if (alocacao == "neyman") {
    # Tamanho total sob alocacao otima de Neyman, substituindo sigmak por
    # sqrt(pk*(1-pk)) na formula (4.21) -- distinto da formula (4.12),
    # usada para alocacao proporcional.
    sigmak <- sqrt(sigmak2)
    num <- (sum(Nk * sigmak))^2
    den <- N^2 * d_star + sum(Nk * sigmak2)
    n_bruto <- num / den
    formula_usada <- "(4.21) [substituindo sigmak por sqrt(pk(1-pk))]"
  } else {
    num <- sum(Nk^2 * sigmak2 / Wk)
    den <- N^2 * d_star + sum(Nk * sigmak2)
    n_bruto <- num / den
    formula_usada <- "(4.12)"
  }
  n <- if (arredondar == "cima") ceiling(n_bruto) else round(n_bruto)

  alocacao_result <- switch(alocacao,
    proporcional = estr_alocacao_proporcional(n, Nk),
    neyman = estr_alocacao_neyman(n, Nk, pk = pk),
    custo = estr_alocacao_custo(n, Nk, pk = pk, ck = ck),
    nenhuma = NULL
  )
  nk_out <- if (!is.null(alocacao_result)) alocacao_result$estimativa$nk else NULL

  new_sampling_result(
    metodo = "Estratificada - tamanho de amostra para a proporcao",
    estimativa = list(n_bruto = n_bruto, n = n, nk = nk_out),
    memoria_calculo = c(
      passo("d* = d^2/z^2 = %s", format(d_star, digits = 6)),
      passo("n = %s", format(n_bruto, digits = 6))
    ),
    interpretacao = sprintf("Sao necessarias %d unidades amostrais no total%s.",
                             n, if (!is.null(nk_out)) paste0(", alocadas como: ", paste(nk_out, collapse = ", ")) else ""),
    referencias = "Cochran (1977), cap. 5.",
    formula_apostila = formula_usada,
    dados_entrada = list(Nk = Nk, pk = pk, d = d, conf = conf, ck = ck, alocacao = alocacao)
  )
}

#' Adjust a sample size for a minimum expected response rate
#'
#' Inflates a sample size to guarantee a target number of completed
#' responses given an expected response rate, following Eqs. (4.24)-(4.25)
#' of the course notes: `n* = n / taxa_resposta`. This targets *completed
#' responses after losses* and is conceptually distinct from the flat
#' safety-margin buffer of [aas_ajustar_perdas()] (which multiplies by
#' `1 + taxa`, see Example 3.14) -- do not confuse the two.
#'
#' @param n Base sample size computed ignoring non-response.
#' @param taxa_resposta Expected response rate, in `(0, 1]` (e.g. `0.85`
#'   for an expected 85% response rate).
#'
#' @return A `sampling_result` object with the adjusted sample size.
#' @examples
#' amostragem_ajustar_resposta(1525, 0.85)
#' @export
amostragem_ajustar_resposta <- function(n, taxa_resposta) {
  validar_escalar(n, "n", min = 1)
  validar_proporcao(taxa_resposta, "taxa_resposta")
  if (taxa_resposta <= 0) stop("`taxa_resposta` deve ser > 0.", call. = FALSE)
  n_ajustado <- ceiling(n / taxa_resposta)
  new_sampling_result(
    metodo = "Ajuste do tamanho de amostra por taxa de resposta esperada",
    estimativa = list(n_original = n, n_ajustado = n_ajustado),
    memoria_calculo = passo("n* = n / taxa_resposta = %d / %.2f = %d", n, taxa_resposta, n_ajustado),
    interpretacao = sprintf("Para obter %d respostas completas com uma taxa de resposta esperada de %.0f%%, e preciso contatar %d unidades.",
                             n, 100 * taxa_resposta, n_ajustado),
    insight = "Nao confundir com aas_ajustar_perdas(), que aplica um acrescimo percentual simples (multiplica por 1+taxa) em vez de dividir pela taxa de resposta.",
    formula_apostila = c("(4.24)", "(4.25)"),
    dados_entrada = list(n = n, taxa_resposta = taxa_resposta)
  )
}

#' Sample size for stratified sampling: automatic formula selection (umbrella)
#'
#' Convenience "umbrella" function analogous to [aas_tamanho_amostra()]:
#' accepts every input potentially relevant to a stratified sample-size
#' problem and dispatches to [estr_tamanho_media()] or
#' [estr_tamanho_proporcao()] depending on `parametro`, ignoring whichever
#' inputs are not needed.
#'
#' @inheritParams estr_tamanho_media
#' @param parametro Either `"media"` or `"proporcao"`.
#' @param pk Numeric vector with the population proportion of each stratum
#'   (used only when `parametro = "proporcao"`).
#' @param ... Ignored. Any other variables from a broader problem
#'   statement may be passed here without causing an error.
#'
#' @return A `sampling_result` object, as produced by
#'   [estr_tamanho_media()] or [estr_tamanho_proporcao()].
#' @examples
#' estr_tamanho_amostra(parametro = "media", Nk = c(1540, 770, 385),
#'                       sigmak2 = c(7, 9, 11), d = 0.5)
#' @export
estr_tamanho_amostra <- function(parametro = c("media", "proporcao"),
                                  Nk, sigmak2 = NULL, pk = NULL, d, conf = 0.95,
                                  ck = NULL,
                                  alocacao = c("proporcional", "neyman", "custo", "nenhuma"),
                                  arredondar = c("cima", "proximo"),
                                  ...) {
  parametro <- match.arg(parametro)
  alocacao <- match.arg(alocacao)
  arredondar <- match.arg(arredondar)
  if (parametro == "media") {
    estr_tamanho_media(Nk = Nk, sigmak2 = sigmak2, d = d, conf = conf, ck = ck,
                        alocacao = alocacao, arredondar = arredondar)
  } else {
    estr_tamanho_proporcao(Nk = Nk, pk = pk, d = d, conf = conf, ck = ck,
                            alocacao = alocacao, arredondar = arredondar)
  }
}
