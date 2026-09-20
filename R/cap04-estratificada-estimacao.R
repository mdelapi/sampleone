#' Estimate the population mean under stratified sampling
#'
#' Estimates the population mean from data collected under stratified
#' random sampling, given per-stratum sample means and sizes (Eqs. 4.1-4.4
#' of the course notes).
#'
#' @param Nk Numeric vector with the population size of each stratum.
#' @param xk_bar Numeric vector with the sample mean of each stratum.
#' @param sk2 Numeric vector with the sample variance of each stratum.
#' @param nk Numeric vector with the sample size of each stratum.
#' @param conf Confidence level, default `0.95`.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the estimated stratified mean,
#'   its variance, coefficient of variation, and confidence interval.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley, cap. 5.
#' @examples
#' estr_estima_media(Nk = c(1540, 770, 385), xk_bar = c(120, 118, 125),
#'                    sk2 = c(7, 9, 11), nk = c(69, 34, 17))
#' @export
estr_estima_media <- function(Nk, xk_bar, sk2, nk, conf = 0.95, ...) {
  L <- length(Nk)
  stopifnot(length(xk_bar) == L, length(sk2) == L, length(nk) == L)
  N <- sum(Nk)
  Wk <- Nk / N
  fk <- nk / Nk

  y_es <- sum(Wk * xk_bar)
  var_yes <- sum(Wk^2 * (1 - fk) * sk2 / nk)
  ep <- sqrt(var_yes)
  cv <- ep / y_es
  z <- z_critico(conf)
  ic <- c(y_es - z * ep, y_es + z * ep)

  new_sampling_result(
    metodo = "Estratificada - estimacao da media",
    estimativa = list(media = y_es, L = L, N = N),
    variancia = list(var_media = var_yes, erro_padrao = ep, cv = cv),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("Wk = Nk/N = %s", paste(round(Wk, 4), collapse = ", ")),
      passo("y_es = soma(Wk*xk_bar) = %s", format(y_es, digits = 6)),
      passo("var(y_es) = soma(Wk^2*(1-fk)*sk2/nk) = %s", format(var_yes, digits = 6))
    ),
    interpretacao = sprintf("A media estratificada estimada e %s, com IC %.0f%% = [%s ; %s].",
                             format(y_es, digits = 6), 100 * conf, format(ic[1], digits = 6), format(ic[2], digits = 6)),
    insight = "Compare a variancia obtida aqui com a de uma AAS de mesmo tamanho total (amostragem_comparar_planos()) para visualizar o ganho de precisao da estratificacao.",
    referencias = "Cochran (1977), cap. 5.",
    formula_apostila = c("(4.1)", "(4.2)", "(4.2.1)", "(4.2.2)", "(4.4)"),
    dados_entrada = list(Nk = Nk, xk_bar = xk_bar, sk2 = sk2, nk = nk, conf = conf)
  )
}

#' Estimate the population proportion under stratified sampling
#'
#' Estimates a population proportion from data collected under stratified
#' random sampling, given per-stratum sample proportions and sizes
#' (Eqs. 4.9-4.11 of the course notes).
#'
#' @param Nk Numeric vector with the population size of each stratum.
#' @param pk Numeric vector with the sample proportion of each stratum.
#' @param nk Numeric vector with the sample size of each stratum.
#' @param conf Confidence level, default `0.95`.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the estimated stratified
#'   proportion, its variance, and confidence interval.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley, cap. 5.
#' @examples
#' estr_estima_proporcao(Nk = c(1540, 770, 385), pk = c(0.75, 0.50, 0.25),
#'                        nk = c(161, 81, 40))
#' @export
estr_estima_proporcao <- function(Nk, pk, nk, conf = 0.95, ...) {
  L <- length(Nk)
  stopifnot(length(pk) == L, length(nk) == L)
  N <- sum(Nk)
  Wk <- Nk / N
  fk <- nk / Nk

  p_es <- sum(Wk * pk)
  var_pes <- sum(Wk^2 * (1 - fk) * (pk * (1 - pk)) / (nk - 1))
  ep <- sqrt(var_pes)
  z <- z_critico(conf)
  ic <- c(max(0, p_es - z * ep), min(1, p_es + z * ep))

  new_sampling_result(
    metodo = "Estratificada - estimacao da proporcao",
    estimativa = list(p_es = p_es, L = L, N = N),
    variancia = list(var_p = var_pes, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("Wk = Nk/N = %s", paste(round(Wk, 4), collapse = ", ")),
      passo("p_es = soma(Wk*pk) = %s", format(p_es, digits = 6)),
      passo("var(p_es) = %s", format(var_pes, digits = 6))
    ),
    interpretacao = sprintf("A proporcao estratificada estimada e %s, com IC %.0f%% = [%s ; %s].",
                             format(p_es, digits = 4), 100 * conf, format(ic[1], digits = 4), format(ic[2], digits = 4)),
    referencias = "Cochran (1977), cap. 5.",
    formula_apostila = c("(4.9)", "(4.10)", "(4.16)", "(4.11)"),
    dados_entrada = list(Nk = Nk, pk = pk, nk = nk, conf = conf)
  )
}

#' Proportional allocation of a stratified sample
#'
#' Allocates a total sample size across strata proportionally to stratum
#' size, `nk = n * Wk` (Eq. 4.6, or Eq. 4.14-4.15 for proportions).
#'
#' @param n Total sample size to allocate.
#' @param Nk Numeric vector with the population size of each stratum.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the per-stratum sample sizes.
#' @examples
#' estr_alocacao_proporcional(n = 120, Nk = c(1540, 770, 385))
#' @export
estr_alocacao_proporcional <- function(n, Nk, ...) {
  N <- sum(Nk)
  Wk <- Nk / N
  nk <- n * Wk
  nk_arred <- round(nk)
  # ajusta para garantir que soma(nk_arred) == n
  diff <- n - sum(nk_arred)
  if (diff != 0) {
    idx <- order(-abs(nk - nk_arred))[seq_len(abs(diff))]
    nk_arred[idx] <- nk_arred[idx] + sign(diff)
  }

  new_sampling_result(
    metodo = "Estratificada - alocacao proporcional",
    estimativa = list(nk = nk_arred, n = sum(nk_arred), L = length(Nk)),
    memoria_calculo = passo("nk = n*Wk = %d * (%s) = %s",
                             n, paste(round(Wk, 4), collapse = ", "),
                             paste(nk_arred, collapse = ", ")),
    interpretacao = sprintf("Alocacao proporcional para %d estratos, somando %d unidades.", length(Nk), sum(nk_arred)),
    formula_apostila = c("(4.6)", "(4.14)", "(4.15)"),
    dados_entrada = list(n = n, Nk = Nk)
  )
}

#' Neyman (optimum) allocation of a stratified sample
#'
#' Allocates a total sample size across strata to minimise variance for a
#' fixed total sample size, assuming equal per-unit cost across strata
#' (Eqs. 4.18-4.20 of the course notes). Works for both mean (`sigmak`
#' known) and proportion (`pk` known, substituting
#' `sigmak = sqrt(pk*(1-pk))`) problems.
#'
#' @param n Total sample size to allocate.
#' @param Nk Numeric vector with the population size of each stratum.
#' @param sigmak Numeric vector with the population standard deviation of
#'   each stratum (for a mean problem). Ignored if `pk` is supplied.
#' @param pk Numeric vector with the population proportion of each
#'   stratum (for a proportion problem); if supplied, `sigmak` is derived
#'   as `sqrt(pk*(1-pk))` and any `sigmak` argument is ignored.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the per-stratum sample sizes
#'   under Neyman allocation.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley, cap. 5.
#' @examples
#' estr_alocacao_neyman(n = 261, Nk = c(1540, 770, 385), sigmak = c(2.6, 3, 3.3))
#' @export
estr_alocacao_neyman <- function(n, Nk, sigmak = NULL, pk = NULL, ...) {
  if (!is.null(pk)) sigmak <- sqrt(pk * (1 - pk))
  if (is.null(sigmak)) stop("Informe `sigmak` ou `pk`.", call. = FALSE)
  L <- length(Nk)
  stopifnot(length(sigmak) == L)

  peso <- Nk * sigmak
  nk <- n * peso / sum(peso)
  nk_arred <- round(nk)
  diff <- n - sum(nk_arred)
  if (diff != 0) {
    idx <- order(-abs(nk - nk_arred))[seq_len(abs(diff))]
    nk_arred[idx] <- nk_arred[idx] + sign(diff)
  }

  new_sampling_result(
    metodo = "Estratificada - alocacao otima de Neyman",
    estimativa = list(nk = nk_arred, n = sum(nk_arred), L = L),
    memoria_calculo = passo("nk = n*(Nk*sigmak)/soma(Nk*sigmak) = %s", paste(nk_arred, collapse = ", ")),
    interpretacao = sprintf("Alocacao de Neyman para %d estratos, somando %d unidades -- estratos com maior Nk*sigmak recebem mais amostra.", L, sum(nk_arred)),
    insight = "A alocacao de Neyman produz variancia menor ou igual a alocacao proporcional para o mesmo n total -- compare com estr_alocacao_proporcional().",
    referencias = "Cochran (1977), cap. 5.",
    formula_apostila = c("(4.18)", "(4.19)", "(4.20)"),
    dados_entrada = list(n = n, Nk = Nk, sigmak = sigmak)
  )
}

#' Cost-based optimum allocation of a stratified sample
#'
#' Allocates strata sample sizes to minimise variance subject to a fixed
#' total budget, when per-unit sampling costs differ across strata
#' (Eqs. 4.7-4.8 for means, Eq. 4.13 for proportions).
#'
#' @inheritParams estr_alocacao_neyman
#' @param ck Numeric vector with the sampling cost per unit in each
#'   stratum.
#'
#' @return A `sampling_result` object with the per-stratum sample sizes
#'   under cost-based optimum allocation.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley, cap. 5.
#' @examples
#' estr_alocacao_custo(n = 491, Nk = c(4000, 3000, 2000), sigmak = c(3, 3, 2),
#'                      ck = c(10, 10, 20))
#' @export
estr_alocacao_custo <- function(n, Nk, sigmak = NULL, pk = NULL, ck, ...) {
  if (!is.null(pk)) sigmak <- sqrt(pk * (1 - pk))
  if (is.null(sigmak)) stop("Informe `sigmak` ou `pk`.", call. = FALSE)
  L <- length(Nk)
  stopifnot(length(sigmak) == L, length(ck) == L)

  peso <- (Nk * sigmak) / sqrt(ck)
  nk <- n * peso / sum(peso)
  nk_arred <- round(nk)
  diff <- n - sum(nk_arred)
  if (diff != 0) {
    idx <- order(-abs(nk - nk_arred))[seq_len(abs(diff))]
    nk_arred[idx] <- nk_arred[idx] + sign(diff)
  }

  new_sampling_result(
    metodo = "Estratificada - alocacao otima com custo",
    estimativa = list(nk = nk_arred, n = sum(nk_arred), L = L),
    memoria_calculo = passo("nk proporcional a (Nk*sigmak)/sqrt(ck) = %s", paste(nk_arred, collapse = ", ")),
    interpretacao = sprintf("Alocacao com custo para %d estratos: estratos mais baratos e/ou mais variaveis recebem mais amostra.", L),
    referencias = "Cochran (1977), cap. 5.",
    formula_apostila = c("(4.7)", "(4.8)", "(4.13)"),
    dados_entrada = list(n = n, Nk = Nk, sigmak = sigmak, ck = ck)
  )
}
