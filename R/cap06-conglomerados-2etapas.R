#' Estimate the population mean under two-stage cluster sampling
#'
#' Estimates the population mean from a two-stage cluster sample (a sample
#' of clusters -- UPA -- followed by a sample of elements within each
#' sampled cluster -- USA), when the total population size `M` is known,
#' following Eqs. (6.12)-(6.18) of the course notes.
#'
#' @param dados A `data.frame` (or `list` of data frames / a list with
#'   one element per cluster) with, for every sampled element, at least
#'   the columns `conglomerado` (cluster id), `Mi` (cluster population
#'   size), and `y` (the measured value for that element). Alternatively,
#'   supply `Mi`, `mi`, `yi_bar`, and `si2` directly (pre-summarised per
#'   cluster) -- see Details.
#' @param Mi Optional numeric vector with the population size of each
#'   sampled cluster (used instead of `dados` when the data are already
#'   summarised per cluster).
#' @param mi Optional numeric vector with the number of sampled elements
#'   in each cluster.
#' @param yi_bar Optional numeric vector with the sample mean of each
#'   cluster.
#' @param si2 Optional numeric vector with the sample variance within
#'   each cluster (Eq. 6.15).
#' @param Nc Total number of clusters in the population.
#' @param M Total number of elements in the population.
#' @param conf Confidence level, default `0.95`.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @details
#' Two calling styles are supported: (1) pass raw per-element data via
#' `dados` (a data.frame with one row per sampled element), or (2) pass
#' already-summarised per-cluster vectors `Mi`, `mi`, `yi_bar`, `si2`
#' directly. The second style is convenient when a problem statement (as
#' in a textbook exercise) already gives per-cluster summaries instead of
#' raw data -- any of `dados`/`Mi`/`mi`/`yi_bar`/`si2` not needed for the
#' chosen style are simply ignored.
#'
#' @return A `sampling_result` object with the estimated mean, its
#'   two-part variance decomposition, and confidence interval.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley, cap. 10.
#' @examples
#' Mi <- c(50, 65, 45, 48, 52, 58, 42, 66, 40, 56)
#' mi <- c(10, 13, 9, 10, 10, 12, 8, 13, 8, 11)
#' yi_bar <- c(5.4, 4.0, 5.6667, 4.8, 4.3, 3.8333, 5.0, 3.8462, 4.875, 5.0)
#' si2 <- c(11.3778, 10.6667, 16.75, 13.2889, 11.1222, 14.8788, 5.1429,
#'          4.3077, 6.125, 11.8)
#' cong2_estima_media(Mi = Mi, mi = mi, yi_bar = yi_bar, si2 = si2,
#'                     Nc = 90, M = 4500)
#' @export
cong2_estima_media <- function(dados = NULL, Mi = NULL, mi = NULL, yi_bar = NULL,
                                si2 = NULL, Nc, M, conf = 0.95, ...) {
  if (!is.null(dados)) {
    agg <- .cong2_agregar(dados)
    Mi <- agg$Mi; mi <- agg$mi; yi_bar <- agg$yi_bar; si2 <- agg$si2
  }
  nc <- length(Mi)
  stopifnot(length(mi) == nc, length(yi_bar) == nc, length(si2) == nc)
  M_barra <- M / Nc

  mu_c2e <- (Nc / M) * sum(Mi * yi_bar) / nc
  Sb2 <- sum((Mi * yi_bar - M_barra * mu_c2e)^2) / (nc - 1)

  parte1 <- ((Nc - nc) / Nc) * (1 / (nc * M_barra^2)) * Sb2
  parte2 <- sum(Mi^2 * ((Mi - mi) / Mi) * (si2 / mi)) / (nc * Nc * M_barra^2)
  V <- parte1 + parte2

  ep <- sqrt(V)
  z <- z_critico(conf)
  ic <- c(mu_c2e - z * ep, mu_c2e + z * ep)

  new_sampling_result(
    metodo = "Conglomerados (2 etapas) - estimacao da media (M conhecido)",
    estimativa = list(media = mu_c2e, nc = nc, Nc = Nc, M = M),
    variancia = list(Sb2 = Sb2, parte1_entre = parte1, parte2_dentro = parte2,
                      var_media = V, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("mu_c2e = (Nc/M)*soma(Mi*yi_bar)/nc = %s", format(mu_c2e, digits = 6)),
      passo("Sb2 = soma((Mi*yi_bar - Mbar*mu_c2e)^2)/(nc-1) = %s", format(Sb2, digits = 6)),
      passo("Parte 1 (entre conglomerados) = %s", format(parte1, digits = 6)),
      passo("Parte 2 (dentro dos conglomerados) = %s", format(parte2, digits = 6)),
      passo("V(mu_c2e) = Parte1 + Parte2 = %s", format(V, digits = 6))
    ),
    interpretacao = sprintf("A media estimada e %s, com IC %.0f%% = [%s ; %s]. A variancia se decompoe em %.1f%% de variabilidade entre conglomerados e %.1f%% dentro dos conglomerados.",
                             format(mu_c2e, digits = 4), 100 * conf, format(ic[1], digits = 4), format(ic[2], digits = 4),
                             100 * parte1 / V, 100 * parte2 / V),
    insight = "Se a Parte 2 (dentro) for pequena em relacao a Parte 1 (entre), amostrar mais elementos por conglomerado tras pouco ganho -- melhor amostrar mais conglomerados. Se M for desconhecido, use cong2_estima_razao_media().",
    referencias = "Cochran (1977), cap. 10.",
    formula_apostila = c("(6.12)", "(6.13)", "(6.14)", "(6.15)", "(6.18)"),
    dados_entrada = list(Mi = Mi, mi = mi, yi_bar = yi_bar, si2 = si2, Nc = Nc, M = M, conf = conf)
  )
}

#' Estimate the population mean via ratio estimator (two-stage cluster sampling, M unknown)
#'
#' Analogous to [cong2_estima_media()] but for the case where the total
#' population size `M` is unknown, using the ratio estimator of
#' Eqs. (6.19)-(6.23) of the course notes, with `m_barra` (the sample
#' average cluster size) substituted for `M_barra = M/Nc`.
#'
#' @inheritParams cong2_estima_media
#' @param M Ignored for this estimator (kept for signature symmetry with
#'   [cong2_estima_media()] so extra arguments from a broader problem can
#'   be passed without error); `m_barra` is estimated from `Mi` instead.
#'
#' @return A `sampling_result` object with the ratio-estimated mean, its
#'   variance, and confidence interval.
#' @examples
#' Mi <- c(50, 65, 45, 48, 52, 58, 42, 66, 40, 56)
#' mi <- c(10, 13, 9, 10, 10, 12, 8, 13, 8, 11)
#' yi_bar <- c(5.4, 4.0, 5.6667, 4.8, 4.3, 3.8333, 5.0, 3.8462, 4.875, 5.0)
#' si2 <- c(11.3778, 10.6667, 16.75, 13.2889, 11.1222, 14.8788, 5.1429,
#'          4.3077, 6.125, 11.8)
#' cong2_estima_razao_media(Mi = Mi, mi = mi, yi_bar = yi_bar, si2 = si2, Nc = 90)
#' @export
cong2_estima_razao_media <- function(dados = NULL, Mi = NULL, mi = NULL, yi_bar = NULL,
                                      si2 = NULL, Nc, M = NULL, conf = 0.95, ...) {
  if (!is.null(dados)) {
    agg <- .cong2_agregar(dados)
    Mi <- agg$Mi; mi <- agg$mi; yi_bar <- agg$yi_bar; si2 <- agg$si2
  }
  nc <- length(Mi)
  stopifnot(length(mi) == nc, length(yi_bar) == nc, length(si2) == nc)
  m_barra <- mean(Mi)

  mu_rc2e <- sum(Mi * yi_bar) / sum(Mi)
  Sbr2 <- sum(Mi^2 * (yi_bar - mu_rc2e)^2) / (nc - 1)

  parte1 <- ((Nc - nc) / Nc) * (1 / (nc * m_barra^2)) * Sbr2
  parte2 <- sum(Mi^2 * ((Mi - mi) / Mi) * (si2 / mi)) / (nc * Nc * m_barra^2)
  V <- parte1 + parte2

  ep <- sqrt(V)
  z <- z_critico(conf)
  ic <- c(mu_rc2e - z * ep, mu_rc2e + z * ep)

  new_sampling_result(
    metodo = "Conglomerados (2 etapas) - estimador razao da media (M desconhecido)",
    estimativa = list(media = mu_rc2e, nc = nc, Nc = Nc, m_barra = m_barra),
    variancia = list(Sbr2 = Sbr2, parte1_entre = parte1, parte2_dentro = parte2,
                      var_media = V, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("m_barra = media(Mi) = %s", format(m_barra, digits = 6)),
      passo("mu_rc2e = soma(Mi*yi_bar)/soma(Mi) = %s", format(mu_rc2e, digits = 6)),
      passo("Sbr2 = soma(Mi^2*(yi_bar-mu_rc2e)^2)/(nc-1) = %s", format(Sbr2, digits = 6)),
      passo("V(mu_rc2e) = Parte1 + Parte2 = %s", format(V, digits = 6))
    ),
    interpretacao = sprintf("A media estimada (estimador razao) e %s, com IC %.0f%% = [%s ; %s].",
                             format(mu_rc2e, digits = 4), 100 * conf, format(ic[1], digits = 4), format(ic[2], digits = 4)),
    insight = "O estimador razao e enviesado em pequenas amostras, mas o vies costuma ser desprezivel para nc>=20 (Cochran, 1977). Se M for conhecido, prefira cong2_estima_media(), que costuma ter variancia menor.",
    referencias = "Cochran (1977), cap. 10.",
    formula_apostila = c("(6.19)", "(6.20)", "(6.21)", "(6.22)", "(6.23)"),
    dados_entrada = list(Mi = Mi, mi = mi, yi_bar = yi_bar, si2 = si2, Nc = Nc, conf = conf)
  )
}

#' Estimate the population proportion under two-stage cluster sampling
#'
#' Analogous to [cong2_estima_media()] but for a population proportion,
#' following Eqs. (6.24)-(6.29) of the course notes. Note that, unlike
#' [cong2_estima_media()], the proportion estimator is inherently a
#' ratio-type estimator (structurally analogous to
#' [cong2_estima_razao_media()]) and therefore always uses the *sample*
#' average cluster size `m_barra = mean(Mi)` in its variance formula, even
#' when the population totals `Nc`/`M` are known -- this is exactly how
#' Example 6.6 of the course notes computes it (using `m_barra = 52.2`
#' rather than `M/Nc = 50`, even though both `M` and `Nc` are known in
#' that example).
#'
#' @param Mi Numeric vector with the population size of each sampled
#'   cluster.
#' @param mi Numeric vector with the number of sampled elements in each
#'   cluster.
#' @param pi_hat Numeric vector with the sample proportion of each
#'   cluster (Eq. 6.24).
#' @param Nc Total number of clusters in the population.
#' @param M Ignored (kept for signature symmetry with
#'   [cong2_estima_media()] and so that extra arguments from a broader
#'   problem statement can be passed without error) -- see Details.
#' @param conf Confidence level, default `0.95`.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the estimated proportion, its
#'   variance, and confidence interval.
#' @examples
#' Mi <- c(50, 65, 45, 48, 52, 58, 42, 66, 40, 56)
#' mi <- c(10, 13, 9, 10, 10, 12, 8, 13, 8, 11)
#' pi_hat <- c(0.4, 0.3077, 0.3333, 0.3, 0.3, 0.25, 0.375, 0.3077, 0.375, 0.3636)
#' cong2_estima_proporcao(Mi = Mi, mi = mi, pi_hat = pi_hat, Nc = 90)
#' @export
cong2_estima_proporcao <- function(Mi, mi, pi_hat, Nc, M = NULL, conf = 0.95, ...) {
  nc <- length(Mi)
  stopifnot(length(mi) == nc, length(pi_hat) == nc)
  m_barra <- mean(Mi)

  p_c2e <- sum(Mi * pi_hat) / sum(Mi)
  Sbp2 <- sum((Mi * pi_hat - p_c2e * Mi)^2) / (nc - 1)
  # nota: (Mi*pi_hat - p_c2e*Mi) = Mi*(pi_hat - p_c2e), forma equivalente a Mi^2*(pi_hat-p_c2e)^2 somado
  sip2 <- pi_hat * (1 - pi_hat) * mi / (mi - 1) # variancia amostral de uma proporcao por conglomerado

  parte1 <- ((Nc - nc) / Nc) * (1 / (nc * m_barra^2)) * Sbp2
  parte2 <- sum(Mi^2 * ((Mi - mi) / Mi) * (sip2 / mi)) / (nc * Nc * m_barra^2)
  V <- parte1 + parte2

  ep <- sqrt(V)
  z <- z_critico(conf)
  ic <- c(max(0, p_c2e - z * ep), min(1, p_c2e + z * ep))

  new_sampling_result(
    metodo = "Conglomerados (2 etapas) - estimacao da proporcao",
    estimativa = list(p_c2e = p_c2e, nc = nc, Nc = Nc, m_barra = m_barra),
    variancia = list(Sbp2 = Sbp2, parte1_entre = parte1, parte2_dentro = parte2,
                      var_p = V, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("m_barra = media(Mi) = %s (nao M/Nc -- ver @details)", format(m_barra, digits = 6)),
      passo("p_c2e = soma(Mi*pi_hat)/soma(Mi) = %s", format(p_c2e, digits = 6)),
      passo("Sbp2 = %s", format(Sbp2, digits = 6)),
      passo("V(p_c2e) = %s", format(V, digits = 6))
    ),
    interpretacao = sprintf("A proporcao estimada e %s, com IC %.0f%% = [%s ; %s].",
                             format(p_c2e, digits = 4), 100 * conf, format(ic[1], digits = 4), format(ic[2], digits = 4)),
    referencias = "Cochran (1977), cap. 10.",
    formula_apostila = c("(6.24)", "(6.25)", "(6.26)", "(6.27)", "(6.28)", "(6.29)"),
    dados_entrada = list(Mi = Mi, mi = mi, pi_hat = pi_hat, Nc = Nc, conf = conf)
  )
}

#' Aggregate raw two-stage cluster data into per-cluster summaries
#' @param dados A data.frame with columns `conglomerado`, `Mi`, `y`.
#' @return A list with `Mi`, `mi`, `yi_bar`, `si2` (one value per cluster,
#'   ordered by first appearance of `conglomerado`).
#' @keywords internal
.cong2_agregar <- function(dados) {
  stopifnot(all(c("conglomerado", "Mi", "y") %in% names(dados)))
  ids <- unique(dados$conglomerado)
  Mi <- vapply(ids, function(id) dados$Mi[dados$conglomerado == id][1], numeric(1))
  mi <- vapply(ids, function(id) sum(dados$conglomerado == id), numeric(1))
  yi_bar <- vapply(ids, function(id) mean(dados$y[dados$conglomerado == id]), numeric(1))
  si2 <- vapply(ids, function(id) stats::var(dados$y[dados$conglomerado == id]), numeric(1))
  list(Mi = Mi, mi = mi, yi_bar = yi_bar, si2 = si2)
}

#' Umbrella estimator for two-stage cluster sampling
#'
#' Convenience "umbrella" function analogous to [aas_tamanho_amostra()]:
#' dispatches automatically to [cong2_estima_media()] (when `M` is
#' supplied) or [cong2_estima_razao_media()] (when `M` is `NULL`),
#' ignoring whichever inputs are not needed.
#'
#' @inheritParams cong2_estima_media
#' @param ... Ignored. Any other variables from a broader problem
#'   statement may be passed here without causing an error.
#'
#' @return A `sampling_result` object, as produced by
#'   [cong2_estima_media()] or [cong2_estima_razao_media()].
#' @examples
#' Mi <- c(50, 65, 45, 48, 52, 58, 42, 66, 40, 56)
#' mi <- c(10, 13, 9, 10, 10, 12, 8, 13, 8, 11)
#' yi_bar <- c(5.4, 4.0, 5.6667, 4.8, 4.3, 3.8333, 5.0, 3.8462, 4.875, 5.0)
#' si2 <- c(11.3778, 10.6667, 16.75, 13.2889, 11.1222, 14.8788, 5.1429,
#'          4.3077, 6.125, 11.8)
#' cong_estima(Mi = Mi, mi = mi, yi_bar = yi_bar, si2 = si2, Nc = 90) # M ausente -> razao
#' @export
cong_estima <- function(dados = NULL, Mi = NULL, mi = NULL, yi_bar = NULL,
                         si2 = NULL, Nc, M = NULL, conf = 0.95, ...) {
  if (is.null(M)) {
    cong2_estima_razao_media(dados = dados, Mi = Mi, mi = mi, yi_bar = yi_bar,
                              si2 = si2, Nc = Nc, conf = conf)
  } else {
    cong2_estima_media(dados = dados, Mi = Mi, mi = mi, yi_bar = yi_bar,
                        si2 = si2, Nc = Nc, M = M, conf = conf)
  }
}
