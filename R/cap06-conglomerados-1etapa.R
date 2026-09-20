#' Estimate the population mean under one-stage cluster sampling
#'
#' Estimates the population mean from a sample of whole clusters, all of
#' whose elements were observed (one-stage cluster sampling), following
#' Eqs. (6.1)-(6.4) of the course notes.
#'
#' @param yi Numeric vector with the cluster totals (`y_i`) for each
#'   sampled cluster.
#' @param Mi Numeric vector with the number of elements in each sampled
#'   cluster.
#' @param Nc Total number of clusters in the population.
#' @param M Optional total number of elements in the population (used to
#'   compute the population average cluster size `M_barra = M/Nc`, as
#'   required by Eq. 6.2). When `M` is unknown, `M_barra` falls back to
#'   the sample average of `Mi`, which is only an approximation and is
#'   flagged as such in the result.
#' @param conf Confidence level, default `0.95`.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the estimated mean, variance,
#'   and confidence interval.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley, cap. 9.
#' @examples
#' # Exemplo 6.1 (turmas da UFMT)
#' cong1_estima_media(
#'   yi = c(1209, 921, 1116, 1433, 1037, 2112, 1737, 2206, 2112, 1870),
#'   Mi = c(20, 16, 17, 21, 16, 25, 21, 26, 25, 23),
#'   Nc = 373, M = 8018
#' )
#' @export
cong1_estima_media <- function(yi, Mi, Nc, M = NULL, conf = 0.95, ...) {
  nc <- length(yi)
  stopifnot(length(Mi) == nc)
  M_aproximado <- is.null(M)
  M_barra <- if (M_aproximado) mean(Mi) else M / Nc

  y_c <- sum(yi) / sum(Mi)
  Sc2 <- sum((yi - y_c * Mi)^2) / (nc - 1)
  var_yc <- ((Nc - nc) / (Nc * nc * M_barra^2)) * Sc2
  ep <- sqrt(var_yc)
  z <- z_critico(conf)
  ic <- c(y_c - z * ep, y_c + z * ep)

  new_sampling_result(
    metodo = "Conglomerados (1 etapa) - estimacao da media",
    estimativa = list(media = y_c, nc = nc, Nc = Nc, M_barra = M_barra),
    variancia = list(Sc2 = Sc2, var_media = var_yc, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("y_c = soma(yi)/soma(Mi) = %s", format(y_c, digits = 6)),
      passo("Sc2 = soma((yi - y_c*Mi)^2)/(nc-1) = %s", format(Sc2, digits = 6)),
      if (M_aproximado) "AVISO: M (total populacional de elementos) nao informado; M_barra aproximado pela media amostral de Mi (ver Eq. 6.2, que usa M_barra = M/Nc)." else
        passo("M_barra = M/Nc = %s/%d = %s", format(M), Nc, format(M_barra, digits = 6)),
      passo("var(y_c) = [(Nc-nc)/(Nc*nc*Mbar^2)]*Sc2 = %s", format(var_yc, digits = 6))
    ),
    interpretacao = sprintf("A media estimada e %s, com IC %.0f%% = [%s ; %s].",
                             format(y_c, digits = 6), 100 * conf, format(ic[1], digits = 6), format(ic[2], digits = 6)),
    insight = "Conglomerados costuma ter variancia maior que AAS/estratificada para o mesmo n de elementos, mas custo de coleta muito menor -- compare via amostragem_comparar_planos().",
    referencias = "Cochran (1977), cap. 9.",
    formula_apostila = c("(6.1)", "(6.2)", "(6.3)", "(6.4)"),
    dados_entrada = list(yi = yi, Mi = Mi, Nc = Nc, M = M, conf = conf)
  )
}

#' Estimate the population proportion under one-stage cluster sampling
#'
#' Analogous to [cong1_estima_media()] but for a population proportion,
#' following Eqs. (6.7)-(6.10) of the course notes.
#'
#' @param yi Numeric vector with the number of "successes" (elements with
#'   the characteristic of interest) in each sampled cluster.
#' @param M Optional total number of elements in the population (see
#'   [cong1_estima_media()] for details on `M_barra = M/Nc`).
#' @inheritParams cong1_estima_media
#'
#' @return A `sampling_result` object with the estimated proportion,
#'   variance, and confidence interval.
#' @examples
#' # Exemplo 6.3 (proporcao de alunos com peso acima da mediana)
#' cong1_estima_proporcao(
#'   yi = c(7, 6, 5, 6, 5, 7, 5, 6, 6, 6),
#'   Mi = c(20, 16, 17, 21, 16, 25, 21, 26, 25, 23),
#'   Nc = 373, M = 8018
#' )
#' @export
cong1_estima_proporcao <- function(yi, Mi, Nc, M = NULL, conf = 0.95, ...) {
  ncp <- length(yi)
  stopifnot(length(Mi) == ncp)
  M_aproximado <- is.null(M)
  M_barra <- if (M_aproximado) mean(Mi) else M / Nc

  p_c <- sum(yi) / sum(Mi)
  Scp2 <- sum((yi - p_c * Mi)^2) / (ncp - 1)
  var_pc <- ((Nc - ncp) / (Nc * ncp * M_barra^2)) * Scp2
  ep <- sqrt(var_pc)
  z <- z_critico(conf)
  ic <- c(max(0, p_c - z * ep), min(1, p_c + z * ep))

  new_sampling_result(
    metodo = "Conglomerados (1 etapa) - estimacao da proporcao",
    estimativa = list(p_c = p_c, ncp = ncp, Nc = Nc, M_barra = M_barra),
    variancia = list(Scp2 = Scp2, var_p = var_pc, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("p_c = soma(yi)/soma(Mi) = %s", format(p_c, digits = 6)),
      passo("Scp2 = %s", format(Scp2, digits = 6)),
      passo("var(p_c) = %s", format(var_pc, digits = 6))
    ),
    interpretacao = sprintf("A proporcao estimada e %s, com IC %.0f%% = [%s ; %s].",
                             format(p_c, digits = 4), 100 * conf, format(ic[1], digits = 4), format(ic[2], digits = 4)),
    referencias = "Cochran (1977), cap. 9.",
    formula_apostila = c("(6.7)", "(6.8)", "(6.9)", "(6.10)"),
    dados_entrada = list(yi = yi, Mi = Mi, Nc = Nc, M = M, conf = conf)
  )
}

#' Sample size (number of clusters) to estimate a mean (one-stage cluster sampling)
#'
#' Computes the number of clusters needed to estimate a population mean
#' with a given margin of error under one-stage cluster sampling
#' (Eq. 6.5 of the course notes).
#'
#' @param Nc Total number of clusters in the population.
#' @param sigmac2 Between-cluster population variance (or its estimate,
#'   from a pilot study).
#' @param M_barra Average number of elements per cluster.
#' @param d Tolerable margin of error (in the original units of the
#'   variable, not per-cluster).
#' @param conf Confidence level, default `0.95`.
#' @param arredondar Either `"cima"` (default) or `"proximo"` -- see
#'   [aas_tamanho_media()]. Example 6.1(c) of the course notes uses
#'   `"proximo"` (41.87 rounds to 42, same result either way here).
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the required number of
#'   clusters.
#' @examples
#' cong1_tamanho_media(Nc = 373, sigmac2 = 51054.3854, M_barra = 8018/373, d = 3)
#' @export
cong1_tamanho_media <- function(Nc, sigmac2, M_barra, d, conf = 0.95,
                                 arredondar = c("cima", "proximo"), ...) {
  arredondar <- match.arg(arredondar)
  z <- z_critico(conf)
  d_star <- (d^2 * M_barra^2) / z^2
  n_bruto <- (Nc * sigmac2) / (Nc * d_star + sigmac2)
  nc <- if (arredondar == "cima") ceiling(n_bruto) else round(n_bruto)

  new_sampling_result(
    metodo = "Conglomerados (1 etapa) - tamanho de amostra (numero de conglomerados) para a media",
    estimativa = list(n_bruto = n_bruto, nc = nc),
    memoria_calculo = c(
      passo("d* = d^2*Mbar^2/z^2 = %s", format(d_star, digits = 6)),
      passo("nc = Nc*sigmac2 / (Nc*d* + sigmac2) = %s", format(n_bruto, digits = 6))
    ),
    interpretacao = sprintf("Sao necessarios aproximadamente %d conglomerados.", nc),
    referencias = "Cochran (1977), cap. 9.",
    formula_apostila = "(6.5)",
    dados_entrada = list(Nc = Nc, sigmac2 = sigmac2, M_barra = M_barra, d = d, conf = conf)
  )
}

#' Sample size (number of clusters) to estimate a proportion (one-stage cluster sampling)
#'
#' Analogous to [cong1_tamanho_media()] but for a population proportion
#' (Eq. 6.11 of the course notes).
#'
#' @inheritParams cong1_tamanho_media
#' @param sigmacp2 Between-cluster population variance for the proportion
#'   (or its estimate).
#' @return A `sampling_result` object with the required number of
#'   clusters.
#' @examples
#' cong1_tamanho_proporcao(Nc = 373, sigmacp2 = 0.91785840, M_barra = 8018/373, d = 0.013)
#' @export
cong1_tamanho_proporcao <- function(Nc, sigmacp2, M_barra, d, conf = 0.95,
                                     arredondar = c("cima", "proximo"), ...) {
  arredondar <- match.arg(arredondar)
  z <- z_critico(conf)
  d_star <- (d^2 * M_barra^2) / z^2
  n_bruto <- (Nc * sigmacp2) / (Nc * d_star + sigmacp2)
  ncp <- if (arredondar == "cima") ceiling(n_bruto) else round(n_bruto)

  new_sampling_result(
    metodo = "Conglomerados (1 etapa) - tamanho de amostra (numero de conglomerados) para a proporcao",
    estimativa = list(n_bruto = n_bruto, ncp = ncp),
    memoria_calculo = c(
      passo("d* = d^2*Mbar^2/z^2 = %s", format(d_star, digits = 6)),
      passo("ncp = Nc*sigmacp2 / (Nc*d* + sigmacp2) = %s", format(n_bruto, digits = 6))
    ),
    interpretacao = sprintf("Sao necessarios aproximadamente %d conglomerados.", ncp),
    referencias = "Cochran (1977), cap. 9.",
    formula_apostila = "(6.11)",
    dados_entrada = list(Nc = Nc, sigmacp2 = sigmacp2, M_barra = M_barra, d = d, conf = conf)
  )
}

#' Adjust a cluster/element sample size for design effect and response rate
#'
#' Alias for [amostragem_ajustar_deff_resposta()], kept for continuity with
#' the cluster-sampling chapter, since that is where the course notes
#' first introduce Eq. (6.6). The underlying formula is general-purpose
#' (not specific to clusters) -- see [amostragem_ajustar_deff_resposta()]
#' for the full documentation and for use with non-cluster problems (AAS,
#' stratified, etc.).
#'
#' @inheritParams amostragem_ajustar_deff_resposta
#'
#' @return A `sampling_result` object with the adjusted sample size.
#' @references Espinosa et al. (2019), as cited in the course notes, Eq. (6.6).
#' @examples
#' cong1_ajustar_deff(n_prime = 518, deff = 1.5, taxa_resposta = 0.85)
#' @export
cong1_ajustar_deff <- function(n_prime, deff = 1.5, taxa_resposta = 0.85) {
  amostragem_ajustar_deff_resposta(n_prime, deff = deff, taxa_resposta = taxa_resposta)
}
