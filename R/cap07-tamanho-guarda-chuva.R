#' Pilot-sample variance estimate for the ratio estimator
#'
#' Computes `s_r^2`, the pilot-sample estimate of the population variance
#' of the ratio estimator (Eq. 7.42 of the course notes), used as an input
#' to [razao_tamanho_R()], [razao_tamanho_total()], and
#' [razao_tamanho_media()] when no prior estimate of the variance is
#' available.
#'
#' @inheritParams razao_estima_R
#' @param N,X Ignored (kept for signature symmetry with [razao_estima_R()]
#'   so a full problem statement can be passed without error) -- `s_r^2`
#'   itself does not require them.
#'
#' @return A `sampling_result` object with `s_r^2`.
#' @examples
#' x <- c(1, 30, 44, 20, 0, 10, 15, 5, 2, 50, 35, 25)
#' y <- c(2, 35, 50, 27, 1, 15, 17, 7, 0, 53, 35, 30)
#' razao_variancia_piloto(x = x, y = y)
#' @export
razao_variancia_piloto <- function(x = NULL, y = NULL, n = NULL, sum_x = NULL, sum_y = NULL,
                                    sum_x2 = NULL, sum_y2 = NULL, sum_xy = NULL,
                                    N = NULL, X = NULL, ...) {
  resumo <- .razao_resumo(x, y, n, sum_x, sum_y, sum_x2, sum_y2, sum_xy)
  sr2 <- resumo$soma_dyx2 / (resumo$n - 1)

  new_sampling_result(
    metodo = "Estimador razao - variancia piloto (amostra preliminar)",
    estimativa = list(sr2 = sr2, r = resumo$r, n_prime = resumo$n),
    memoria_calculo = passo("sr2 = soma((y-rx)^2)/(n'-1) = %s/%d = %s",
                             format(resumo$soma_dyx2, digits = 6), resumo$n - 1, format(sr2, digits = 6)),
    interpretacao = sprintf("A variancia piloto estimada e sr2=%s, a partir de uma amostra preliminar de %d observacoes.",
                             format(sr2, digits = 6), resumo$n),
    insight = "Use este valor em razao_tamanho_R(), razao_tamanho_total() ou razao_tamanho_media() para planejar o tamanho da amostra definitiva.",
    formula_apostila = "(7.42)",
    dados_entrada = list()
  )
}

#' Sample size to estimate the population ratio R
#'
#' Computes the sample size needed to estimate the population ratio `R`
#' with a given margin of error (Eq. 7.41 of the course notes).
#'
#' @param sr2 Variance of the ratio estimator (population value, or a
#'   pilot-sample estimate from [razao_variancia_piloto()]).
#' @param N Population size.
#' @param X_barra Population mean of the auxiliary variable (`X/N`).
#' @param d Tolerable margin of error.
#' @param conf Confidence level, default `0.95`.
#' @param arredondar Either `"cima"` (default) or `"proximo"` -- see
#'   [aas_tamanho_media()].
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the required sample size.
#' @examples
#' razao_tamanho_R(sr2 = 7.7853, N = 1500, X_barra = 10, d = 0.05)
#' @export
razao_tamanho_R <- function(sr2, N, X_barra, d, conf = 0.95, arredondar = c("cima", "proximo"), ...) {
  arredondar <- match.arg(arredondar)
  z <- z_critico(conf)
  d_star <- (d^2 * X_barra^2) / z^2
  n_bruto <- (N * sr2) / (N * d_star + sr2)
  n <- if (arredondar == "cima") ceiling(n_bruto) else round(n_bruto)

  new_sampling_result(
    metodo = "Estimador razao - tamanho de amostra para R",
    estimativa = list(n_bruto = n_bruto, n = n),
    memoria_calculo = c(
      passo("d* = d^2*Xbar^2/z^2 = %s", format(d_star, digits = 6)),
      passo("nr = N*sr2 / (N*d* + sr2) = %s", format(n_bruto, digits = 6))
    ),
    interpretacao = sprintf("Sao necessarias aproximadamente %d observacoes para estimar R.", n),
    referencias = "Cochran (1977), cap. 6.",
    formula_apostila = "(7.41)",
    dados_entrada = list(sr2 = sr2, N = N, X_barra = X_barra, d = d, conf = conf)
  )
}

#' Sample size to estimate the population total via the ratio estimator
#'
#' Analogous to [razao_tamanho_R()] but for the population total
#' (Eq. 7.43 of the course notes).
#'
#' @inheritParams razao_tamanho_R
#'
#' @return A `sampling_result` object with the required sample size.
#' @examples
#' razao_tamanho_total(sr2 = 7.7853, N = 1500, d = 500)
#' @export
razao_tamanho_total <- function(sr2, N, d, conf = 0.95, arredondar = c("cima", "proximo"), ...) {
  arredondar <- match.arg(arredondar)
  z <- z_critico(conf)
  d_star <- d^2 / (z^2 * N^2)
  n_bruto <- (N * sr2) / (N * d_star + sr2)
  n <- if (arredondar == "cima") ceiling(n_bruto) else round(n_bruto)

  new_sampling_result(
    metodo = "Estimador razao - tamanho de amostra para o total",
    estimativa = list(n_bruto = n_bruto, n = n),
    memoria_calculo = c(
      passo("d* = d^2/(z^2*N^2) = %s", format(d_star, digits = 6)),
      passo("nrT = N*sr2 / (N*d* + sr2) = %s", format(n_bruto, digits = 6))
    ),
    interpretacao = sprintf("Sao necessarias aproximadamente %d observacoes para estimar o total.", n),
    referencias = "Cochran (1977), cap. 6.",
    formula_apostila = "(7.43)",
    dados_entrada = list(sr2 = sr2, N = N, d = d, conf = conf)
  )
}

#' Sample size to estimate the population mean via the ratio estimator
#'
#' Analogous to [razao_tamanho_R()] but for the population mean
#' (Eq. 7.44 of the course notes). Unlike [razao_tamanho_R()], this does
#' not require `X_barra`.
#'
#' @param sr2 Variance of the ratio estimator (population value, or a
#'   pilot-sample estimate from [razao_variancia_piloto()]).
#' @param N Population size.
#' @param d Tolerable margin of error.
#' @param conf Confidence level, default `0.95`.
#' @param arredondar Either `"cima"` (default) or `"proximo"`.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the required sample size.
#' @examples
#' razao_tamanho_media(sr2 = 2.2462, N = 1000, d = 0.75)
#' @export
razao_tamanho_media <- function(sr2, N, d, conf = 0.95, arredondar = c("cima", "proximo"), ...) {
  arredondar <- match.arg(arredondar)
  z <- z_critico(conf)
  d_star <- d^2 / z^2
  n_bruto <- (N * sr2) / (N * d_star + sr2)
  n <- if (arredondar == "cima") ceiling(n_bruto) else round(n_bruto)

  new_sampling_result(
    metodo = "Estimador razao - tamanho de amostra para a media",
    estimativa = list(n_bruto = n_bruto, n = n),
    memoria_calculo = c(
      passo("d* = d^2/z^2 = %s", format(d_star, digits = 6)),
      passo("nrY = N*sr2 / (N*d* + sr2) = %s", format(n_bruto, digits = 6))
    ),
    interpretacao = sprintf("Sao necessarias aproximadamente %d observacoes para estimar a media.", n),
    referencias = "Cochran (1977), cap. 6.",
    formula_apostila = "(7.44)",
    dados_entrada = list(sr2 = sr2, N = N, d = d, conf = conf)
  )
}

#' Ratio vs. regression: full side-by-side comparison (umbrella function)
#'
#' Convenience "umbrella" function that fits the regression model, tests
#' whether the line passes through the origin, and computes *both* the
#' ratio and the regression estimators of the population mean side by
#' side, clearly flagging which one the origin test recommends --
#' following the full workflow of Examples 7.1-7.2 of the course notes.
#' This lets students and instructors compare both estimators even when
#' only one is formally "correct" for the data at hand.
#'
#' @inheritParams reg_teste_origem
#' @param X_barra Known population mean of the auxiliary variable
#'   (required for the regression estimator; not required for the ratio
#'   estimator, whose mean form only needs `N`).
#' @param N Population size.
#' @param ... Ignored. Any other variables from a broader problem
#'   statement may be passed here without causing an error.
#'
#' @return A named list with elements `teste` (from [reg_teste_origem()]),
#'   `razao` (from [razao_estima_media()]), and `regressao` (from
#'   [reg_estima_media()]), plus a `recomendado` character scalar.
#' @examples
#' x <- c(1, 30, 44, 20, 0, 10, 15, 5, 2, 50, 35, 25)
#' y <- c(2, 35, 50, 27, 1, 15, 17, 7, 0, 53, 35, 30)
#' razao_vs_regressao_decidir(x, y, X_barra = 10, N = 1500)
#' @export
razao_vs_regressao_decidir <- function(x, y, X_barra, N, conf = 0.95, ...) {
  teste <- reg_teste_origem(x, y, conf = conf)
  razao <- razao_estima_media(x = x, y = y, N = N, conf = conf)
  regressao <- reg_estima_media(x, y, X_barra = X_barra, N = N, conf = conf)

  cat("<sampleone> Comparacao razao vs. regressao\n")
  cat(strrep("-", 60), "\n", sep = "")
  cat(sprintf("Recomendado pelo teste de origem: %s\n\n", toupper(teste$estimativa$recomendado)))
  cat("[RAZAO]     media =", format(razao$estimativa$media, digits = 6),
      " IC =", format(razao$intervalo_confianca[1], digits = 4), "-", format(razao$intervalo_confianca[2], digits = 4), "\n")
  cat("[REGRESSAO] media =", format(regressao$estimativa$media, digits = 6),
      " IC =", format(regressao$intervalo_confianca[1], digits = 4), "-", format(regressao$intervalo_confianca[2], digits = 4), "\n")

  invisible(list(teste = teste, razao = razao, regressao = regressao,
                  recomendado = teste$estimativa$recomendado))
}
