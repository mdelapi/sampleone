#' Sample size for AAS: automatic formula selection (umbrella function)
#'
#' Convenience "umbrella" function that accepts every input that might be
#' relevant to determine an AAS sample size (population variance or
#' standard deviation, expected proportion, population size, expected
#' losses) and automatically dispatches to [aas_tamanho_media()] or
#' [aas_tamanho_proporcao()] depending on `parametro`, silently ignoring
#' whichever inputs are not needed for the chosen calculation.
#'
#' @param parametro Either `"media"` or `"proporcao"`: which population
#'   parameter the sample size should be computed for.
#' @param N Optional population size.
#' @param sigma2 Population variance (used only when `parametro = "media"`).
#' @param sigma Population standard deviation (alternative to `sigma2`,
#'   used only when `parametro = "media"`).
#' @param p Expected proportion (used only when `parametro = "proporcao"`).
#' @param d Tolerable margin of error.
#' @param conf Confidence level, default `0.95`.
#' @param perda Optional expected non-response/loss rate in `[0, 1)`.
#' @param arredondar Either `"cima"` (default) or `"proximo"` -- see
#'   [aas_tamanho_media()].
#' @param ... Ignored. Any other variables from a broader problem statement
#'   (e.g. sample data, stratum information) may be passed here without
#'   causing an error.
#'
#' @return A `sampling_result` object, as produced by
#'   [aas_tamanho_media()] or [aas_tamanho_proporcao()].
#' @examples
#' aas_tamanho_amostra(parametro = "media", sigma = 7, d = 2)
#' aas_tamanho_amostra(parametro = "proporcao", p = 0.6, d = 0.03, N = 5000)
#' @export
aas_tamanho_amostra <- function(parametro = c("media", "proporcao"),
                                 N = NULL, sigma2 = NULL, sigma = NULL,
                                 p = NULL, d, conf = 0.95, perda = 0,
                                 arredondar = c("cima", "proximo"), ...) {
  parametro <- match.arg(parametro)
  arredondar <- match.arg(arredondar)
  if (parametro == "media") {
    aas_tamanho_media(sigma2 = sigma2, sigma = sigma, d = d, conf = conf,
                       N = N, perda = perda, arredondar = arredondar)
  } else {
    aas_tamanho_proporcao(p = p, d = d, conf = conf, N = N, perda = perda,
                           arredondar = arredondar)
  }
}
