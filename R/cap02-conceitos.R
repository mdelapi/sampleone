#' Population total
#'
#' Computes the population total \eqn{X_T = \sum_{i=1}^{N} X_i}
#' (course notes, Eq. 2.2).
#'
#' @param x Numeric vector with the full population values.
#' @param ... Ignored. Present so that extra, unrelated arguments can be
#'   passed without causing an error (see package overview).
#'
#' @return A `sampling_result` object with the population total.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley.
#' @examples
#' pop_total(c(1, 2, 3, 4, 5))
#' @export
pop_total <- function(x, ...) {
  x <- x[!is.na(x)]
  total <- sum(x)
  new_sampling_result(
    metodo = "Total populacional",
    estimativa = list(total = total, N = length(x)),
    memoria_calculo = passo("X_T = soma dos %d valores = %s", length(x), format(total, digits = 6)),
    interpretacao = sprintf("O total da caracteristica de interesse na populacao e %s.", format(total, digits = 6)),
    insight = "Compare com pop_media() * N para verificar consistencia.",
    formula_apostila = "(2.2)",
    dados_entrada = list(x = x)
  )
}

#' Population mean
#'
#' Computes the population mean \eqn{\mu = \sum X_i / N} (Eq. 2.3) and,
#' when requested, the population variance (Eq. 2.5).
#'
#' @param x Numeric vector with the full population values.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the population mean and variance.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley.
#' @examples
#' pop_media(c(44, 45, 48, 42, 46))
#' @export
pop_media <- function(x, ...) {
  x <- x[!is.na(x)]
  N <- length(x)
  mu <- mean(x)
  sigma2 <- sum((x - mu)^2) / N
  new_sampling_result(
    metodo = "Media e variancia populacional",
    estimativa = list(media = mu, N = N),
    variancia = list(sigma2 = sigma2),
    memoria_calculo = c(
      passo("mu = soma(x)/N = %s / %d = %s", format(sum(x), digits = 6), N, format(mu, digits = 6)),
      passo("sigma2 = soma((x-mu)^2)/N = %s", format(sigma2, digits = 6))
    ),
    interpretacao = sprintf("A media populacional e %s, com variancia populacional %s.",
                             format(mu, digits = 6), format(sigma2, digits = 6)),
    insight = "sigma2 aqui usa divisor N (parametro); em amostras, use amostra_variancia() que usa divisor n-1 (estimador nao viesado).",
    formula_apostila = c("(2.3)", "(2.5)"),
    dados_entrada = list(x = x)
  )
}

#' Population proportion
#'
#' Computes the population proportion \eqn{P = \sum X_i / N} for a binary
#' (0/1) characteristic (Eq. 2.4) and its variance \eqn{P(1-P)} (Eq. 2.5.1).
#'
#' @param x Numeric or logical vector coded as 0/1 (or `FALSE`/`TRUE`).
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the population proportion.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley.
#' @examples
#' pop_proporcao(c(1, 0, 0, 1, 1))
#' @export
pop_proporcao <- function(x, ...) {
  x <- as.numeric(x[!is.na(x)])
  if (!all(x %in% c(0, 1))) {
    stop("`x` deve conter apenas 0/1 (ou FALSE/TRUE).", call. = FALSE)
  }
  N <- length(x)
  P <- mean(x)
  var_p <- P * (1 - P)
  new_sampling_result(
    metodo = "Proporcao populacional",
    estimativa = list(P = P, N = N),
    variancia = list(var_P = var_p),
    memoria_calculo = c(
      passo("P = soma(x)/N = %d / %d = %s", sum(x), N, format(P, digits = 6)),
      passo("Var(P) = P(1-P) = %s", format(var_p, digits = 6))
    ),
    interpretacao = sprintf("A proporcao populacional e %s.", format(P, digits = 6)),
    insight = "Var(P) = P(1-P) atinge o maximo em P=0.5; se P estiver perto de 0 ou 1, a variancia e menor.",
    formula_apostila = c("(2.4)", "(2.5.1)"),
    dados_entrada = list(x = x)
  )
}

#' Sample-based estimators (mean, variance, proportion)
#'
#' Computes the sample mean (Eq. 2.8), the unbiased sample variance
#' (Eq. 2.10, divisor n-1), and, when the data are binary, the sample
#' proportion (Eq. 2.9) -- all from a single call. This function is used
#' internally as a building block by most other functions in the package.
#'
#' @param x Numeric vector with the sample data.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the requested sample estimators.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley.
#' @examples
#' amostra_estimadores(c(12, 30, 18))
#' amostra_estimadores(c(1, 0, 1, 1, 0))
#' @export
amostra_estimadores <- function(x, ...) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) stop("`x` precisa ter pelo menos 2 observacoes.", call. = FALSE)
  xbar <- mean(x)
  s2 <- sum((x - xbar)^2) / (n - 1)

  estimativa <- list(media_amostral = xbar, n = n)
  variancia <- list(s2 = s2)
  formulas <- c("(2.8)", "(2.10)")

  binaria <- all(x %in% c(0, 1))
  if (binaria) {
    p_hat <- mean(x)
    estimativa$p_hat <- p_hat
    formulas <- c(formulas, "(2.9)")
  }

  new_sampling_result(
    metodo = "Estimadores amostrais (media, variancia, proporcao)",
    estimativa = estimativa,
    variancia = variancia,
    memoria_calculo = c(
      passo("x_bar = soma(x)/n = %s / %d = %s", format(sum(x), digits = 6), n, format(xbar, digits = 6)),
      passo("s2 = soma((x-x_bar)^2)/(n-1) = %s", format(s2, digits = 6))
    ),
    interpretacao = sprintf("Media amostral = %s; variancia amostral (nao viesada) = %s.",
                             format(xbar, digits = 6), format(s2, digits = 6)),
    insight = "Estes sao os blocos de construcao usados internamente pelas funcoes aas_*(), estr_*() etc.",
    formula_apostila = formulas,
    dados_entrada = list(x = x)
  )
}
