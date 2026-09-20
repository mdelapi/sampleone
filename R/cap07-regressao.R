#' Fit a simple linear regression (ratio/regression estimator groundwork)
#'
#' Fits the simple linear regression of `y` on `x` by least squares
#' (Eqs. 7.9-7.10 of the course notes), and computes the residual
#' variance, the standard error of the intercept, and the t-statistic
#' used to test whether the line passes through the origin
#' (Eqs. 7.18-7.20) -- the basis for deciding between the ratio and the
#' regression estimator (see [reg_teste_origem()]).
#'
#' @param x Numeric vector with the auxiliary variable.
#' @param y Numeric vector with the variable of interest.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the fitted coefficients,
#'   residual variance, and the intercept's standard error and
#'   t-statistic.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley, cap. 7.
#' @examples
#' x <- c(39, 43, 21, 64, 57, 47, 28, 75, 34, 52)
#' y <- c(65, 78, 52, 82, 92, 89, 73, 98, 56, 75)
#' reg_ajustar(x, y)
#' @export
reg_ajustar <- function(x, y, ...) {
  n <- length(x)
  stopifnot(length(y) == n, n >= 3)
  x_bar <- mean(x); y_bar <- mean(y)
  Sxx <- sum((x - x_bar)^2)
  Sxy <- sum((x - x_bar) * (y - y_bar))
  a1 <- Sxy / Sxx
  a0 <- y_bar - a1 * x_bar

  y_hat <- a0 + a1 * x
  residuos <- y - y_hat
  SQE <- sum(residuos^2)
  sigma2_hat <- SQE / (n - 2)
  ep_a0 <- sqrt(sigma2_hat) * sqrt(1 / n + x_bar^2 / Sxx)
  t_a0 <- a0 / ep_a0

  new_sampling_result(
    metodo = "Regressao linear simples - ajuste dos parametros",
    estimativa = list(a0 = a0, a1 = a1, n = n),
    variancia = list(sigma2_hat = sigma2_hat, SQE = SQE, ep_a0 = ep_a0),
    memoria_calculo = c(
      passo("a1 = Sxy/Sxx = %s", format(a1, digits = 6)),
      passo("a0 = ybar - a1*xbar = %s", format(a0, digits = 6)),
      passo("sigma2_hat = SQE/(n-2) = %s", format(sigma2_hat, digits = 6)),
      passo("EP(a0) = %s", format(ep_a0, digits = 6)),
      passo("t(a0) = a0/EP(a0) = %s", format(t_a0, digits = 6))
    ),
    interpretacao = sprintf("Modelo ajustado: y_hat = %s + %s*x.", format(a0, digits = 4), format(a1, digits = 4)),
    insight = "Use reg_teste_origem() para decidir, com base neste ajuste, entre o estimador razao e o estimador regressao.",
    referencias = "Cochran (1977), cap. 7.",
    formula_apostila = c("(7.9)", "(7.10)", "(7.18)", "(7.19)", "(7.20)"),
    dados_entrada = list(x = x, y = y, y_hat = y_hat, residuos = residuos, t_a0 = t_a0)
  )
}

#' Decide between the ratio and regression estimators
#'
#' Tests whether the regression line of `y` on `x` passes through the
#' origin (`H0: a0 = 0`), using both the t-statistic (Eq. 7.20) and the
#' confidence interval for `a0` (Eq. 7.21), and recommends the ratio
#' estimator if the line passes through the origin (fails to reject
#' `H0`) or the regression estimator otherwise -- following the logic of
#' Examples 7.1-7.2 of the course notes.
#'
#' @inheritParams reg_ajustar
#' @param conf Confidence level, default `0.95`.
#'
#' @return A `sampling_result` object stating the recommended estimator,
#'   the test statistic, the confidence interval for `a0`, and both
#'   estimators computed side by side for comparison.
#' @examples
#' x <- c(1, 30, 44, 20, 0, 10, 15, 5, 2, 50, 35, 25)
#' y <- c(2, 35, 50, 27, 1, 15, 17, 7, 0, 53, 35, 30)
#' reg_teste_origem(x, y)
#' @export
reg_teste_origem <- function(x, y, conf = 0.95, ...) {
  ajuste <- reg_ajustar(x, y)
  n <- ajuste$estimativa$n
  a0 <- ajuste$estimativa$a0
  ep_a0 <- ajuste$variancia$ep_a0
  t_a0 <- ajuste$dados_entrada$t_a0
  t_critico <- stats::qt(1 - (1 - conf) / 2, df = n - 2)
  ic_a0 <- c(a0 - t_critico * ep_a0, a0 + t_critico * ep_a0)

  passa_origem <- abs(t_a0) < t_critico
  recomendado <- if (passa_origem) "razao" else "regressao"

  new_sampling_result(
    metodo = "Decisao: estimador razao vs. regressao",
    estimativa = list(a0 = a0, t_a0 = t_a0, t_critico = t_critico, recomendado = recomendado),
    intervalo_confianca = ic_a0,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("t(a0) = %s ; t_critico(%.3f, n-2=%d) = %s", format(t_a0, digits = 6), conf, n - 2, format(t_critico, digits = 6)),
      passo("IC de a0: [%s ; %s]", format(ic_a0[1], digits = 6), format(ic_a0[2], digits = 6)),
      if (passa_origem) "|t(a0)| < t_critico (ou IC contem 0) -> nao rejeitar H0 -> reta passa pela origem -> usar estimador RAZAO"
      else "|t(a0)| >= t_critico (ou IC nao contem 0) -> rejeitar H0 -> reta NAO passa pela origem -> usar estimador REGRESSAO"
    ),
    interpretacao = sprintf("O estimador recomendado e o de %s.", toupper(recomendado)),
    insight = "Ambos os estimadores (razao e regressao) podem ser calculados e comparados lado a lado, mesmo quando um deles nao e o recomendado -- ver razao_vs_regressao_decidir().",
    referencias = "Cochran (1977), cap. 6-7.",
    formula_apostila = c("(7.20)", "(7.21)"),
    dados_entrada = list(x = x, y = y, conf = conf)
  )
}

#' Regression estimator of the population mean
#'
#' Estimates the population mean using the regression estimator
#' (Eq. 7.22-7.25 of the course notes), which uses a known population
#' mean `X_barra` of an auxiliary variable correlated with `y`.
#'
#' @inheritParams reg_ajustar
#' @param X_barra Known population mean of the auxiliary variable.
#' @param N Optional population size, used for the finite-population
#'   correction `f = n/N`. If `NULL`, `f = 0` is assumed.
#' @param conf Confidence level, default `0.95`.
#'
#' @return A `sampling_result` object with the regression-estimated mean,
#'   its variance, and confidence interval.
#' @examples
#' x <- c(39, 43, 21, 64, 57, 47, 28, 75, 34, 52)
#' y <- c(65, 78, 52, 82, 92, 89, 73, 98, 56, 75)
#' reg_estima_media(x, y, X_barra = 52, N = 486)
#' @export
reg_estima_media <- function(x, y, X_barra, N = NULL, conf = 0.95, ...) {
  n <- length(x)
  x_bar <- mean(x); y_bar <- mean(y)
  Sxx <- sum((x - x_bar)^2)
  Sxy <- sum((x - x_bar) * (y - y_bar))
  Syy <- sum((y - y_bar)^2)
  a1 <- Sxy / Sxx

  mu_YL <- y_bar + a1 * (X_barra - x_bar)
  f <- if (is.null(N)) 0 else n / N
  var_muYL <- ((1 - f) / n) * (1 / (n - 2)) * (Syy - a1^2 * Sxx)
  ep <- sqrt(var_muYL)
  z <- z_critico(conf)
  ic <- c(mu_YL - z * ep, mu_YL + z * ep)

  new_sampling_result(
    metodo = "Estimador regressao da media populacional",
    estimativa = list(media = mu_YL, n = n, a1 = a1),
    variancia = list(var_media = var_muYL, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("mu_YL = ybar + a1*(Xbar - xbar) = %s", format(mu_YL, digits = 6)),
      passo("Var(mu_YL) = %s", format(var_muYL, digits = 6))
    ),
    interpretacao = sprintf("A media estimada (regressao) e %s, com IC %.0f%% = [%s ; %s].",
                             format(mu_YL, digits = 4), 100 * conf, format(ic[1], digits = 4), format(ic[2], digits = 4)),
    insight = "Use reg_teste_origem() antes desta funcao para confirmar que o estimador regressao (em vez do estimador razao) e o mais adequado.",
    referencias = "Cochran (1977), cap. 7.",
    formula_apostila = c("(7.22)", "(7.23)", "(7.24)", "(7.25)"),
    dados_entrada = list(x = x, y = y, X_barra = X_barra, N = N, conf = conf)
  )
}
