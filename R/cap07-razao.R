#' Compute summary statistics needed by the ratio estimator
#'
#' Internal helper that accepts either raw vectors `x`/`y` or
#' pre-aggregated sums, and returns a common set of summary statistics
#' (`n`, `sum_x`, `sum_y`, `sum_x2`, `sum_y2`, `sum_xy`, `r`) used by the
#' `razao_*()` functions. Supporting both calling styles matters because
#' some textbook problems (e.g. Example 7.5 of the course notes) give
#' only pre-computed sums, not the raw sample data.
#'
#' @param x,y Optional raw numeric vectors.
#' @param n,sum_x,sum_y,sum_x2,sum_y2,sum_xy Optional pre-aggregated
#'   summary statistics (used only if `x`/`y` are not supplied).
#' @return A list with `n`, `sum_x`, `sum_y`, `sum_x2`, `sum_y2`,
#'   `sum_xy`, `r`, and `soma_dyx2` (= `sum((y - r*x)^2)`, via the
#'   computational Eq. 7.40).
#' @keywords internal
.razao_resumo <- function(x = NULL, y = NULL, n = NULL, sum_x = NULL, sum_y = NULL,
                           sum_x2 = NULL, sum_y2 = NULL, sum_xy = NULL) {
  if (!is.null(x) && !is.null(y)) {
    stopifnot(length(x) == length(y))
    n <- length(x)
    sum_x <- sum(x); sum_y <- sum(y)
    sum_x2 <- sum(x^2); sum_y2 <- sum(y^2); sum_xy <- sum(x * y)
  } else {
    if (is.null(n) || is.null(sum_x) || is.null(sum_y) || is.null(sum_x2) ||
        is.null(sum_y2) || is.null(sum_xy)) {
      stop("Informe `x` e `y` (dados brutos) ou todos os somatorios: n, sum_x, sum_y, sum_x2, sum_y2, sum_xy.",
           call. = FALSE)
    }
  }
  r <- sum_y / sum_x
  # Eq. (7.40): soma((y-rx)^2) = sum_y2 + r^2*sum_x2 - 2*r*sum_xy
  soma_dyx2 <- sum_y2 + r^2 * sum_x2 - 2 * r * sum_xy
  list(n = n, sum_x = sum_x, sum_y = sum_y, sum_x2 = sum_x2, sum_y2 = sum_y2,
       sum_xy = sum_xy, r = r, soma_dyx2 = soma_dyx2)
}

#' Ratio estimator of the population ratio R
#'
#' Estimates the population ratio `R = Y/X` (Eq. 7.27-7.30 of the course
#' notes) and its variance (Eq. 7.31), from either raw sample data
#' (`x`, `y`) or pre-aggregated sums -- see [.razao_resumo()].
#'
#' @param x,y Optional raw numeric vectors (sample data).
#' @param n,sum_x,sum_y,sum_x2,sum_y2,sum_xy Optional pre-aggregated
#'   summary statistics, used when raw data are not available (e.g. a
#'   problem statement that only gives totals).
#' @param N Population size.
#' @param X Population total of the auxiliary variable (used to derive
#'   `X_barra = X/N`, required by Eq. 7.31).
#' @param conf Confidence level, default `0.95`.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the estimated ratio, its
#'   variance, and confidence interval.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley, cap. 6.
#' @examples
#' x <- c(1, 30, 44, 20, 0, 10, 15, 5, 2, 50, 35, 25)
#' y <- c(2, 35, 50, 27, 1, 15, 17, 7, 0, 53, 35, 30)
#' razao_estima_R(x = x, y = y, N = 1500, X = 15000)
#' @export
razao_estima_R <- function(x = NULL, y = NULL, n = NULL, sum_x = NULL, sum_y = NULL,
                            sum_x2 = NULL, sum_y2 = NULL, sum_xy = NULL,
                            N, X, conf = 0.95, ...) {
  resumo <- .razao_resumo(x, y, n, sum_x, sum_y, sum_x2, sum_y2, sum_xy)
  X_barra <- X / N
  var_r <- ((N - resumo$n) / (resumo$n * N)) * (1 / X_barra^2) * (1 / (resumo$n - 1)) * resumo$soma_dyx2
  ep <- sqrt(var_r)
  z <- z_critico(conf)
  ic <- c(resumo$r - z * ep, resumo$r + z * ep)

  new_sampling_result(
    metodo = "Estimador razao - razao populacional R",
    estimativa = list(R = resumo$r, n = resumo$n),
    variancia = list(var_R = var_r, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("r = soma(y)/soma(x) = %s", format(resumo$r, digits = 6)),
      passo("Xbar = X/N = %s", format(X_barra, digits = 6)),
      passo("Var(r) = %s", format(var_r, digits = 6))
    ),
    interpretacao = sprintf("A razao populacional estimada e %s, com IC %.0f%% = [%s ; %s].",
                             format(resumo$r, digits = 4), 100 * conf, format(ic[1], digits = 4), format(ic[2], digits = 4)),
    referencias = "Cochran (1977), cap. 6.",
    formula_apostila = c("(7.27)", "(7.28)", "(7.29)", "(7.30)", "(7.31)", "(7.32)"),
    dados_entrada = list(N = N, X = X, conf = conf)
  )
}

#' Ratio estimator of the population total
#'
#' Estimates the population total using the ratio estimator
#' (Eq. 7.33-7.35 of the course notes).
#'
#' @inheritParams razao_estima_R
#'
#' @return A `sampling_result` object with the estimated total, its
#'   variance, and confidence interval.
#' @examples
#' x <- c(1, 30, 44, 20, 0, 10, 15, 5, 2, 50, 35, 25)
#' y <- c(2, 35, 50, 27, 1, 15, 17, 7, 0, 53, 35, 30)
#' razao_estima_total(x = x, y = y, N = 1500, X = 15000)
#' @export
razao_estima_total <- function(x = NULL, y = NULL, n = NULL, sum_x = NULL, sum_y = NULL,
                                sum_x2 = NULL, sum_y2 = NULL, sum_xy = NULL,
                                N, X, conf = 0.95, ...) {
  resumo <- .razao_resumo(x, y, n, sum_x, sum_y, sum_x2, sum_y2, sum_xy)
  X_barra <- X / N
  var_r <- ((N - resumo$n) / (resumo$n * N)) * (1 / X_barra^2) * (1 / (resumo$n - 1)) * resumo$soma_dyx2

  tau_Y <- resumo$r * X
  var_tau <- X^2 * var_r
  ep <- sqrt(var_tau)
  z <- z_critico(conf)
  ic <- c(tau_Y - z * ep, tau_Y + z * ep)

  new_sampling_result(
    metodo = "Estimador razao - total populacional",
    estimativa = list(total = tau_Y, r = resumo$r, n = resumo$n),
    variancia = list(var_total = var_tau, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("tau_Y = r*X = %s*%s = %s", format(resumo$r, digits = 6), format(X), format(tau_Y, digits = 6)),
      passo("Var(tau_Y) = X^2*Var(r) = %s", format(var_tau, digits = 6))
    ),
    interpretacao = sprintf("O total estimado (razao) e %s, com IC %.0f%% = [%s ; %s].",
                             format(tau_Y, digits = 6), 100 * conf, format(ic[1], digits = 6), format(ic[2], digits = 6)),
    referencias = "Cochran (1977), cap. 6.",
    formula_apostila = c("(7.33)", "(7.34)", "(7.35)"),
    dados_entrada = list(N = N, X = X, conf = conf)
  )
}

#' Ratio estimator of the population mean
#'
#' Estimates the population mean using the ratio estimator
#' (Eq. 7.36-7.38 of the course notes). Unlike [razao_estima_R()], this
#' does not require knowing `X` (the population total of the auxiliary
#' variable) -- only `N`.
#'
#' @inheritParams razao_estima_R
#' @param X Optional population total of the auxiliary variable; not
#'   needed to compute the mean itself (only `N` is required), but
#'   accepted here so that a full problem statement (which usually
#'   supplies `X`) can be passed without error. Ignored if `NULL`.
#'
#' @return A `sampling_result` object with the estimated mean, its
#'   variance, and confidence interval.
#' @examples
#' x <- c(1, 30, 44, 20, 0, 10, 15, 5, 2, 50, 35, 25)
#' y <- c(2, 35, 50, 27, 1, 15, 17, 7, 0, 53, 35, 30)
#' razao_estima_media(x = x, y = y, N = 1500)
#' @export
razao_estima_media <- function(x = NULL, y = NULL, n = NULL, sum_x = NULL, sum_y = NULL,
                                sum_x2 = NULL, sum_y2 = NULL, sum_xy = NULL,
                                N, X = NULL, conf = 0.95, ...) {
  resumo <- .razao_resumo(x, y, n, sum_x, sum_y, sum_x2, sum_y2, sum_xy)
  mu_rY <- resumo$r * (if (!is.null(X)) X / N else resumo$sum_x / resumo$n)
  # Var(mu_rY) = (N-n)/(nN) * 1/(n-1) * soma((y-rx)^2)  [Xbar^2 se cancela algebricamente]
  var_muY <- ((N - resumo$n) / (resumo$n * N)) * (1 / (resumo$n - 1)) * resumo$soma_dyx2
  ep <- sqrt(var_muY)
  z <- z_critico(conf)
  ic <- c(mu_rY - z * ep, mu_rY + z * ep)

  new_sampling_result(
    metodo = "Estimador razao - media populacional",
    estimativa = list(media = mu_rY, r = resumo$r, n = resumo$n),
    variancia = list(var_media = var_muY, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("mu_rY = r * Xbar = %s", format(mu_rY, digits = 6)),
      passo("Var(mu_rY) = (N-n)/(nN) * 1/(n-1) * soma((y-rx)^2) = %s", format(var_muY, digits = 6))
    ),
    interpretacao = sprintf("A media estimada (razao) e %s, com IC %.0f%% = [%s ; %s].",
                             format(mu_rY, digits = 4), 100 * conf, format(ic[1], digits = 4), format(ic[2], digits = 4)),
    insight = "Compare com reg_estima_media() para os mesmos dados -- se reg_teste_origem() recomendar razao, este estimador tende a ter variancia menor.",
    referencias = "Cochran (1977), cap. 6.",
    formula_apostila = c("(7.36)", "(7.37)", "(7.38)"),
    dados_entrada = list(N = N, X = X, conf = conf)
  )
}
