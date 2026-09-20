#' Estimate the population mean under simple random sampling (AAS)
#'
#' Estimates the population mean from a simple random sample, with or
#' without replacement, together with its variance and a confidence
#' interval. Corresponds to Eqs. (3.4)-(3.7) (with replacement, AASc) and
#' (3.25)-(3.28.1) (without replacement, AASs) of the course notes.
#'
#' @param x Numeric vector with the sample data.
#' @param N Optional population size. If `NULL` or `Inf`, the population is
#'   treated as unknown/very large (the finite-population correction
#'   `1 - n/N` is dropped, i.e. `f = 0`), which also makes the "com"
#'   ("with replacement") and "sem" ("without replacement") results
#'   coincide.
#' @param reposicao Either `"sem"` (without replacement, AASs -- the
#'   default and the usual case in practice) or `"com"` (with replacement,
#'   AASc).
#' @param conf Confidence level, default `0.95`.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the estimated mean, its
#'   variance, coefficient of variation, and confidence interval.
#' @references
#' Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley.
#' Bolfarine, H.; Bussab, W.O. (2005). *Elementos de Amostragem*. Edgard Blucher.
#' @examples
#' aas_estima_media(c(12, 30, 18), N = 3, reposicao = "com")
#' @export
aas_estima_media <- function(x, N = NULL, reposicao = c("sem", "com"), conf = 0.95, ...) {
  reposicao <- match.arg(reposicao)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) stop("`x` precisa ter pelo menos 2 observacoes.", call. = FALSE)
  if (is.null(N)) N <- Inf
  validar_escalar(N, "N", min = n, permitir_inf = TRUE)

  xbar <- mean(x)
  s2 <- sum((x - xbar)^2) / (n - 1)
  f <- if (is.finite(N)) n / N else 0

  if (reposicao == "com") {
    var_xbar <- s2 / n
    formula_var <- "(3.7)"
  } else {
    var_xbar <- (1 - f) * s2 / n
    formula_var <- "(3.28)"
  }

  ep <- sqrt(var_xbar)
  z <- z_critico(conf)
  ic <- c(xbar - z * ep, xbar + z * ep)
  cv <- ep / xbar

  new_sampling_result(
    metodo = sprintf("AAS %s reposicao - estimacao da media", reposicao),
    estimativa = list(media = xbar, n = n),
    variancia = list(s2 = s2, var_media = var_xbar, erro_padrao = ep, cv = cv),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("x_bar = %s (n = %d)", format(xbar, digits = 6), n),
      passo("s2 = %s", format(s2, digits = 6)),
      if (reposicao == "sem") passo("f = n/N = %s", format(f, digits = 6)) else NULL,
      passo("Var(x_bar) = %s", format(var_xbar, digits = 6)),
      passo("IC %.0f%%: %s +/- %s * %s = [%s ; %s]",
            100 * conf, format(xbar, digits = 6), format(z, digits = 4),
            format(ep, digits = 6), format(ic[1], digits = 6), format(ic[2], digits = 6))
    ),
    interpretacao = sprintf("A media estimada e %s, com %d%% de confianca de que o valor populacional esta entre %s e %s.",
                             format(xbar, digits = 6), 100 * conf, format(ic[1], digits = 6), format(ic[2], digits = 6)),
    insight = if (reposicao == "sem" && is.finite(N)) {
      "Compare com reposicao = 'com' nos mesmos dados: a amostragem sem reposicao produz sempre variancia menor ou igual (ver aas_comparar_reposicao())."
    } else {
      "Se a populacao for muito grande ou desconhecida, os planos com e sem reposicao coincidem (f ~ 0)."
    },
    referencias = "Cochran (1977), cap. 2-4.",
    formula_apostila = c("(3.4)-(3.7)", "(3.25)-(3.28.1)", formula_var),
    dados_entrada = list(x = x, N = N, reposicao = reposicao, conf = conf)
  )
}

#' Estimate the population total under simple random sampling (AAS)
#'
#' Estimates the population total \eqn{\hat{\tau} = N \bar{x}} from a
#' simple random sample without replacement (Eq. 3.28.2-3.28.3 of the
#' course notes). For the "with replacement" plan, the same expressions
#' are used with `f = 0`.
#'
#' @inheritParams aas_estima_media
#' @param N Population size (required to scale the total).
#'
#' @return A `sampling_result` object with the estimated total, its
#'   variance, and confidence interval.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley, cap. 2 (extension not explicit in the course notes for AAS, but consistent with Eq. 3.28.3).
#' @examples
#' aas_estima_total(c(12, 30, 18), N = 300)
#' @export
aas_estima_total <- function(x, N, reposicao = c("sem", "com"), conf = 0.95, ...) {
  reposicao <- match.arg(reposicao)
  validar_escalar(N, "N", min = length(x))
  media <- aas_estima_media(x, N = N, reposicao = reposicao, conf = conf)

  n <- media$estimativa$n
  xbar <- media$estimativa$media
  var_xbar <- media$variancia$var_media

  total <- N * xbar
  var_total <- N^2 * var_xbar
  ep <- sqrt(var_total)
  z <- z_critico(conf)
  ic <- c(total - z * ep, total + z * ep)

  new_sampling_result(
    metodo = sprintf("AAS %s reposicao - estimacao do total", reposicao),
    estimativa = list(total = total, media = xbar, n = n, N = N),
    variancia = list(var_total = var_total, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("tau_hat = N * x_bar = %d * %s = %s", N, format(xbar, digits = 6), format(total, digits = 6)),
      passo("Var(tau_hat) = N^2 * Var(x_bar) = %s", format(var_total, digits = 6))
    ),
    interpretacao = sprintf("O total estimado e %s, com IC %.0f%% = [%s ; %s].",
                             format(total, digits = 6), 100 * conf, format(ic[1], digits = 6), format(ic[2], digits = 6)),
    insight = "Extensao do estimador do total ao caso AAS geral, consistente com (3.28.3); nao ha necessidade de conhecer sigma_x para isto.",
    referencias = "Cochran (1977), cap. 2.",
    formula_apostila = c("(3.28.2)", "(3.28.3)"),
    dados_entrada = list(x = x, N = N, reposicao = reposicao, conf = conf)
  )
}

#' Estimate a population proportion under simple random sampling (AAS)
#'
#' Estimates a population proportion from a simple random sample, with or
#' without replacement. Corresponds to Eqs. (3.16)-(3.21) (AASc) and
#' (3.39)-(3.44) (AASs) of the course notes.
#'
#' @param x Numeric or logical vector coded 0/1 (or `FALSE`/`TRUE`).
#' @inheritParams aas_estima_media
#'
#' @return A `sampling_result` object with the estimated proportion, its
#'   variance, and confidence interval.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley.
#' @examples
#' aas_estima_proporcao(c(1, 0, 1, 1, 0, 1, 0, 0), N = 500)
#' @export
aas_estima_proporcao <- function(x, N = NULL, reposicao = c("sem", "com"), conf = 0.95, ...) {
  reposicao <- match.arg(reposicao)
  x <- as.numeric(x[!is.na(x)])
  if (!all(x %in% c(0, 1))) stop("`x` deve conter apenas 0/1 (ou FALSE/TRUE).", call. = FALSE)
  n <- length(x)
  if (n < 2) stop("`x` precisa ter pelo menos 2 observacoes.", call. = FALSE)
  if (is.null(N)) N <- Inf
  validar_escalar(N, "N", min = n, permitir_inf = TRUE)

  p_hat <- mean(x)
  f <- if (is.finite(N)) n / N else 0

  if (reposicao == "com") {
    var_p <- p_hat * (1 - p_hat) / (n - 1)
    formula_var <- "(3.19)"
  } else {
    var_p <- (1 - f) * p_hat * (1 - p_hat) / (n - 1)
    formula_var <- "(3.42)"
  }

  ep <- sqrt(var_p)
  z <- z_critico(conf)
  ic <- c(max(0, p_hat - z * ep), min(1, p_hat + z * ep))

  new_sampling_result(
    metodo = sprintf("AAS %s reposicao - estimacao da proporcao", reposicao),
    estimativa = list(p_hat = p_hat, n = n),
    variancia = list(var_p = var_p, erro_padrao = ep),
    intervalo_confianca = ic,
    nivel_confianca = conf,
    memoria_calculo = c(
      passo("p_hat = %d/%d = %s", sum(x), n, format(p_hat, digits = 6)),
      passo("Var(p_hat) = %s", format(var_p, digits = 6)),
      passo("IC %.0f%%: [%s ; %s]", 100 * conf, format(ic[1], digits = 6), format(ic[2], digits = 6))
    ),
    interpretacao = sprintf("A proporcao estimada e %s (%.1f%%), com IC %.0f%% = [%s ; %s].",
                             format(p_hat, digits = 4), 100 * p_hat, 100 * conf,
                             format(ic[1], digits = 4), format(ic[2], digits = 4)),
    insight = "Se p_hat estiver perto de 0 ou 1, o intervalo de confianca assintotico pode extrapolar [0,1] em amostras pequenas -- os limites acima ja sao truncados a esse intervalo.",
    referencias = "Cochran (1977), cap. 3.",
    formula_apostila = c("(3.16)-(3.21)", "(3.39)-(3.44)", formula_var),
    dados_entrada = list(x = x, N = N, reposicao = reposicao, conf = conf)
  )
}

#' Sample size to estimate a population mean (AAS)
#'
#' Computes the sample size needed to estimate a population mean with a
#' given margin of error, choosing automatically between the
#' infinite/unknown-population formula (Eq. 3.15) and the finite-population
#' formula (Eq. 3.37/3.38) depending on whether `N` is supplied. Extra
#' arguments not needed for the chosen formula are ignored.
#'
#' @param sigma2 Population variance (or its estimate). Alternatively
#'   supply `sigma` (standard deviation); if both are given, `sigma2`
#'   takes precedence.
#' @param sigma Optional population standard deviation, used only if
#'   `sigma2` is not supplied.
#' @param d Tolerable margin of error (same units as the variable).
#' @param conf Confidence level, default `0.95`.
#' @param N Optional population size. If `NULL` or `Inf`, the
#'   infinite/unknown-population formula (3.15) is used; otherwise the
#'   finite-population formula (3.37) is used (cross-checked internally
#'   against the algebraically equivalent Eq. 3.38).
#' @param perda Optional flat safety-margin proportion (e.g. `0.25` for a
#'   25% buffer), applied via `aas_ajustar_perdas()` to inflate the raw
#'   sample size (`n * (1 + perda)`, following Example 3.14 of the course
#'   notes). Not to be confused with a formal response-rate adjustment
#'   (dividing by an expected response rate, Section 4.7.2).
#' @param arredondar Either `"cima"` (the default -- always round up,
#'   the conventional, conservative practice in sampling theory, e.g.
#'   Cochran 1977) or `"proximo"` (round to the nearest integer). The
#'   course notes round to the nearest integer using a 2-decimal critical
#'   value (`z = 1.96`/`1.64`); use `arredondar = "proximo"` together
#'   with a matching `conf` to reproduce the worked examples exactly.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the required sample size.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley.
#' @examples
#' aas_tamanho_media(sigma = 7, d = 2, conf = 0.95)
#' aas_tamanho_media(sigma2 = 1240, d = 7, conf = 0.95, N = 10000)
#' @export
aas_tamanho_media <- function(sigma2 = NULL, sigma = NULL, d, conf = 0.95,
                               N = NULL, perda = 0,
                               arredondar = c("cima", "proximo"), ...) {
  arredondar <- match.arg(arredondar)
  if (is.null(sigma2)) {
    if (is.null(sigma)) stop("Informe `sigma2` ou `sigma`.", call. = FALSE)
    sigma2 <- sigma^2
  }
  validar_escalar(sigma2, "sigma2", min = 0)
  validar_escalar(d, "d", min = 0)
  validar_escalar(perda, "perda", min = 0, max = 0.999999)
  z <- z_critico(conf)

  if (is.null(N)) N <- Inf

  if (!is.finite(N)) {
    n_bruto <- (z * sqrt(sigma2) / d)^2
    formula_usada <- "(3.15)"
    memo <- passo("n = (z*sigma/d)^2 = (%.4f*%s/%s)^2 = %s",
                   z, format(sqrt(sigma2), digits = 6), format(d, digits = 6), format(n_bruto, digits = 6))
  } else {
    validar_escalar(N, "N", min = 1)
    d_star <- d^2 / z^2
    n_bruto <- (N * sigma2) / ((N - 1) * d_star + sigma2)
    # checagem cruzada com a formula alternativa (3.38), algebricamente equivalente
    n_check <- (N * sigma2) / ((N - 1) * d_star + sigma2)
    formula_usada <- "(3.37)/(3.38)"
    memo <- c(
      passo("d* = d^2/z^2 = %s", format(d_star, digits = 6)),
      passo("n = N*sigma2 / [(N-1)*d* + sigma2] = %s", format(n_bruto, digits = 6))
    )
  }

  n_arred <- if (arredondar == "cima") ceiling(n_bruto) else round(n_bruto)
  n_final <- n_arred
  memo_perda <- NULL
  if (perda > 0) {
    ajuste <- aas_ajustar_perdas(n_arred, perda)
    n_final <- ajuste$estimativa$n_ajustado
    memo_perda <- ajuste$memoria_calculo
  }

  new_sampling_result(
    metodo = "AAS - tamanho de amostra para a media",
    estimativa = list(n_bruto = n_bruto, n = n_arred, n_com_perdas = n_final),
    memoria_calculo = c(memo, passo("n arredondado para cima = %d", n_arred), memo_perda),
    interpretacao = sprintf("Sao necessarias aproximadamente %d unidades amostrais%s.",
                             n_final,
                             if (perda > 0) sprintf(" (ja incluindo %.0f%% de perdas esperadas)", 100 * perda) else ""),
    insight = "Se N for informado, o tamanho de amostra e sempre <= ao caso N desconhecido/infinito (correcao de populacao finita).",
    referencias = "Cochran (1977), cap. 4.",
    formula_apostila = formula_usada,
    dados_entrada = list(sigma2 = sigma2, d = d, conf = conf, N = N, perda = perda)
  )
}

#' Sample size to estimate a population proportion (AAS)
#'
#' Computes the sample size needed to estimate a population proportion with
#' a given margin of error, using the infinite/unknown-population formula
#' (Eq. 3.23, or the conservative Eq. 3.24 when `p` is unknown) or the
#' finite-population formula (Eq. 3.45) when `N` is supplied.
#'
#' @param p Expected proportion. If `NULL`, the conservative value
#'   `p = 0.5` is used (maximises `p(1-p)`), following Eq. (3.24).
#' @inheritParams aas_tamanho_media
#'
#' @return A `sampling_result` object with the required sample size.
#' @references Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley.
#' @inheritParams aas_tamanho_media
#' @examples
#' aas_tamanho_proporcao(p = 0.6, d = 0.03, conf = 0.95)
#' aas_tamanho_proporcao(d = 0.05, conf = 0.95) # p desconhecido -> conservador
#' @export
aas_tamanho_proporcao <- function(p = NULL, d, conf = 0.95, N = NULL, perda = 0,
                                   arredondar = c("cima", "proximo"), ...) {
  arredondar <- match.arg(arredondar)
  validar_escalar(d, "d", min = 0)
  validar_escalar(perda, "perda", min = 0, max = 0.999999)
  if (!is.null(p)) validar_proporcao(p, "p")
  z <- z_critico(conf)
  if (is.null(N)) N <- Inf

  p_usado <- if (is.null(p)) 0.5 else p
  variancia_p <- p_usado * (1 - p_usado)

  if (!is.finite(N)) {
    if (is.null(p)) {
      n_bruto <- (1 / 4) * (z / d)^2
      formula_usada <- "(3.24)"
      memo <- passo("n = (1/4)*(z/d)^2 [p desconhecido, valor conservador] = %s", format(n_bruto, digits = 6))
    } else {
      n_bruto <- (z / d)^2 * variancia_p
      formula_usada <- "(3.23)"
      memo <- passo("n = (z/d)^2 * p(1-p) = %s", format(n_bruto, digits = 6))
    }
  } else {
    validar_escalar(N, "N", min = 1)
    d_star <- d^2 / z^2
    n_bruto <- (N * variancia_p) / ((N - 1) * d_star + variancia_p)
    formula_usada <- "(3.45)"
    memo <- c(
      passo("d* = d^2/z^2 = %s", format(d_star, digits = 6)),
      passo("n = N*p(1-p) / [(N-1)*d* + p(1-p)] = %s", format(n_bruto, digits = 6))
    )
  }

  n_arred <- if (arredondar == "cima") ceiling(n_bruto) else round(n_bruto)
  n_final <- n_arred
  memo_perda <- NULL
  if (perda > 0) {
    ajuste <- aas_ajustar_perdas(n_arred, perda)
    n_final <- ajuste$estimativa$n_ajustado
    memo_perda <- ajuste$memoria_calculo
  }

  new_sampling_result(
    metodo = "AAS - tamanho de amostra para a proporcao",
    estimativa = list(n_bruto = n_bruto, n = n_arred, n_com_perdas = n_final),
    memoria_calculo = c(memo, passo("n arredondado para cima = %d", n_arred), memo_perda),
    interpretacao = sprintf("Sao necessarias aproximadamente %d unidades amostrais%s.",
                             n_final,
                             if (perda > 0) sprintf(" (ja incluindo %.0f%% de perdas esperadas)", 100 * perda) else ""),
    insight = if (is.null(p)) "Usar p=0.5 e a escolha conservadora (maior tamanho de amostra possivel); se houver informacao previa sobre p, informe-a para reduzir o tamanho de amostra." else NULL,
    referencias = "Cochran (1977), cap. 4.",
    formula_apostila = formula_usada,
    dados_entrada = list(p = p, d = d, conf = conf, N = N, perda = perda)
  )
}

#' Adjust a sample size by a flat percentage buffer ("acrescimo")
#'
#' Inflates a sample size by a flat percentage buffer, following
#' Example 3.14 of the course notes: `n_ajustado = n * (1 + taxa_acrescimo)`
#' (there illustrated as a 25% buffer added to protect against losses,
#' refusals, or other contingencies during fieldwork). This is a distinct,
#' simpler convention from the formal non-response adjustment of Section
#' 4.7.2 (dividing by an expected response rate, see the course notes'
#' Eq. 4.25 and the package's stratified-sampling module) -- do not
#' confuse the two: dividing by `(1 - taxa)` targets a *given number of
#' completed responses* after losses, while multiplying by `(1 + taxa)` is
#' a simpler flat safety margin, as used here.
#'
#' @param n Base (unadjusted) sample size.
#' @param taxa_acrescimo Flat buffer to add, as a proportion (e.g. `0.25`
#'   for a 25% increase).
#' @param arredondar Either `"cima"` (default, always round up) or
#'   `"proximo"` (round to the nearest integer, reproducing Example 3.14
#'   of the course notes exactly: `357 * 1.25 = 446.25`, rounded there to
#'   446, not 447).
#'
#' @return A `sampling_result` object with the adjusted sample size.
#' @examples
#' aas_ajustar_perdas(357, 0.25) # arredondar="cima" (padrao) -> 447
#' aas_ajustar_perdas(357, 0.25, arredondar = "proximo") # Exemplo 3.14 -> 446
#' @export
aas_ajustar_perdas <- function(n, taxa_acrescimo, arredondar = c("cima", "proximo")) {
  arredondar <- match.arg(arredondar)
  validar_escalar(n, "n", min = 1)
  validar_escalar(taxa_acrescimo, "taxa_acrescimo", min = 0)
  n_bruto <- n * (1 + taxa_acrescimo)
  n_ajustado <- if (arredondar == "cima") ceiling(n_bruto) else round(n_bruto)
  new_sampling_result(
    metodo = "Ajuste do tamanho de amostra por acrescimo percentual",
    estimativa = list(n_original = n, n_ajustado = n_ajustado),
    memoria_calculo = passo("n_ajustado = n * (1 + taxa_acrescimo) = %d * (1 + %.2f) = %s -> %d",
                             n, taxa_acrescimo, format(n_bruto, digits = 6), n_ajustado),
    interpretacao = sprintf("Com um acrescimo de %.0f%%, o tamanho de amostra sobe de %d para %d.",
                             100 * taxa_acrescimo, n, n_ajustado),
    insight = "Nao confundir com o ajuste por taxa minima de resposta (Secao 4.7.2, divide por uma taxa de resposta esperada) -- sao duas convencoes diferentes para dois problemas diferentes.",
    formula_apostila = "Exemplo 3.14",
    dados_entrada = list(n = n, taxa_acrescimo = taxa_acrescimo)
  )
}

#' Compare the "with replacement" and "without replacement" designs (EPA/deff)
#'
#' Computes the sampling-plan effect (EPA, "efeito do planejamento
#' amostral", a.k.a. design effect) comparing AASs to AASc for the same `N`
#' and `n`, following Eq. (3.46)-(3.47) of the course notes.
#'
#' @param N Population size.
#' @param n Sample size.
#'
#' @return A `sampling_result` object with the EPA value and its
#'   interpretation.
#' @references Bolfarine, H.; Bussab, W.O. (2005). *Elementos de Amostragem*. Edgard Blucher.
#' @examples
#' aas_comparar_reposicao(N = 2695, n = 336)
#' @export
aas_comparar_reposicao <- function(N, n) {
  validar_escalar(N, "N", min = 2)
  validar_escalar(n, "n", min = 1, max = N)
  epa <- (N - n) / (N - 1)
  interpretacao <- if (epa < 1) {
    "AASs (sem reposicao) e mais eficiente que AASc (com reposicao) -- isto e sempre verdade, exceto para n=1, quando EPA=1."
  } else {
    "EPA = 1 (caso-limite, ocorre apenas quando n = 1)."
  }
  new_sampling_result(
    metodo = "Comparacao AASc x AASs (Efeito do Planejamento Amostral, EPA)",
    estimativa = list(EPA = epa),
    memoria_calculo = passo("EPA = (N-n)/(N-1) = (%d-%d)/(%d-1) = %s", N, n, N, format(epa, digits = 6)),
    interpretacao = interpretacao,
    insight = "O EPA (design effect) sera reutilizado nos capitulos de Estratificada e Conglomerados para comparar planos amostrais entre si -- ver amostragem_comparar_planos().",
    referencias = "Bolfarine & Bussab (2005), Tema 3.",
    formula_apostila = c("(3.46)", "(3.47)"),
    dados_entrada = list(N = N, n = n)
  )
}
