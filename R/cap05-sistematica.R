#' Sampling interval for systematic sampling
#'
#' Computes the sampling interval `k = N/n` used to select a systematic
#' sample, following Section 5.1-5.2 of the course notes. When `N/n` is
#' not an integer, `k` is first rounded up; if `k*n > N`, `k` is then
#' decremented by 1, and the correction constant `k* = N - k*n` is
#' computed (Example 5.2 of the course notes).
#'
#' @param N Population size.
#' @param n Desired sample size.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the sampling interval (and,
#'   when `N/n` is not an integer, the correction constant `k*` needed by
#'   [sist_selecionar_amostra()]).
#' @examples
#' sist_intervalo(N = 2000, n = 80)
#' sist_intervalo(N = 2000, n = 75)
#' @export
sist_intervalo <- function(N, n, ...) {
  validar_escalar(N, "N", min = 1)
  validar_escalar(n, "n", min = 1, max = N)
  k_exato <- N / n
  k_inteiro <- (k_exato == round(k_exato))

  if (k_inteiro) {
    k <- as.integer(k_exato)
    k_estrela <- 0
    memo <- passo("k = N/n = %d/%d = %d (inteiro)", N, n, k)
  } else {
    k <- ceiling(k_exato)
    memo_arred <- passo("k = N/n = %d/%d = %.4f (nao inteiro, arredondar para cima -> k=%d)", N, n, k_exato, k)
    if (k * n > N) {
      k <- k - 1
      memo_ajuste <- passo("k*n = %d*%d = %d > N=%d -> reduzir k em 1 -> k=%d", k + 1, n, (k + 1) * n, N, k)
    } else {
      memo_ajuste <- NULL
    }
    k_estrela <- N - k * n
    memo <- c(memo_arred, memo_ajuste, passo("k* = N - k*n = %d - %d*%d = %d", N, k, n, k_estrela))
  }

  new_sampling_result(
    metodo = "Sistematica - intervalo de selecao",
    estimativa = list(k = k, k_estrela = k_estrela, k_inteiro = k_inteiro),
    memoria_calculo = memo,
    interpretacao = if (k_inteiro) {
      sprintf("O intervalo de selecao e k=%d (inteiro); a amostra sistematica e simples de obter.", k)
    } else {
      sprintf("O intervalo de selecao aproximado e k=%d, com uma correcao k*=%d a ser somada a partir do segundo elemento selecionado.", k, k_estrela)
    },
    formula_apostila = "Secao 5.2",
    dados_entrada = list(N = N, n = n)
  )
}

#' Select a systematic sample
#'
#' Selects a systematic sample of size `n` from a population of size `N`,
#' handling both the integer-interval case and the non-integer-interval
#' case (with the correction constant `k*`), following the selection
#' rules of Section 5.2 of the course notes (Examples 5.1-5.2). In the
#' non-integer case, the first selected element is `r`; from the second
#' element onward, `k*` is added on top of the regular step `k`
#' (i.e. element `i` for `i >= 2` equals `(i-1)*k + k*` plus `r`), exactly as
#' illustrated in Example 5.2.
#'
#' @param N Population size.
#' @param n Desired sample size.
#' @param r Optional random start (an integer between 1 and `k`,
#'   inclusive). If `NULL`, one is drawn uniformly at random.
#' @param semente Optional integer seed for reproducibility. The user's
#'   global random state is saved and restored, so calling this function
#'   does not affect subsequent unrelated random draws.
#' @param ... Ignored (see package overview on tolerated extra arguments).
#'
#' @return A `sampling_result` object with the selected element positions
#'   (`1:N`).
#' @examples
#' sist_selecionar_amostra(N = 2000, n = 80, r = 8)
#' sist_selecionar_amostra(N = 2000, n = 75, r = 26)
#' @export
sist_selecionar_amostra <- function(N, n, r = NULL, semente = NULL, ...) {
  info <- sist_intervalo(N, n)
  k <- info$estimativa$k
  k_estrela <- info$estimativa$k_estrela
  k_inteiro <- info$estimativa$k_inteiro

  if (is.null(r)) {
    if (!is.null(semente)) {
      old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
      on.exit({
        if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }, add = TRUE)
      set.seed(semente)
    }
    r <- sample.int(k, 1)
  }
  validar_escalar(r, "r", min = 1, max = k)

  if (k_inteiro) {
    elementos <- r + (0:(n - 1)) * k
    formula_usada <- "Secao 5.2 (k inteiro)"
  } else {
    i <- seq_len(n)
    ajuste <- ifelse(i == 1, 0, k_estrela)
    elementos <- r + (i - 1) * k + ajuste
    formula_usada <- "Secao 5.2 (k nao inteiro, com correcao k* a partir do 2o elemento)"
  }
  elementos <- elementos[elementos <= N]

  new_sampling_result(
    metodo = "Sistematica - selecao da amostra",
    estimativa = list(elementos = elementos, k = k, r = r, n_selecionado = length(elementos)),
    memoria_calculo = c(
      info$memoria_calculo,
      passo("r (inicio aleatorio) = %d", r),
      passo("elementos selecionados: %s", paste(utils::head(elementos, 10), collapse = ", "))
    ),
    interpretacao = sprintf("Foram selecionados %d elementos, comecando em r=%d e avancando de %d em %d posicoes%s.",
                             length(elementos), r, k, k,
                             if (!k_inteiro) sprintf(" (com correcao k*=%d a partir do 2o elemento)", k_estrela) else ""),
    insight = "Na pratica, a variancia da amostragem sistematica costuma ser aproximada pela formula de AAS (Cochran, 1977, cap. 8) -- a apostila nao apresenta uma formula fechada propria para este plano.",
    referencias = "Cochran (1977), cap. 8.",
    formula_apostila = formula_usada,
    dados_entrada = list(N = N, n = n, r = r, semente = semente)
  )
}
