#' Adjust a sample size for design effect and expected response rate
#'
#' Inflates a base sample size (computed under a simple-random-sampling
#' assumption) by a design effect (`deff`, accounting for the extra
#' variability introduced by a more complex design such as clustering or
#' multi-stage sampling) and by an expected response rate, following
#' Eq. (6.6) of the course notes (Espinosa et al., 2019).
#'
#' Although this formula is first introduced in the course notes' cluster
#' sampling chapter, it is a **general-purpose adjustment**: it only needs
#' a base sample size, a design effect, and a response rate -- nothing
#' specific to clusters. Course exercises apply it to plain AAS and
#' stratified problems just as often as to cluster problems (e.g. to
#' scale up a prevalence-survey sample size for an anticipated design
#' effect and non-response), so it is exposed here as a cross-cutting
#' utility rather than tucked away under the `cong1_` prefix.
#' [cong1_ajustar_deff()] remains available as an alias, for continuity
#' with the cluster-sampling chapter and its documentation.
#'
#' @param n_prime Base sample size (e.g. from [aas_tamanho_media()],
#'   [aas_tamanho_proporcao()], [estr_tamanho_media()], or
#'   [cong1_tamanho_media()]), ignoring the design effect and non-response.
#' @param deff Design effect (`>= 1`), default `1.5` as commonly assumed
#'   in the absence of better information. `deff = 1` means no loss of
#'   efficiency relative to simple random sampling.
#' @param taxa_resposta Expected response rate, default `0.85`.
#'
#' @return A `sampling_result` object with the adjusted sample size.
#' @references Espinosa et al. (2019), as cited in the course notes, Eq. (6.6).
#' @examples
#' # Cap. 3 (AAS) + deff + resposta -- ex.: planejamento de prevalencia
#' n_prime <- aas_tamanho_proporcao(p = 0.20, d = 0.05)$estimativa$n
#' amostragem_ajustar_deff_resposta(n_prime, deff = 1.2, taxa_resposta = 0.85)
#' @export
amostragem_ajustar_deff_resposta <- function(n_prime, deff = 1.5, taxa_resposta = 0.85) {
  validar_escalar(n_prime, "n_prime", min = 1)
  validar_escalar(deff, "deff", min = 1)
  validar_proporcao(taxa_resposta, "taxa_resposta")
  n_deff <- n_prime * deff
  n_final <- ceiling(n_deff + (n_prime / taxa_resposta - n_prime))

  new_sampling_result(
    metodo = "Ajuste do tamanho de amostra (efeito de delineamento + taxa de resposta)",
    estimativa = list(n_prime = n_prime, n_deff = n_deff, n_final = n_final),
    memoria_calculo = passo("n = n'*deff + (n'/taxa_resposta - n') = %s*%.2f + (%s/%.2f - %s) = %d",
                             format(n_prime), deff, format(n_prime), taxa_resposta, format(n_prime), n_final),
    interpretacao = sprintf("Considerando um efeito de delineamento de %.2f e uma taxa de resposta esperada de %.0f%%, o tamanho final de amostra e %d.",
                             deff, 100 * taxa_resposta, n_final),
    insight = "O design effect (deff) mede o quanto a variancia sob um plano mais complexo (conglomerados, multietapas) e maior do que sob AAS para o mesmo n -- deff=1 significa nenhuma perda de eficiencia. Aplicavel a qualquer n' de base (AAS, estratificada ou conglomerados), nao apenas a conglomerados.",
    formula_apostila = "(6.6)",
    dados_entrada = list(n_prime = n_prime, deff = deff, taxa_resposta = taxa_resposta)
  )
}
