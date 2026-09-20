# Golden tests derivados da Lista 1 (Temas 1-5) do Prof. Dr. Mariano Martinez
# Espinosa, resolvidos de forma independente (Python) ANTES de consultar o
# codigo do pacote -- ver VALIDACAO_LISTA01.md para o detalhamento completo
# de cada calculo e as paginas/formulas citadas.

test_that("Questao 2a: prevalencia diabetes, N=45000, p=0.20, d=0.025 (formula 3.45) + acrescimo 25%", {
  base <- aas_tamanho_proporcao(p = 0.20, d = 0.025, conf = 0.95, N = 45000)
  expect_equal(base$estimativa$n, 963)
  ajustado <- aas_ajustar_perdas(base$estimativa$n, 0.25)
  expect_equal(ajustado$estimativa$n_ajustado, 1204)
})

test_that("Questao 2b: mesma prevalencia, N desconhecido (formula 3.23)", {
  res <- aas_tamanho_proporcao(p = 0.20, d = 0.025, conf = 0.95)
  expect_equal(res$estimativa$n, 984)
})

test_that("Questao 2c: prevalencia com deff=1.2 e taxa de resposta=85% (formula 6.6)", {
  n_prime <- aas_tamanho_proporcao(p = 0.20, d = 0.05, conf = 0.95)$estimativa$n
  expect_equal(n_prime, 246)
  ajustado <- amostragem_ajustar_deff_resposta(n_prime, deff = 1.2, taxa_resposta = 0.85)
  expect_equal(ajustado$estimativa$n_final, 339)
})

test_that("Questao 3a: soja 3 estratos, alocacao proporcional (formulas 4.5/4.6)", {
  res <- estr_tamanho_media(Nk = c(2500, 1000, 500), sigmak2 = c(64, 400, 1600),
                             d = 5, conf = 0.95, alocacao = "proporcional")
  expect_equal(res$estimativa$n, 52)
  expect_equal(as.numeric(res$estimativa$nk), c(33, 13, 6))
})

test_that("Questao 3b: soja 3 estratos, alocacao de Neyman (formula 4.21) -- caso de empate exato Nk*sigmak", {
  res <- estr_tamanho_media(Nk = c(2500, 1000, 500), sigmak2 = c(64, 400, 1600),
                             d = 5, conf = 0.95, alocacao = "neyman")
  expect_equal(res$estimativa$n, 35)
  expect_equal(sum(res$estimativa$nk), 35)
  # Nk*sigmak e identico nos 3 estratos (20000) -> alocacao deve ser quase igual
  expect_true(all(abs(res$estimativa$nk - 35 / 3) <= 1))
})

test_that("Questao 3d: soja, sigma2=700 unico (reduz a AAS), com deff=1.5 e resposta=85%", {
  n_prime <- aas_tamanho_media(sigma2 = 700, d = 5, conf = 0.95)$estimativa$n
  expect_equal(n_prime, 108)
  ajustado <- amostragem_ajustar_deff_resposta(n_prime, deff = 1.5, taxa_resposta = 0.85)
  expect_equal(ajustado$estimativa$n_final, 182)
})

test_that("Questao 4a: satisfacao salarial, alocacao proporcional (formula 4.12)", {
  res <- estr_tamanho_proporcao(Nk = c(500, 920, 430, 200), pk = c(0.10, 0.50, 0.30, 0.10),
                                 d = 0.05, conf = 0.95, alocacao = "proporcional")
  expect_equal(res$estimativa$n, 252)
  expect_equal(as.numeric(res$estimativa$nk), c(61, 113, 53, 25))
})

test_that("Questao 4b: satisfacao salarial, alocacao de Neyman (formula 4.21 com sqrt(pk(1-pk)))", {
  res <- estr_tamanho_proporcao(Nk = c(500, 920, 430, 200), pk = c(0.10, 0.50, 0.30, 0.10),
                                 d = 0.05, conf = 0.95, alocacao = "neyman")
  expect_equal(res$estimativa$n, 242)
  expect_equal(as.numeric(res$estimativa$nk), c(42, 128, 55, 17))
})

test_that("Questao 5: amostragem sistematica N=1500, n=40 (k nao inteiro, com correcao k*)", {
  info <- sist_intervalo(N = 1500, n = 40)
  expect_equal(info$estimativa$k, 37)
  expect_equal(info$estimativa$k_estrela, 20)

  amostra <- sist_selecionar_amostra(N = 1500, n = 40, r = 15)
  expect_equal(amostra$estimativa$n_selecionado, 40)
  expect_equal(amostra$estimativa$elementos[1:5], c(15, 72, 109, 146, 183))
  expect_equal(utils::tail(amostra$estimativa$elementos, 1), 1478)
})

test_that("amostragem_ajustar_deff_resposta e cong1_ajustar_deff sao equivalentes (promocao pos-validacao)", {
  a <- amostragem_ajustar_deff_resposta(246, deff = 1.2, taxa_resposta = 0.85)
  b <- cong1_ajustar_deff(246, deff = 1.2, taxa_resposta = 0.85)
  expect_equal(a$estimativa, b$estimativa)
})
