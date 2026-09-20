test_that("estr_tamanho_media + alocacao proporcional reproduzem Exemplo 4.1", {
  res <- estr_tamanho_media(Nk = c(1540, 770, 385), sigmak2 = c(7, 9, 11), d = 0.5,
                             conf = 0.95, alocacao = "proporcional")
  expect_equal(res$estimativa$n, 120)
  expect_equal(as.numeric(res$estimativa$nk), c(69, 34, 17))
})

test_that("estr_tamanho_media com custo reproduz Exemplo 4.3", {
  res <- estr_tamanho_media(Nk = c(15000, 10000, 5000), sigmak2 = c(9, 9, 4),
                             d = 0.25, conf = 0.95, ck = c(10, 10, 20),
                             alocacao = "custo", arredondar = "proximo")
  expect_equal(res$estimativa$n, 491)
  expect_equal(as.numeric(res$estimativa$nk), c(269, 180, 42))
})

test_that("estr_tamanho_proporcao reproduz Exemplo 4.4 (com arredondar='proximo')", {
  res <- estr_tamanho_proporcao(Nk = c(1540, 770, 385), pk = c(0.75, 0.50, 0.25),
                                 d = 0.05, conf = 0.95, arredondar = "proximo")
  expect_equal(res$estimativa$n, 282)
  expect_equal(as.numeric(res$estimativa$nk), c(161, 81, 40))
})

test_that("estr_estima_proporcao reproduz a variancia do Exemplo 4.4", {
  res <- estr_estima_proporcao(Nk = c(1540, 770, 385), pk = c(0.75, 0.50, 0.25),
                                nk = c(161, 81, 40))
  # 0.00065884 no catalogo é o valor ja truncado a 8 casas; comparamos aqui
  # a diferenca absoluta em vez de usar tolerancia relativa (que seria
  # exigente demais para um numero dessa ordem de grandeza).
  expect_true(abs(res$variancia$var_p - 0.00065884) < 1e-7)
})

test_that("EPA do Exemplo 4.4 (estratificada vs AAS) e proximo de 1.0086", {
  var_estratificada <- 0.00065884
  aas_ref <- aas_tamanho_proporcao(p = 0.5, d = 0.05, N = 2695, arredondar = "proximo")
  expect_equal(aas_ref$estimativa$n, 336)
  # var(p_hat) da AAS com n=336 (formula 3.42)
  N <- 2695; n <- 336
  var_aas <- (1 - n / N) * (0.5 * 0.5) / (n - 1)
  epa <- var_estratificada / var_aas
  expect_equal(epa, 1.00863442, tolerance = 1e-4)
})

test_that("estr_tamanho_proporcao (proporcional) reproduz Exemplo 4.5", {
  res <- estr_tamanho_proporcao(Nk = c(1500, 2700, 3300), pk = c(0.10, 0.20, 0.40),
                                 d = 0.05, conf = 0.95, arredondar = "proximo")
  expect_equal(res$estimativa$n, 268)
  expect_equal(as.numeric(res$estimativa$nk), c(54, 96, 118))
})

test_that("estr_alocacao_neyman reproduz a alocacao otima do Exemplo 4.5", {
  n_neyman <- 261
  res <- estr_alocacao_neyman(n = n_neyman, Nk = c(1500, 2700, 3300), pk = c(0.10, 0.20, 0.40))
  expect_equal(as.numeric(res$estimativa$nk), c(37, 90, 134))
})

test_that("estr_tamanho_proporcao com alocacao='neyman' reproduz n=261 do Exemplo 4.5", {
  res <- estr_tamanho_proporcao(Nk = c(1500, 2700, 3300), pk = c(0.10, 0.20, 0.40),
                                 d = 0.05, conf = 0.95, alocacao = "neyman",
                                 arredondar = "proximo")
  expect_equal(res$estimativa$n, 261)
})

test_that("amostragem_ajustar_resposta reproduz o caso de ausencia de resposta (208045 hab.)", {
  res_n <- aas_tamanho_proporcao(p = 0.5, d = 0.025, N = 208045, arredondar = "proximo")
  expect_equal(res_n$estimativa$n, 1525)

  res_ajustado <- amostragem_ajustar_resposta(1525, 0.85)
  expect_equal(res_ajustado$estimativa$n_ajustado, 1795)
})

test_that("estr_tamanho_amostra (guarda-chuva) ignora argumentos excedentes", {
  res <- estr_tamanho_amostra(parametro = "media", Nk = c(1540, 770, 385),
                               sigmak2 = c(7, 9, 11), pk = c(0.1, 0.2, 0.3), d = 0.5,
                               variavel_irrelevante = TRUE)
  expect_equal(res$estimativa$n, 120)
})
