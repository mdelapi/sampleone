turmas_Mi <- c(20, 16, 17, 21, 16, 25, 21, 26, 25, 23)
turmas_yi_peso <- c(1209, 921, 1116, 1433, 1037, 2112, 1737, 2206, 2112, 1870)
turmas_yi_prop <- c(7, 6, 5, 6, 5, 7, 5, 6, 6, 6)

test_that("cong1_estima_media reproduz Exemplo 6.1 (a,b)", {
  res <- cong1_estima_media(yi = turmas_yi_peso, Mi = turmas_Mi, Nc = 373, M = 8018)
  expect_equal(res$estimativa$media, 75.0143, tolerance = 1e-4)
  expect_equal(res$variancia$Sc2, 51054.3854, tolerance = 1e-3)
  expect_equal(res$variancia$var_media, 10.7527, tolerance = 1e-3)
  expect_equal(res$intervalo_confianca[1], 68.5872, tolerance = 2e-2)
  expect_equal(res$intervalo_confianca[2], 81.4414, tolerance = 2e-2)
})

test_that("cong1_tamanho_media reproduz Exemplo 6.1 (c)", {
  res <- cong1_tamanho_media(Nc = 373, sigmac2 = 51054.3854, M_barra = 8018 / 373, d = 3)
  expect_equal(res$estimativa$nc, 42)
})

test_that("cong1_ajustar_deff reproduz Exemplo 6.2 (n'=518 -> n=868, com nota de arredondamento)", {
  res <- cong1_ajustar_deff(n_prime = 518, deff = 1.5, taxa_resposta = 0.85)
  # A apostila arredonda o termo intermediario (518/0.85=609.41 -> 609) antes de
  # subtrair, chegando a 868; nossa formula nao arredonda o termo intermediario,
  # dando 869. Diferenca de 1 unidade documentada -- ver R/cap06-conglomerados-1etapa.R
  expect_true(res$estimativa$n_final %in% c(868, 869))
})

test_that("cong1_estima_proporcao reproduz Exemplo 6.3", {
  res <- cong1_estima_proporcao(yi = turmas_yi_prop, Mi = turmas_Mi, Nc = 373, M = 8018)
  expect_equal(res$estimativa$p_c, 0.28095238, tolerance = 1e-6)
  expect_equal(res$variancia$Scp2, 0.91785840, tolerance = 1e-4)
})

test_that("cong1_tamanho_proporcao reproduz Exemplo 6.3", {
  res <- cong1_tamanho_proporcao(Nc = 373, sigmacp2 = 0.91785840, M_barra = 8018 / 373, d = 0.013,
                                  arredondar = "proximo")
  expect_equal(res$estimativa$ncp, 40)
})
