fabricas_Mi <- c(50, 65, 45, 48, 52, 58, 42, 66, 40, 56)
fabricas_mi <- c(10, 13, 9, 10, 10, 12, 8, 13, 8, 11)
fabricas_yij_total <- c(4, 5, 2, 3, 5, 3, 3, 4, 2, 4) # nº de maquinas com reparos maiores, por conglomerado (Exemplo 6.6)
fabricas_yi_bar <- c(5.4, 4.0, 5.66667, 4.8, 4.3, 3.83333, 5.0, 3.84615, 4.875, 5.0)
fabricas_si2 <- c(11.37778, 10.66667, 16.75, 13.28889, 11.12222, 14.87879,
                   5.14286, 4.30769, 6.125, 11.8)
# fracoes exatas (yij_total/mi), para evitar erro de arredondamento propagado
fabricas_pi_hat <- fabricas_yij_total / fabricas_mi

test_that("cong2_estima_media reproduz Exemplo 6.4 (M conhecido)", {
  res <- cong2_estima_media(Mi = fabricas_Mi, mi = fabricas_mi, yi_bar = fabricas_yi_bar,
                             si2 = fabricas_si2, Nc = 90, M = 4500)
  expect_equal(res$estimativa$media, 4.8004, tolerance = 1e-3)
  expect_equal(res$variancia$Sb2, 768.327, tolerance = 1e-2)
  expect_equal(res$variancia$parte1_entre, 0.02731829, tolerance = 1e-5)
  expect_equal(res$variancia$parte2_dentro, 0.00977124, tolerance = 1e-5)
  expect_equal(res$variancia$var_media, 0.03708953, tolerance = 1e-5)
  expect_equal(res$intervalo_confianca, c(4.42, 5.18), tolerance = 5e-3)
})

test_that("cong2_estima_razao_media reproduz Exemplo 6.5 (M desconhecido)", {
  res <- cong2_estima_razao_media(Mi = fabricas_Mi, mi = fabricas_mi, yi_bar = fabricas_yi_bar,
                                   si2 = fabricas_si2, Nc = 90)
  expect_equal(res$estimativa$media, 4.598, tolerance = 1e-3)
  expect_equal(res$variancia$var_media, 0.049234, tolerance = 1e-4)
  expect_equal(res$intervalo_confianca, c(4.16, 5.03), tolerance = 5e-3)
})

test_that("cong2_estima_proporcao reproduz Exemplo 6.6", {
  res <- cong2_estima_proporcao(Mi = fabricas_Mi, mi = fabricas_mi, pi_hat = fabricas_pi_hat, Nc = 90)
  expect_equal(res$estimativa$p_c2e, 0.3378, tolerance = 1e-3)
  expect_equal(res$estimativa$m_barra, 52.2, tolerance = 1e-6)
  expect_equal(res$variancia$Sbp2, 18.5739, tolerance = 1e-2)
  # comparado com diferenca absoluta: 0.00081233 no catalogo ja e um valor
  # truncado, entao tolerancia relativa (o padrao do expect_equal) seria
  # exigente demais aqui.
  expect_true(abs(res$variancia$var_p - 0.00081233) < 1e-5)
  # nota: a apostila arredonda p_hat_c2e para 0,34 e o limite de erro (B)
  # para 0,06 (ambos a 2 casas) ANTES de somar/subtrair, chegando a
  # [0,28 ; 0,40]. O valor preciso (sem esse arredondamento em cascata) é
  # (0,2819 ; 0,3936) -- por isso a tolerancia aqui e mais larga que nos
  # demais testes.
  expect_true(all(abs(res$intervalo_confianca - c(0.28, 0.40)) < 0.01))
})

test_that("cong_estima (guarda-chuva) escolhe razao quando M e NULL, media quando M e fornecido", {
  r1 <- cong_estima(Mi = fabricas_Mi, mi = fabricas_mi, yi_bar = fabricas_yi_bar,
                     si2 = fabricas_si2, Nc = 90)
  expect_match(r1$metodo, "estimador razao")

  r2 <- cong_estima(Mi = fabricas_Mi, mi = fabricas_mi, yi_bar = fabricas_yi_bar,
                     si2 = fabricas_si2, Nc = 90, M = 4500)
  expect_match(r2$metodo, "M conhecido")
})

test_that("cong2_estima_media aceita dados brutos via data.frame (dados=)", {
  set.seed(1)
  dados <- data.frame(
    conglomerado = rep(1:3, each = 5),
    Mi = rep(c(50, 60, 40), each = 5),
    y = c(5, 7, 9, 0, 11, 4, 3, 7, 2, 11, 5, 6, 4, 11, 12)
  )
  res <- cong2_estima_media(dados = dados, Nc = 20, M = 1000)
  expect_equal(res$estimativa$nc, 3)
})
