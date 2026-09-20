test_that("aas_tamanho_media reproduz Exemplo 3.8 (N desconhecido)", {
  res <- aas_tamanho_media(sigma = 7, d = 2, conf = 0.95)
  expect_equal(res$estimativa$n_bruto, 47.06, tolerance = 0.01)
  expect_equal(res$estimativa$n, 48) # arredondar='cima' (padrao, conservador)

  res_proximo <- aas_tamanho_media(sigma = 7, d = 2, conf = 0.95, arredondar = "proximo")
  expect_equal(res_proximo$estimativa$n, 47) # replica exatamente a apostila
})

test_that("aas_tamanho_proporcao reproduz Exemplo 3.9 (p conhecido)", {
  res <- aas_tamanho_proporcao(p = 0.6, d = 0.03, conf = 0.95, arredondar = "proximo")
  expect_equal(res$estimativa$n, 1024)
})

test_that("aas_tamanho_proporcao reproduz Exemplo 3.10 (p desconhecido, conservador)", {
  res <- aas_tamanho_proporcao(d = 0.05, conf = 0.95, arredondar = "proximo")
  expect_equal(res$estimativa$n, 384)
  expect_null(res$dados_entrada$p)
})

test_that("aas_tamanho_media reproduz Exemplo 3.11 (conf=0.90)", {
  res <- aas_tamanho_media(sigma = 3, d = 0.8, conf = 0.90, arredondar = "proximo")
  expect_equal(res$estimativa$n, 38)
})

test_that("aas_tamanho_media reproduz Exemplo 3.12 (N conhecido, formulas 3.37/3.38)", {
  res <- aas_tamanho_media(sigma2 = 1240, d = 7, conf = 0.95, N = 10000, arredondar = "proximo")
  expect_equal(res$estimativa$n, 96)
})

test_that("aas_tamanho_proporcao reproduz Exemplo 3.13 (N conhecido)", {
  res <- aas_tamanho_proporcao(d = 0.05, conf = 0.95, N = 5000, arredondar = "proximo")
  expect_equal(res$estimativa$n, 357)
})

test_that("aas_ajustar_perdas reproduz Exemplo 3.14 (acrescimo de 25%: 357 -> 446 com arredondar='proximo')", {
  res <- aas_ajustar_perdas(357, 0.25, arredondar = "proximo")
  expect_equal(res$estimativa$n_ajustado, 446)
  # padrao ('cima', mais conservador) arredonda para cima: 446.25 -> 447
  res_cima <- aas_ajustar_perdas(357, 0.25)
  expect_equal(res_cima$estimativa$n_ajustado, 447)
})

test_that("aas_estima_media da variancia menor ou igual sem reposicao vs com reposicao", {
  set.seed(42)
  x <- rnorm(30, mean = 50, sd = 5)
  com <- aas_estima_media(x, N = 200, reposicao = "com")
  sem <- aas_estima_media(x, N = 200, reposicao = "sem")
  expect_true(sem$variancia$var_media <= com$variancia$var_media)
})

test_that("aas_estima_media com N infinito faz com e sem reposicao coincidirem", {
  set.seed(42)
  x <- rnorm(30, mean = 50, sd = 5)
  com <- aas_estima_media(x, reposicao = "com")
  sem <- aas_estima_media(x, reposicao = "sem")
  expect_equal(com$variancia$var_media, sem$variancia$var_media)
})

test_that("aas_estima_proporcao produz IC dentro de [0,1]", {
  set.seed(1)
  x <- rbinom(50, 1, 0.5)
  res <- aas_estima_proporcao(x, N = 1000)
  expect_true(res$intervalo_confianca[1] >= 0)
  expect_true(res$intervalo_confianca[2] <= 1)
})

test_that("aas_estima_total = N * media", {
  x <- c(12, 30, 18)
  res <- aas_estima_total(x, N = 300)
  expect_equal(res$estimativa$total, 300 * mean(x))
})

test_that("aas_comparar_reposicao calcula EPA corretamente e EPA <= 1", {
  res <- aas_comparar_reposicao(N = 2695, n = 336)
  expect_equal(res$estimativa$EPA, (2695 - 336) / (2695 - 1), tolerance = 1e-8)
  expect_true(res$estimativa$EPA <= 1)
})

test_that("aas_tamanho_amostra (guarda-chuva) ignora argumentos excedentes", {
  # passa 'p' junto com parametro='media' -- deve ser ignorado sem erro
  res <- aas_tamanho_amostra(parametro = "media", sigma = 7, d = 2, p = 0.9,
                              dados_extra_irrelevantes = "abc")
  expect_equal(res$estimativa$n, 48)
})
