x71 <- c(39, 43, 21, 64, 57, 47, 28, 75, 34, 52)
y71 <- c(65, 78, 52, 82, 92, 89, 73, 98, 56, 75)

x72 <- c(1, 30, 44, 20, 0, 10, 15, 5, 2, 50, 35, 25)
y72 <- c(2, 35, 50, 27, 1, 15, 17, 7, 0, 53, 35, 30)

test_that("reg_ajustar reproduz Exemplo 7.1 (b)", {
  res <- reg_ajustar(x71, y71)
  expect_equal(res$estimativa$a1, 0.76556184, tolerance = 1e-6)
  expect_equal(res$estimativa$a0, 40.78415536, tolerance = 1e-5)
})

test_that("reg_teste_origem reproduz Exemplo 7.1 (d): reta NAO passa pela origem -> regressao", {
  res <- reg_teste_origem(x71, y71)
  expect_equal(res$estimativa$t_a0, 4.7946, tolerance = 1e-3)
  expect_equal(res$estimativa$recomendado, "regressao")
})

test_that("reg_estima_media reproduz Exemplo 7.1 (e,f)", {
  res <- reg_estima_media(x71, y71, X_barra = 52, N = 486)
  expect_equal(res$estimativa$media, 80.5936, tolerance = 1e-3)
  expect_equal(res$variancia$var_media, 7.4195, tolerance = 1e-3)
  expect_equal(res$intervalo_confianca, c(75.2548, 85.9324), tolerance = 5e-3)
})

test_that("reg_ajustar reproduz Exemplo 7.2 (b)", {
  res <- reg_ajustar(x72, y72)
  expect_equal(res$estimativa$a1, 1.06893643, tolerance = 1e-6)
  expect_equal(res$estimativa$a0, 1.55517215, tolerance = 1e-5)
})

test_that("reg_teste_origem reproduz Exemplo 7.2 (d): reta passa pela origem -> razao", {
  res <- reg_teste_origem(x72, y72)
  expect_equal(res$estimativa$t_a0, 1.35, tolerance = 1e-2)
  expect_equal(res$estimativa$recomendado, "razao")
})

test_that("razao_estima_media reproduz Exemplo 7.2 (e,f)", {
  res <- razao_estima_media(x = x72, y = y72, N = 1500, X = 15000)
  expect_equal(res$estimativa$media, 11.48, tolerance = 1e-2)
  expect_equal(res$variancia$var_media, 0.64358407, tolerance = 1e-4)
  expect_equal(res$intervalo_confianca, c(9.91, 13.05), tolerance = 1e-2)
})

test_that("razao_estima_R reproduz Exemplo 7.3", {
  res <- razao_estima_R(x = x72, y = y72, N = 1500, X = 15000)
  expect_equal(res$estimativa$R, 1.15, tolerance = 1e-2)
  expect_equal(res$variancia$var_R, 0.00643584, tolerance = 1e-6)
  expect_equal(res$intervalo_confianca, c(0.99, 1.31), tolerance = 1e-2)
})

test_that("razao_estima_total reproduz Exemplo 7.4", {
  res <- razao_estima_total(x = x72, y = y72, N = 1500, X = 15000)
  expect_equal(res$estimativa$total, 17250, tolerance = 1)
  expect_equal(res$variancia$var_total, 1448064, tolerance = 10)
  expect_equal(res$intervalo_confianca, c(14891.42, 19608.58), tolerance = 5)
})

test_that("razao_estima_media aceita somatorios agregados (Exemplo 7.5)", {
  res <- razao_estima_media(n = 100, sum_x = 1200, sum_y = 1750, sum_x2 = 15620,
                             sum_y2 = 31650, sum_xy = 22059.35, N = 1000, X = 12500)
  expect_equal(res$estimativa$media, 18.23, tolerance = 1e-2)
  expect_equal(res$variancia$var_media, 0.048163636, tolerance = 1e-3)
})

test_that("razao_variancia_piloto + razao_tamanho_R reproduzem Exemplo 7.6", {
  piloto <- razao_variancia_piloto(x = x72, y = y72)
  expect_equal(piloto$estimativa$sr2, 7.7853, tolerance = 1e-3)

  res <- razao_tamanho_R(sr2 = piloto$estimativa$sr2, N = 1500, X_barra = 10, d = 0.05)
  expect_equal(res$estimativa$n, 111)
})

test_that("razao_tamanho_media reproduz Exemplo 7.7", {
  x77 <- c(23, 14, 20, 25, 12, 18, 30, 27, 8, 31)
  y77 <- c(25, 15, 22, 24, 11, 18, 31, 30, 11, 33)
  piloto <- razao_variancia_piloto(x = x77, y = y77)
  expect_equal(piloto$estimativa$sr2, 2.2462, tolerance = 1e-3)
  expect_equal(piloto$estimativa$r, 1.06, tolerance = 1e-2)

  res <- razao_tamanho_media(sr2 = piloto$estimativa$sr2, N = 1000, d = 0.75, arredondar = "proximo")
  expect_equal(res$estimativa$n, 15)
})

test_that("razao_vs_regressao_decidir ignora argumentos excedentes e recomenda corretamente", {
  res <- suppressWarnings(utils::capture.output(
    resultado <- razao_vs_regressao_decidir(x72, y72, X_barra = 10, N = 1500, variavel_extra = "abc")
  ))
  expect_equal(resultado$recomendado, "razao")
})
