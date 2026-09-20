test_that("pop_media reproduz Exemplo 2.1 da apostila", {
  # Populacao de 5 pacientes, variavel idade
  idade <- c(44, 45, 48, 42, 46)
  res <- pop_media(idade)
  expect_equal(res$estimativa$media, 45, tolerance = 1e-6)
  expect_equal(res$variancia$sigma2, 4, tolerance = 1e-6)
})

test_that("pop_total soma corretamente", {
  res <- pop_total(c(1, 2, 3, 4, 5))
  expect_equal(res$estimativa$total, 15)
})

test_that("pop_proporcao calcula P e Var(P) corretamente", {
  res <- pop_proporcao(c(1, 0, 0, 1, 1))
  expect_equal(res$estimativa$P, 0.6, tolerance = 1e-6)
  expect_equal(res$variancia$var_P, 0.6 * 0.4, tolerance = 1e-6)
})

test_that("amostra_estimadores reproduz Exemplo 3.2 (media e variancia amostral)", {
  # Populacao de 3 domicilios (rendas): usada tambem como amostra neste teste unitario
  res <- amostra_estimadores(c(12, 30, 18))
  expect_equal(res$estimativa$media_amostral, 20, tolerance = 1e-6)
  # s2 com divisor n-1 = 2: soma dos quadrados dos desvios = 64+100+4=168; 168/2=84
  expect_equal(res$variancia$s2, 84, tolerance = 1e-6)
})

test_that("amostra_estimadores identifica dados binarios e calcula p_hat", {
  res <- amostra_estimadores(c(1, 0, 1, 1, 0))
  expect_equal(res$estimativa$p_hat, 0.6, tolerance = 1e-6)
})
