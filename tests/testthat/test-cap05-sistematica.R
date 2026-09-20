test_that("sist_intervalo identifica k inteiro (Exemplo 5.1)", {
  res <- sist_intervalo(N = 2000, n = 80)
  expect_true(res$estimativa$k_inteiro)
  expect_equal(res$estimativa$k, 25)
})

test_that("sist_selecionar_amostra reproduz Exemplo 5.1 (k inteiro)", {
  res <- sist_selecionar_amostra(N = 2000, n = 80, r = 8)
  expect_equal(res$estimativa$n_selecionado, 80)
  expect_equal(res$estimativa$elementos[1:5], c(8, 33, 58, 83, 108))
  expect_equal(utils::tail(res$estimativa$elementos, 1), 1983)
})

test_that("sist_intervalo identifica k nao inteiro e calcula k* (Exemplo 5.2)", {
  res <- sist_intervalo(N = 2000, n = 75)
  expect_false(res$estimativa$k_inteiro)
  expect_equal(res$estimativa$k, 26)
  expect_equal(res$estimativa$k_estrela, 50)
})

test_that("sist_selecionar_amostra reproduz Exemplo 5.2 (k nao inteiro, com correcao)", {
  res <- sist_selecionar_amostra(N = 2000, n = 75, r = 26)
  expect_equal(res$estimativa$n_selecionado, 75)
  expect_equal(res$estimativa$elementos[1:5], c(26, 102, 128, 154, 180))
  expect_equal(utils::tail(res$estimativa$elementos, 1), 2000)
})

test_that("sist_selecionar_amostra sorteia r automaticamente sem alterar o RNG global", {
  set.seed(123)
  antes <- .Random.seed
  res <- sist_selecionar_amostra(N = 100, n = 10, semente = 999)
  depois <- .Random.seed
  expect_true(res$estimativa$r >= 1 && res$estimativa$r <= res$estimativa$k)
  expect_equal(antes, depois) # RNG global restaurado
})
