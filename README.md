# sampleone

<!-- badges: start -->
[![R-CMD-check](https://github.com/mdelapi/sampleone/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mdelapi/sampleone/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`sampleone` provides R functions for planning, selecting, and estimating
from surveys under the main probability sampling designs taught in
introductory sampling courses: simple random sampling (with and without
replacement), stratified sampling (proportional, Neyman, and cost-based
allocation), systematic sampling, one- and two-stage cluster sampling, and
ratio/regression estimators.

Developed by Miguel Luiz de Almeida Pinto to accompany the "Noções de
Amostragem" course at DEs-ICET-UFMT (Prof. Dr. Mariano Martínez Espinosa,
co-author of the package as author of the source course material).

## Status

✅ **`R CMD check --as-cran`: 0 errors, 0 warnings, 0 relevant notes.**
✅ 111 testthat tests passing, reproducing worked numerical examples from
the source course notes.
✅ Formulas catalog complete (all 6 technical chapters) and cross-validated.
✅ Core implementation complete: Cap. 2-7, 45 exported functions.

- [x] Cap. 2 -- Conceitos base (`pop_*()`, `amostra_estimadores()`)
- [x] Cap. 3 -- Amostragem Aleatória Simples (`aas_*()`)
- [x] Cap. 4 -- Amostragem Estratificada (`estr_*()`, `amostragem_ajustar_resposta()`)
- [x] Cap. 5 -- Amostragem Sistemática (`sist_*()`)
- [x] Cap. 6 -- Conglomerados (`cong1_*()`, `cong2_*()`, `cong_estima()`)
- [x] Cap. 7 -- Razão e Regressão (`razao_*()`, `reg_*()`)
- [x] Função transversal `amostragem_comparar_planos()`
- [x] Vinheta "catalogo-formulas" (`vignettes/catalogo-formulas.Rmd`)
- [x] `R CMD check --as-cran` local (0 errors, 0 warnings)
- [ ] Cobertura de testes com `covr`
- [ ] Publicação no GitHub
- [ ] Submissão ao CRAN

## Installation (development version)

```r
# install.packages("devtools")
devtools::install_github("mdelapi/sampleone")
```

## Example

```r
library(sampleone)

# Tamanho de amostra para estimar uma media, populacao finita conhecida
aas_tamanho_media(sigma2 = 1240, d = 7, conf = 0.95, N = 10000)

# Alocacao otima de Neyman em amostragem estratificada
estr_alocacao_neyman(n = 261, Nk = c(1500, 2700, 3300), pk = c(0.10, 0.20, 0.40))
```

## License

MIT © Miguel Luiz de Almeida Pinto. Course content basis: Prof. Dr.
Mariano Martínez Espinosa (DEs-ICET-UFMT), used with attribution -- see
`inst/CITATION`.
