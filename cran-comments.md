# CRAN submission comments

## Resubmission
This is a resubmission. In response to the review by Konstanze Lauseker:

* DESCRIPTION: removed the redundant "Provides functions for" from the
  start of the Description field.
* Console output: `razao_vs_regressao_decidir()` (R/cap07-tamanho-guarda-chuva.R)
  no longer writes to the console. It now returns an object of class
  `razao_vs_regressao` and the text summary is produced by a `print()`
  method. All other `cat()` calls are inside `print()`/`summary()` methods.
* .GlobalEnv: `sist_selecionar_amostra()` no longer saves/restores
  `.Random.seed` by hand in `.GlobalEnv`; the optional `semente` argument
  now uses `withr::with_seed()` (added `withr` to Imports). The package
  no longer touches `.GlobalEnv`.
* Also removed `LazyData: true` (the package has no `data/` directory).

## Test environments
- Local: R 4.5.2 on Ubuntu 26.04.1 LTS -- 0 errors, 0 warnings, 0 notes
- win-builder (R-devel) -- 0 errors, 0 warnings, 1 note (see below)
- GitHub Actions: ubuntu-latest (release, devel, oldrel-1), windows-latest
  (release), macos-latest (R 4.5) -- all passing
  (https://github.com/mdelapi/sampleone/actions)

## R CMD check results
0 errors | 0 warnings | 1 note

The note has two parts, both expected/non-actionable:

1. "New submission" -- this is the first release of this package.
2. "Possibly misspelled words in DESCRIPTION: Bolfarine, Bussab, Neyman" --
   these are author surnames (Bolfarine & Bussab, 2005) and a standard
   statistical term (Neyman allocation), not spelling errors.

## Notes for reviewers
This is the first submission of `sampleone`.

The package implements standard survey-sampling formulas (simple random,
stratified, systematic, cluster, and ratio/regression estimators)
following Cochran (1977) and course notes cited in DESCRIPTION and in
each function's `@references`. All formulas are cross-validated against
worked numerical examples from the source material in the test suite
(`tests/testthat/`, 136 expectations), including a blind validation against an
independent exercise list not used to derive the package's own tests
(see docs/VALIDACAO_LISTA01.md in the GitHub repository).

## Downstream dependencies
None (first release).
