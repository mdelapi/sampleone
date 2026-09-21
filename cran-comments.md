# CRAN submission comments

## Test environments
- Local: R 4.5.2 on Ubuntu 26.04.1 LTS -- 0 errors, 0 warnings, 1 note
  (`unable to verify current time`, an environment/clock artifact)
- win-builder (R-devel) -- 0 errors, 0 warnings, 1 note (see below)
- GitHub Actions: ubuntu-latest (release, devel, oldrel-1), windows-latest
  (release), macos-latest (R 4.5) -- all passing
  (https://github.com/mdelapi/sampleone/actions)

## R CMD check results
0 errors | 0 warnings | 1 note

The note has three parts, all expected/non-actionable:

1. "New submission" -- this is the first release of this package.
2. "Possibly misspelled words in DESCRIPTION: Bolfarine, Bussab, Neyman" --
   these are author surnames (Bolfarine & Bussab, 2005) and a standard
   statistical term (Neyman allocation), not spelling errors.
3. ~~"Invalid file URIs" for docs/*.md links in README.md~~ -- fixed by
   switching those links to absolute GitHub URLs, since the `docs/`
   folder (developer-facing reference material) is intentionally excluded
   from the built package via `.Rbuildignore`.

## Notes for reviewers
This is the first submission of `sampleone`.

The package implements standard survey-sampling formulas (simple random,
stratified, systematic, cluster, and ratio/regression estimators)
following Cochran (1977) and course notes cited in DESCRIPTION and in
each function's `@references`. All formulas are cross-validated against
worked numerical examples from the source material in the test suite
(`tests/testthat/`, 133 tests), including a blind validation against an
independent exercise list not used to derive the package's own tests
(see docs/VALIDACAO_LISTA01.md in the GitHub repository).

## Downstream dependencies
None (first release).
