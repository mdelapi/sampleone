# CRAN submission comments (draft)

## Test environments
- [x] local: R 4.5.2 on Ubuntu 26.04.1 LTS -- 0 errors, 0 warnings, 1 note
      (`unable to verify current time`, an environment/clock artifact, not
      package-related)
- [ ] win-builder (devel and release) -- `devtools::check_win_devel()`
- [ ] R-hub (Windows, macOS, Linux) -- `rhub::rhub_check()`

## R CMD check results
0 errors | 0 warnings | 1 note (see above)

## Notes for reviewers
This is the first submission of `sampleone`.

The package implements standard survey-sampling formulas (simple random,
stratified, systematic, cluster, and ratio/regression estimators)
following Cochran (1977) and course notes cited in DESCRIPTION and in
each function's `@references`. All formulas are cross-validated against
worked numerical examples from the source material in the test suite
(`tests/testthat/`, 111 tests).

## Downstream dependencies
None (first release).

---

**Remaining steps before actual submission (internal checklist):**
1. [x] `devtools::document()` / `devtools::test()` / `devtools::check()`
       all clean.
2. [x] Real author names/emails filled in `DESCRIPTION`.
3. [ ] Push to GitHub (`https://github.com/mdelapi/sampleone`) and confirm
       the `R-CMD-check.yaml` Action passes on Windows/macOS/Linux.
4. [ ] Run `devtools::check_win_devel()` and/or `rhub::rhub_check()` for
       multi-platform confirmation.
5. [ ] Confirm the `sampleone` name is still available on CRAN at
       submission time (checked via web search during development; not a
       substitute for the official CRAN name-check at submission).
6. [ ] `devtools::release()` or manual submission via
       https://cran.r-project.org/submit.html
