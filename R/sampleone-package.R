#' sampleone: Survey Sampling Methods for Teaching and Practice
#'
#' Functions for planning, selecting, and estimating from surveys under
#' simple random sampling (with/without replacement), stratified
#' sampling, systematic sampling, one- and two-stage cluster sampling,
#' and ratio/regression estimators. Developed to accompany the "Nocoes
#' de Amostragem" course at DEs-ICET-UFMT (Prof. Dr. Mariano Martinez
#' Espinosa).
#'
#' See `vignette("catalogo-formulas", package = "sampleone")` for a full
#' map from course-notes equations to package functions.
#'
#' @section Function families:
#' \itemize{
#'   \item `pop_*()`, `amostra_*()` -- Cap. 2, basic population/sample concepts.
#'   \item `aas_*()` -- Cap. 3, simple random sampling.
#'   \item `estr_*()` -- Cap. 4, stratified sampling.
#'   \item `sist_*()` -- Cap. 5, systematic sampling.
#'   \item `cong1_*()`, `cong2_*()`, `cong_estima()` -- Cap. 6, cluster sampling.
#'   \item `reg_*()`, `razao_*()` -- Cap. 7, regression and ratio estimators.
#'   \item `amostragem_*()` -- cross-cutting utilities (response-rate
#'     adjustment, comparison of sampling plans).
#' }
#'
#' Every function returns an object of class `sampling_result` with
#' `print()`/`summary()` methods -- see `?print.sampling_result`.
#'
#' @keywords internal
"_PACKAGE"
