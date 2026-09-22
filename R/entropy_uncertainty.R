# ===========================================================================
# Elicitation uncertainty (Beta-distributed CPT parameters)
# ===========================================================================

#' Map a 0-100 "how certain are you?" slider value to a Beta concentration
#'
#' Interpolates geometrically between \code{kappa_min} (very uncertain) and
#' \code{kappa_max} (very certain), so that equal slider movements feel like
#' roughly equal *relative* changes in confidence rather than equal absolute
#' changes in concentration.
#'
#' @param certainty Numeric in \code{[0, 100]}.
#' @param kappa_min Numeric > 0. Beta concentration (a + b) at certainty = 0.
#' @param kappa_max Numeric > kappa_min. Concentration at certainty = 100.
#' @return Numeric. Beta concentration.
#' @keywords internal
.certainty_to_kappa <- function(certainty, kappa_min = 2, kappa_max = 200) {
  certainty <- pmax(0, pmin(100, certainty))
  kappa_min * (kappa_max / kappa_min) ^ (certainty / 100)
}


#' Convert a point probability + certainty into Beta(a, b) parameters
#'
#' @param p Numeric in (0, 1). Elicited probability; becomes the Beta mean.
#' @param certainty Numeric in \code{[0, 100]}.
#' @return List with elements \code{a} and \code{b}.
#' @keywords internal
.beta_params <- function(p, certainty, kappa_min = 2, kappa_max = 200) {
  p     <- pmax(1e-6, pmin(1 - 1e-6, p))
  kappa <- .certainty_to_kappa(certainty, kappa_min, kappa_max)
  list(a = p * kappa, b = (1 - p) * kappa)
}


#' Draw one Monte Carlo sample of a full CPT set from elicited (p, certainty)
#'
#' @param cpts Named list of CPT data frames (as built by the elicitation
#'   server), each with a \code{prob} column giving P(node = 1 | combo).
#' @param certainty Named numeric vector/list, node -> certainty in
#'   \code{[0, 100]}. Nodes missing from \code{certainty} default to 100
#'   (fully certain), so they still get resampled from a very tight Beta
#'   rather than left fixed -- consistent with, but negligibly different
#'   from, the elicited point value.
#' @return A new named list of CPTs with \code{prob} replaced by one draw.
#' @keywords internal
.sample_cpts <- function(cpts, certainty, kappa_min = 2, kappa_max = 200) {
  sampled <- lapply(names(cpts), function(node) {
    cpt      <- cpts[[node]]
    cert     <- certainty[[node]] %||% 100
    pars     <- .beta_params(cpt$prob, cert, kappa_min, kappa_max)
    cpt$prob <- stats::rbeta(length(cpt$prob), pars$a, pars$b)
    cpt
  })
  stats::setNames(sampled, names(cpts))
}


#' Monte Carlo entropy under elicitation uncertainty
#'
#' Repeatedly samples full CPTs from Beta distributions centred on the
#' elicited probabilities (see \code{\link{.sample_cpts}}), recomputes the
#' joint distribution and Shannon entropy for each draw via the existing
#' \code{compute_joint_distribution()} / \code{shannon_entropy()}, and
#' summarises the resulting spread.
#'
#' This indirection is necessary: the Beta-Bernoulli predictive mean equals
#' \code{p} regardless of concentration, so plugging point probabilities into
#' \code{shannon_entropy()} once would never reflect confidence. Only
#' propagating sampled parameters through the full joint distribution makes
#' the certainty slider affect the reported entropy.
#'
#' @param nodes Character vector. Node names in topological order.
#' @param cpts Named list of CPTs (point estimates).
#' @param certainty Named numeric vector/list, node -> certainty in
#'   \code{[0, 100]}.
#' @param n_sims Integer. Number of Monte Carlo draws.
#' @param base Numeric. Logarithm base passed to \code{shannon_entropy()}.
#' @return List with \code{draws} (numeric vector of length \code{n_sims})
#'   and \code{summary} (named numeric vector: mean, sd, lower/upper 95%
#'   quantiles).
#' @keywords internal
simulate_entropy_uncertainty <- function(nodes, cpts, certainty,
                                          n_sims = 500, kappa_min = 2,
                                          kappa_max = 200, base = 1.01) {
  draws <- vapply(seq_len(n_sims), function(i) {
    sampled <- .sample_cpts(cpts, certainty, kappa_min, kappa_max)
    joint   <- compute_joint_distribution(nodes, sampled)
    shannon_entropy(joint$prob, base)
  }, numeric(1L))

  list(
    draws   = draws,
    summary = c(
      mean  = mean(draws),
      sd    = stats::sd(draws),
      lower = stats::quantile(draws, 0.025, names = FALSE),
      upper = stats::quantile(draws, 0.975, names = FALSE)
    )
  )
}
