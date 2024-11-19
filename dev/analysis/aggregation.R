# Nicoletti 2000
# State control domain example, p. 22

## Squared and normalized indicator weights (normal squared loading)
(f1_loadings <- c(0.79, 0.77, 0.76, 0.52, 0.18, -0.01))
(f1_sq <- f1_loadings^2)
(f1_weights <- f1_sq / sum(f1_sq))

(f2_loadings <- c(-0.01, 0.28, 0.05, 0.48, 0.84, 0.78))
(f2_sq <- f2_loadings^2)
(f2_weights <- f2_sq / sum(f2_sq))
# These are indicator weights within each factor


## Factor weights - prop or variance explained (normal SUM of sq loadings)
# Sum of squared factor loadings
(ssl <- c(sum(f1_sq), sum(f2_sq)))
(factor_weights <- map_dbl(ssl, ~ .x / (sum(ssl))))
# These are factor weights, two parts to be combined into state control domain


## Table 6, checking Australia example
subdomains <- c(0.81, 1.83)
domain_score <- sum(map2_dbl(subdomains, factor_weights, ~ .x * .y))
# weight each subdomain by factor weight to get domain score

# But how did they get subdomain scores?
# - Aggregate detailed indicators using weights


#' Prep
#' 1. Determine number of components 
#' 2. PCA on indicators
#' 3. Determine weights for indicators within each factor
#'      Normalized squared factor loadings
#' 4. Calculate factor weights
#'      Normalized sum of squared factor loadings

#' Aggregation
#' 1. For each unit (county, state), aggregate indicators into factor score by
#'      using indicator weights
#' 2. Then aggregate factors using factor weights to get final scores

