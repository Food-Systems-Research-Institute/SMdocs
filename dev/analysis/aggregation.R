# Nicoletti 2000
# State control domain example, p. 22

## Squared and normalized indicator weights (normal squared loading)
(f1_loadings <- c(0.79, 0.77, 0.76, 0.52, 0.18, -0.01))
(f1_sq <- f1_loadings^2)
(f1_weights <- f1_sq / sum(f1_sq))

(f2_loadings <- c(-0.01, 0.28, 0.05, 0.48, 0.84, 0.78))
(f2_sq <- f2_loadings^2)
(f2_weights <- f2_sq / sum(f2_sq))
# These are indicator weights within each component


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





# -------------------------------------------------------------------------


# Example data
set.seed(123)
df <- data.frame(
  Variable1 = rnorm(100),
  Variable2 = rnorm(100, mean = 1),
  Variable3 = rnorm(100, mean = 2)
)
df

# Perform PCA
(pca_result <- prcomp(df, scale. = TRUE))

# Extract PCA loadings (weights for the first principal component)
(loadings <- pca_result$rotation[, 1])

# Normalize the weights to sum to 1
(normalized_weights <- abs(loadings) / sum(abs(loadings)))

# Compute the weighted average index score
(index_scores <- as.matrix(df) %*% normalized_weights)

# View the scores
head(index_scores)




# -------------------------------------------------------------------------



set.seed(42)
df <- data.frame(
  Factor1_Var1 = rnorm(100),
  Factor1_Var2 = rnorm(100, mean = 2),
  Factor2_Var1 = rnorm(100, mean = -1),
  Factor2_Var2 = rnorm(100, mean = 1)
)

# Factor 1 PCA
factor1_data <- df[, c("Factor1_Var1", "Factor1_Var2")]
factor1_pca <- prcomp(factor1_data, scale. = TRUE)
(factor1_weights <- factor1_pca$rotation[, 1] / sum(abs(factor1_pca$rotation[, 1])))
(factor1_index <- as.matrix(factor1_data) %*% factor1_weights)

# Factor 2 PCA
factor2_data <- df[, c("Factor2_Var1", "Factor2_Var2")]
factor2_pca <- prcomp(factor2_data, scale. = TRUE)
factor2_weights <- factor2_pca$rotation[, 1] / sum(abs(factor2_pca$rotation[, 1]))
(factor2_index <- as.matrix(factor2_data) %*% factor2_weights)



# -------------------------------------------------------------------------


(mat <- matrix(c(1:9), nrow = 3, ncol = 3))
(mult <- 1:3)

mat %*% mult
# This does sum and add at once - getting broad index score