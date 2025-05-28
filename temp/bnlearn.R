# Bnlearn
# 2025-05-22


# Description -------------------------------------------------------------

# Poking around with bnlearn examples
# https://www.bnlearn.com/examples/


# Housekeeping ------------------------------------------------------------


if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr,
  bnlearn,
  
)

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install()
BiocManager::install(c("graph", "Rgraphviz"))



# Creating Network Structure ----------------------------------------------


# Empty graphk
e = empty.graph(LETTERS[1:6])
class(e)

# Complete network
graph <- complete.graph(LETTERS[1:6])
graphviz.compare(graph, complete.graph(LETTERS[6:1]))


# Arc set, then assign set to bn object
arc.set = matrix(
  c("A", "C", "B", "F", "C", "F"),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(NULL, c("from", "to"))
)
arcs(e) = arc.set
e

# specific formula
model2network("[A][C][B|A][D|C][F|A:B:C][E|F]")
# Don't have to make empty graph first

# Sample uniform probability from space of graphs with Ide and Cozman sampler
random.graph(LETTERS[1:6], num = 2, method = "ic-dag")
# Multiple graphs is more efficient than singles

# Sampling from DAG with uniform prob
random.graph(LETTERS[1:6], method = "melancon")
# MCMC samples with uniform prob from space of DAG 



# Custom Fitted Bayesian Networks -----------------------------------------


# Data driven, expert driven, hybrid approach (combine both)


## Discrete ----------------------------------------------------------------

cptA = matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("LOW", "HIGH")))
cptB = matrix(c(0.8, 0.2), ncol = 2, dimnames = list(NULL, c("GOOD", "BAD")))
cptC = c(0.5, 0.5, 0.4, 0.6, 0.3, 0.7, 0.2, 0.8)
dim(cptC) = c(2, 2, 2)
dimnames(cptC) = list(
  "C" = c("TRUE", "FALSE"),
  "A" =  c("LOW", "HIGH"),
  "B" = c("GOOD", "BAD")
)

net = model2network("[A][B][C|A:B]")
dfit = custom.fit(net, dist = list(A = cptA, B = cptB, C = cptC))
dfit

# Add ordinal
dfit = custom.fit(net,
                  dist = list(A = cptA, B = cptB, C = cptC),
                  ordinal = c("A", "B"))
dfit


## Continuous Networks -----------------------------------------------------


distA = list(coef = c("(Intercept)" = 2), sd = 1)
distB = list(coef = c("(Intercept)" = 1), sd = 1.5)
distC = list(coef = c("(Intercept)" = 0.5, "A" = 0.75, "B" = 1.32), sd = 0.4)
cfit = custom.fit(net, dist = list(A = distA, B = distB, C = distC))
cfit

# Can add fitted values and residfuals for each node
distA = list(coef = c("(Intercept)" = 2), fitted = 1:10, resid = rnorm(10))
distB = list(coef = c("(Intercept)" = 1), fitted = 3:12, resid = rnorm(10))
distC = list(coef = c("(Intercept)" = 0.5, "A" = 0.75, "B" = 1.32), fitted = 2:11, resid = rnorm(10))
cfit = custom.fit(net, dist = list(A = distA, B = distB, C = distC))
cfit

# Or lm() to produce parameters. Good for missing data or with weights
distA = lm(A ~ 1, data = gaussian.test)
distB = lm(B ~ 1, data = gaussian.test)
distC = lm(C ~ A + B, data = gaussian.test)
cfit = custom.fit(net, dist = list(A = distA, B = distB, C = distC))
cfit

# penalized() for ridge and elastic nets as local dists instead of OLS
# No LASSO - does not make sense here, must have non zero coefficients
pacman::p_load(penalized)
distC = penalized(
  C ~ A + B,
  data = gaussian.test,
  lambda1 = 0,
  lambda2 = 0.5,
  trace = FALSE
)
cfit = custom.fit(net, dist = list(A = distA, B = distB, C = distC))
cfit



## Hybrid ------------------------------------------------------------------




# SM Data -----------------------------------------------------------------


# Try it on our data I guess
# Latest years only I suppose. Or maybe not?
getcwd()
