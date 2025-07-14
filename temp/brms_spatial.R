pacman::p_load(
  sf,
  spdep,
  brms
)

# neighborhood structure
nb <- poly2nb(your_sf_object)
W <- nb2mat(nb, style = "W")  # row-standardized

# Or use adjacency matrix for CAR
adj <- nb2mat(nb, style = "B")  # binary style

# CAR structure: BYM2 for spatial + non-spatial random effects
# Conditional auto regressive
formula <- brmsformula(
  outcome ~ 1 + covariates + (1 | state) + (1 | county) + 
    car(county_id, gr = state, adj = adj, type = "bym2")
)

fit <- brm(
  formula,
  data = dat,
  family = gaussian(),
  data2 = list(adj = adj),
  chains = 4, cores = 4
)
