# Explore Frontier
# 2025-05-14


# Description -------------------------------------------------------------

# Figuring out which of our metrics are at county or state levels to inform
# the Frontiers submission

# Also poke around with some county level analyses


# Housekeeping ------------------------------------------------------------

options(scipen = 999)

pacman::p_load(
  dplyr,
  stringr,
  tidyr,
  glmnet,
  missRanger,
  purrr,
  broom,
  caret
)

# Load metrics
sm_data <- readRDS('data/sm_data.rds')
metrics <- sm_data$metrics
meta <- sm_data$metadata

# Load frame
frame <- readRDS('data/frameworks/new_frame.rds')

# Functions
source('dev/data_pipeline_functions.R')



# Check Scales ------------------------------------------------------------


get_str(metrics)
get_str(meta)
get_str(frame)

# Variable names
vars <- frame %>% 
  filter(variable_name != 'NONE') %>% 
  pull(variable_name)
vars
# 124



## By Meta -----------------------------------------------------------------


# Reduce meta to those variables
var_meta <- meta %>% 
  filter(variable_name %in% vars) %>% 
  select(variable_name, resolution)
get_str(var_meta)

# Get props
get_table(var_meta$resolution) %>% 
  prop.table()
# 40% at state level only
# but these are probably not updated lately



## By Metrics --------------------------------------------------------------


# Check with the actual metrics
get_str(metrics)

# Filter to vars, remove state fips codes
var_metrics <- metrics %>%
  filter(
    variable_name %in% vars
  )
get_str(var_metrics)
length(unique(var_metrics$variable_name))

# Check how many are left
out <- var_metrics %>% 
  filter(str_length(fips) == 5) %>% 
  pull(variable_name) %>% 
  unique() %>% 
  length()
out
out / length(unique(var_metrics$variable_name))
# 73 (58%) are at the county level. Better


## What do we ONLY have at the state level?
county_metrics <- var_metrics %>% 
  filter(str_length(fips) == 5) %>% 
  pull(variable_name) %>%
  unique()
state_metrics <- var_metrics %>% 
  filter(str_length(fips) == 2) %>% 
  pull(variable_name) %>%
  unique()
(state_only <- setdiff(state_metrics, county_metrics))
length(state_only)
# 51

# Back to frame for context
state_only_frame <- frame %>% 
  filter(variable_name %in% state_only)
state_only_frame

# Save it to csv to explore
write.csv(state_only_frame, 'temp/state_only_metrics.csv')


## Get the ones that are available at county level
get_str(frame)
county_df <- frame %>% 
  filter(
    !variable_name %in% state_only_frame$variable_name,
    variable_name != 'NONE'
  )
get_str(county_df)

# Save just the county level variables here for analyses
county_vars <- county_df$variable_name

# We actually want a DF with a column that species the difference
df <- frame %>% 
  filter(variable_name != 'NONE') %>% 
  mutate(has_county = case_when(
    variable_name %in% county_df$variable_name ~ 'x',
    .default = ''
  ))
get_str(df)

# Save this one to explore
write.csv(df, 'temp/county_vs_state.csv')



# Outcome Analyses --------------------------------------------------------
## Wrangle -----------------------------------------------------------------


# See what happens if we regress county level variables onto food insecurity
# and other outcome vars that might match SDG indicators
get_str(metrics)

outcome_vars <- c(
  'foodInsecurity',
  'communityEnvRank',
  'happinessScore',
  'wellbeingRank',
  'workEnvRank',
  'foodEnvironmentIndex',
  'lifeExpectancy',
  'population',
  'gdpCurrent'
)
outcome_vars
county_vars

# Get actual metrics data that at county level only
county_metrics <- metrics %>% 
  filter(
    variable_name %in% c(county_vars, outcome_vars),
    str_length(fips) == 5,
    variable_name != 'unemploymentRate'
  )
get_str(county_metrics)
# This is our county level dataset

# Make it dat
dat <- county_metrics

# Check number of metrics again
length(unique(dat$variable_name))

# Are we missing any
setdiff(unique(dat$variable_name), county_vars)
# Those are just our outcome variables - we have food insecurity and env index
# That will do for now

# Check years
get_table(dat$year)
# Thems all over

# Reduce to only latest year
dat <- get_latest_year(dat)
get_str(dat)

# Make it numeric, but first filter out -666666666
dat <- dat %>% 
  filter(value != -666666666) %>% 
  mutate(value = as.numeric(value))
get_str(dat)

# Make it wide
dat <- pivot_wider(
  dat,
  names_from = variable_name,
  values_from = value
)
get_str(dat)



## Imputation --------------------------------------------------------------


# Imputation
imp <- missRanger(dat[names(dat) != 'fips'])
get_str(imp)

# This is awkward but whatever. using imp without scaling in next section
out <- imp



## GLMnet ------------------------------------------------------------------


# Remove fips and some other variables that shouldn't be predictors
# Then normalize
save <- out %>% 
  select(-matches('population|fips|EnvironmentIndex'))
out <- save %>% 
  mutate(across(everything(), ~ scale(.x)))
get_str(out)

# Split data 70/30
set.seed(42)
indices <- createDataPartition(out$foodInsecurity_2024, p = 0.7, list = FALSE)
training_data <- out[indices, ]
testing_data <- out[-indices,]
my_folds <- createFolds(training_data$foodInsecurity_2024, k = 5, list = TRUE)

# Set up cv control
my_control <- trainControl(
  method = 'cv',
  number = 5,
  verboseIter = TRUE,
  index = my_folds
)

# run glmnet
set.seed(42)
glmnet <- train(
  foodInsecurity_2024 ~ .,
  data = out, 
  tuneGrid = expand.grid(
    alpha = seq(0.1, 1, length = 5),
    lambda = seq(0.0001, 0.1, length = 100)
  ),
  method = "glmnet",
  trControl = my_control
  # preProcess = c('zv', 'center', 'scale')
)

# Outputs
glmnet
get_str(glmnet)
glmnet$bestTune

# Predictions and performance
pred <- predict(glmnet, testing_data)
performance <- postResample(
  pred = pred, 
  obs = testing_data$foodInsecurity_2024
) %>% 
  round(3)
performance


# Variable importance
importance <- varImp(glmnet, scale = TRUE)
importance %>% 
  ggplot(aes(x = Overall, y = rownames(.))) +
  geom_col(
    color = 'royalblue',
    fill = 'lightblue'
  ) +
  theme_classic() 



## Jenky Regression? -------------------------------------------------------


summary(lm(foodInsecurity_2024 ~ ., data = save))



# Time Series -------------------------------------------------------------


# Using county metrics data but with all years
get_str(county_metrics)
unique(county_metrics$variable_name)
length(unique(county_metrics$variable_name))

# Get names of vars that have > 1 time point
series_vars <- county_metrics %>% 
  group_by(variable_name) %>% 
  summarize(n_year = length(unique(year))) %>% 
  filter(n_year > 1) %>% 
  pull(variable_name)
series_vars
length(series_vars)
length(series_vars) / 73
# 64 have more than 1 time point. That's actually not bad
# 88%

# Filter to just those variables so we can do regressions
series_metrics <- filter(metrics, variable_name %in% series_vars)
get_str(series_metrics)

# Side note - what is frequency of years
tab <- series_metrics %>% 
  group_by(variable_name) %>% 
  # summarize(years = paste0(unique(year), collapse = ','))
  mutate(year = as.numeric(year)) %>% 
  summarize(years = list(unique(year))) %>% 
  mutate(diffs = map_dbl(as.vector(years), ~ {
    unique(diff(.x))
  })) %>% 
  pull(diffs) %>% 
  get_table()
tab
prop.table(tab)
# 41, (64%) annual, 23 (36%) every 5 years


# For each one, run a regression with year as predictor
res <- map(series_vars, ~ {
  # browser()
  tryCatch(
    {
      df <- series_metrics %>% 
        filter(variable_name == .x) %>% 
        mutate(
          value = as.numeric(value),
          year = as.numeric(year)
        )
      model <- lm(value ~ year, data = df)
    },
    error = function(e) {
      message(paste('Error with var', .x))
    }
  )
}) %>% 
  setNames(c(series_vars)) %>% 
  discard(\(x) is.null(x))

get_str(res)
res[[1]]

# Put together one table of results from all models, add stars
res_df <- imap(res, ~ {
  .x %>% 
    tidy() %>% 
    filter(term == 'year') %>% 
    mutate(term = .y)
}) %>% 
  bind_rows() %>% 
  mutate(sig = ifelse(p.value < 0.05, '*', ''))
res_df

# How many metrics
nrow(res_df)
# 63

# How many are significant
mean(res_df$sig == '*')
# 38% (24 of them)

# Bring in directional values. If not reverse, then positive is good
reverse <- c(
  'unemploymentRate',
  'gini',
  'lowBirthweight',
  'teenBirths',
  'uninsured',
  'incomeInequality',
  'childrenInSingleParentHouseholds',
  'injuryDeaths',
  'airPollutionParticulateMatter',
  'drinkingWaterViolations',
  'severeHousingProblems',
  'prematureAgeAdjustedMortality',
  'infantMortality',
  'frequentPhysicalDistress',
  'frequentMentalDistress',
  'diabetesPrevalence',
  'hivPrevalence',
  'limitedAccessToHealthyFoods',
  'drugOverdoseDeaths',
  'disconnectedYouth',
  'residentialSegregationBlackWhite',
  'suicides',
  'motorVehicleCrashDeaths',
  'severeHousingCostBurden',
  'schoolSegregation',
  'childCareCostBurden',
  'wicPercEligible',
  'droughtMeanPercArea',
  'pctAtRiskAnimalSpp',
  'pctAtRiskPlantSpp',
  'pctAtRiskBeeSpp',
  'pctAtRiskOrchidSpp',
  'pctAtRiskEcosystems',
  'expChemicalPct',
  'ageProducers',
  'waterIrrSrcOffFarmExp',
  'waterIrrSrcOffFarmExpPerAcreFt',
  'CH4FromAg',
  'N2OFromAg',
  'CO2FromAg',
  'propAreaFsaSecDisasters',
  'totalCapConsNoDwellings',
  'totalIntExpRealEstateNoDwellings',
  'totalIncomeInsuranceIndemnities',
  'totalIncomeInsuranceIndemnitiesFederal',
  'totalValueEmergPayments',
  'totalValueOtherAdHocEmergPayments',
  'totalValueDairyMarginProtPayments',
  'totalValueAllLossCoveragePayments',
  'totalValueAgRiskCoveragePayments',
  'totalCapExpBldgsLandNoDwellings',
  'alcoholImpairedDrivingDeaths' 
)

# Of those that are significant, what are trends
sig_models <- res_df %>% 
  filter(sig == '*') %>% 
  mutate(trend = case_when(
    estimate > 0 ~ 'increasing',
    .default = 'decreasing'
  ))
sig_models
mean(sig_models$trend == 'increasing')

# But is this good or bad - bring in directional values
sig_models <- sig_models %>% 
  mutate(
    direction = ifelse(term %in% reverse, 'reverse', 'asis'),
    outcome = case_when(
      direction == 'asis' & trend == 'increasing' ~ 'better',
      direction == 'asis' & trend == 'decreasing' ~ 'worse',
      direction == 'reverse' & trend == 'increasing' ~ 'worse',
      direction == 'reverse' & trend == 'decreasing' ~ 'better',
      .default = NA
    )
  )
sig_models

# proportion getting better or worse
(tab <- get_table(sig_models$direction))
prop.table(tab)
# Half getting better, half getting worse


