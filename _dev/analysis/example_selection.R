#' Supplemental Geographic Analysis
#' 2024-06-05
#' 
#' Trying out some GLMnet and ML techniques to get at the geography question



# Housekeeping ------------------------------------------------------------


pacman::p_load(dplyr,
               readr,
               stringr,
               caret,
               purrr,
               DHARMa,
               lmtest)


raw <- readRDS('ewe/2_cleanData/sem_dat.rds')$unscaled$sem_dat_locs %>% 
  mutate(category_binary = factor(category_revised,
                                  levels = c('true_report', 'false_report')))

survey_2a <- readRDS('ewe/2_cleanData/surveys_2a_2b_val_clean.rds')$survey_2a


# Get demographics alongside the semdat df
dat <- survey_2a %>% 
  select(prolificID:election, BIPOC) %>% 
  right_join(raw, by = 'prolificID') %>%
  select(-contains('.att'), -prolificID, -split, -rebl_3pla) %>% 
  mutate(across(c(age:state), as.factor)) %>%
  as.data.frame() %>% 
  na.omit()
get_str(dat)



# Split -------------------------------------------------------------------


# Split 80/20
set.seed(42)
indices <- createDataPartition(dat$rebl_tpm, p = 0.80, list = FALSE)
training_data <- dat[indices, ]
testing_data <- dat[-indices,]



# Preprocess, Control --------------------------------------------------


# Set folds so all models use them the same
set.seed(42)
my_folds <- createFolds(training_data$rebl_tpm, k = 5, list = TRUE)

# Control
my_control <- trainControl(
  method = 'cv',
  number = 5,
  verboseIter = TRUE,
  index = my_folds
)



# Explore NZV -------------------------------------------------------------


nearZeroVar(dat,
            names = TRUE,
            saveMetrics = TRUE)
get_table(dat$race)
get_table(dat$rurality)
get_table(dat$election)




# LM ----------------------------------------------------------------------


model <- lm(rebl_tpm ~ eewe1 + eewe3 + eewe5 + politics + BIPOC + education + 
              gender + att3 + att4 + att5 + pd1 + pd2 + pd5 + region_sub + 
              income,
            data = training_data)
summary(model)

simulateResiduals(model, plot = TRUE) # passes all
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))
bptest(model) # passes Breusch Pagan test of heteroskedasticity 
# This actually looks surprisingly good


p <- predict(model, testing_data)
postResample(pred = p, obs = testing_data$rebl_tpm)

set_flextable_defaults(digits = 3)
lm_out <- model %>% 
  as_flextable() %>% 
  theme_apa() %>% 
  colformat_double() %>% 
  autofit(
    add_w = 0,
    part = 'all',
    hspans = "none"
  )
save_as_docx(
  lm_out,
  path = 'ewe/6_outputs/throes/defense/regressions.docx')



# Predict Region ----------------------------------------------------------
## LM ---------------------------------------------------------------------


p_load(nnet, caTools)
m2 <- multinom(
  region ~ eewe1 + eewe3 + eewe5 + politics + BIPOC + education +
    gender + att3 + att4 + att5 + pd1 + pd2 + pd5 + income + rebl_tpm,
  data = training_data,
  family = 'binomial'
)
summary(m2)

# Make predictions on test_data
p <- predict(m2, testing_data, type = "class")

confusionMatrix(p, testing_data$region)
# 44% accuracy


# Predict Category --------------------------------------------------------


m3 <- glm(
  category_binary ~ eewe1 + eewe3 + eewe5 + politics + BIPOC + education +
    gender + att3 + att4 + att5 + pd1 + pd2 + pd5 + income + rebl_tpm + region_sub,
  data = training_data,
  family = 'binomial'
)
summary(m3)

p <- predict(m3, testing_data, type = "response")
colAUC(p, testing_data$category_binary, plotROC = TRUE)



## Random Forest -----------------------------------------------------------


set.seed(42)
model_mf <- train(
  category_binary ~ eewe1 + eewe3 + eewe5 + politics + BIPOC + education +
    gender + att3 + att4 + att5 + pd1 + pd2 + pd5 + income + rebl_tpm + region,
  data = training_data,
  tuneLength = 7,
  method = "ranger",
  trControl = my_control,
  importance = 'impurity'
)

model_mf
plot(model_mf)

importance <- varImp(model_mf, scale = TRUE)
importance
plot(importance)
# Check ?importance in missForest for explanation

# Predict
p <- predict(model_mf, testing_data)
confusionMatrix(p, testing_data$category_binary)



## Compare -----------------------------------------------------------------

bwplot(resamples, metric = "ROC")
xyplot(resamples, metric = "ROC")
dotplot(resamps, metric = "ROC") # best for many models
densityplot(resamps, metric = "ROC")



# CV Linear ------------------------------------------------------------------


# Stable
model_lm <- train(
  rebl_tpm ~ eewe1 + eewe3 + eewe5 + politics + BIPOC + education + gender + 
    att3 + att4 + att5 + pd1 + pd2 + pd5 + region_sub + income,
  data = training_data, 
  method = "lm",
  trControl = my_control,
  preProcess = c('zv', 'center', 'scale')
)

model_lm
summary(model_lm)

importance <- varImp(model_lm, scale = TRUE)
importance
plot(importance)

p <- predict(model_lm, testing_data)
postResample(pred = p, obs = testing_data$rebl_tpm)
# 0.595

# This is the same as a regular LM though



# MF ----------------------------------------------------------------------


set.seed(42) # unstable
model_mf <- train(
  rebl_tpm ~ eewe1 + eewe3 + eewe5 + politics + BIPOC + education + gender + 
    att3 + att4 + att5 + pd1 + pd2 + pd5 + region_sub + income,
  data = training_data, 
  # tuneLength = 7,
  method = "ranger",
  trControl = my_control,
  importance = 'impurity'
)

model_mf
plot(model_mf)

importance <- varImp(model_mf, scale = TRUE)
importance
plot(importance)
# Check ?importance in missForest for explanation

hist(dat$rebl_tpm, breaks = 25)


# Predict
p <- predict(model_mf, testing_data)
postResample(pred = p, obs = testing_data$rebl_tpm)
# 0.606



# GLMnet ------------------------------------------------------------------


#' Ridge: alpha = 0
#'  Penalizes magnitude of coefficients, proportional to the sum of squared
#' coefficients. Coefficients approach 0 when penalty term increases, lowering
#' the model variance. Does not erase coefficients.

#' Lasso: alpha = 1
#'  Penalty based on total absolute value of coefficients. Can reduce some
#' coefficients to zero, removing them from model. Good with very redundant
#' variables. 

set.seed(42)
model_glmnet <- train(
  rebl_tpm ~ eewe1 + eewe3 + eewe5 + politics + BIPOC + education + gender + 
    att3 + att4 + att5 + pd1 + pd2 + pd5 + region_sub + income,
  data = training_data, 
  # tuneGrid = expand.grid(
  #   alpha = seq(0.1, 1, length = 5),
  #   lambda = seq(0.0001, 0.1, length = 100)
  # ),
  method = "glmnet",
  trControl = my_control,
  preProcess = c('zv', 'center', 'scale')
)

print(model_glmnet)
plot(model_glmnet)
plot(model_glmnet$finalModel)

# Coefficients
coef <- coef(model_glmnet$finalModel, model_glmnet$finalModel$lambdaOpt)
coef
coefs <- data.frame(
  'variable' = row.names(coef),
  'coefficient' = as.vector(coef)
) %>%
  slice(-1) %>%
  arrange(1, desc(coefficient))
coefs
  

importance <- varImp(model_glmnet, scale = TRUE)
importance
plot(importance)

# Predict
p <- predict(model_glmnet, testing_data)
postResample(pred = p, obs = testing_data$rebl_tpm)
# 0.583 with defaults



# Compare -----------------------------------------------------------------


# Create model_list
model_list <- list(
  lm = model_lm,
  glm = model_glmnet,
  mf = model_mf
)

map(model_list, summary)

# Summarize the results
summary(resamples(model_list))

# And for predicting the test set
map(model_list, ~ {
  p <- predict(.x, testing_data)
  postResample(pred = p, obs = testing_data$rebl_tpm)
})


