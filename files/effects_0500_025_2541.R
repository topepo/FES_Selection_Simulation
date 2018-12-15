library(caret)
library(tidyverse)
library(earth)
library(keras)
library(glmnet)
library(xgboost)
library(ranger)
library(kknn)
library(kernlab)
library(Cubist)
library(ipred)
library(sessioninfo)

# ----------------------------------------------------------------

n <- 500
extra <- 25
cor_value <- 0
seed <- 2541

# ----------------------------------------------------------------

set.seed(seed)
trn <- SLC14_1(   n, corrVars = extra, corrValue = cor_value)
lrg <- SLC14_1(10^5, corrVars = extra, corrValue = cor_value)

# ----------------------------------------------------------------

ctrl  <- trainControl(method = "cv")
rctrl <- trainControl(method = "cv", search = "random")

# ----------------------------------------------------------------


mod2df <- function(obj, name) {
  res <- postResample(
    pred = predict(obj, newdata = lrg), 
    obs = lrg$y
  )
  test <- tibble(
    RMSE = res["RMSE"],
    Rsquared = res["Rsquared"],
    MAE = res["MAE"],
    time = obj$times$everything[3]/60,
    model = name
  )
  trn <- getTrainPerf(obj)
  trn <- tibble(
    RMSE_cv  = trn[,"TrainRMSE"],
    Rsquared_cv  = trn[,"TrainRsquared"],
    MAE_cv  = trn[,"TrainMAE"]
    )
  bind_cols(test, trn)
}

# not in caret at time of writing:
predictors.cubist <- function(x, ...)
  subset(x$usage, Conditions > 0 | Model > 0)$Variable


# Avoid counting variables in surrogate splits as real variables used
bagged_predictors <- function(x, surrogate = FALSE, ...) {
  code <- getModelInfo("rpart", regex = FALSE)[[1]]$predictors
  eachTree <- lapply(x$mtree,
                     function(u, surr) code(u$btree, surrogate = surr),
                     surr = FALSE)
  unique(unlist(eachTree))
}

vars2df <- function(obj, name) {
  pred_vars <- 
    if (obj$method == "cubist") {
      predictors.cubist(obj$finalModel) 
    } else {
        if (obj$method == "treebag") {
          bagged_predictors(obj$finalModel) 
        } else {
          predictors(obj)
        }
      } 
        
  tibble(
    Selected = pred_vars,
    model = rep(name, length(pred_vars))
  )
}


# ----------------------------------------------------------------

set.seed(890)
lm_fit <- train(y ~ . + Var05:Var06 + Var19:Var20, 
                data = trn, method = "lm",
                trControl = ctrl)
lm_res <- mod2df(lm_fit, "linear regression")

# ----------------------------------------------------------------

set.seed(890)
mars_fit <- train(y ~ ., data = trn, method = "gcvEarth",
                  tuneGrid = data.frame(degree = 1:2),
                  trControl = ctrl)
mars_res <- mod2df(mars_fit, "MARS")

# ----------------------------------------------------------------

set.seed(890)
svmr_fit <- train(y ~ ., data = trn, method = "svmRadial",
                  tuneLength = 50,
                  preProc = c("center", "scale"),
                  trControl = rctrl)
svmr_res <- mod2df(svmr_fit, "SVM (radial)")

# ----------------------------------------------------------------

set.seed(890)
mlp_fit <- train(y ~ ., data = trn, method = "mlpKerasDecay",
                 tuneLength = 50,
                 preProc = c("center", "scale"),
                 trControl = rctrl,
                 epochs = 100, 
                 verbose = 0)
mlp_res <- mod2df(mlp_fit, "neural network")
keras::backend()$clear_session()

# ----------------------------------------------------------------

knn_grid <- expand.grid(
  kmax = 1:20,
  distance = 2,
  kernel = c("rectangular", "triangular", "gaussian")
)

set.seed(890)
knn_fit <- train(y ~ ., data = trn, method = "kknn",
                 tuneGrid = knn_grid,
                 preProc = c("center", "scale"),
                 trControl = ctrl)
knn_res <- mod2df(knn_fit, "KNN")

# ----------------------------------------------------------------

cb_grid <- expand.grid(committees = c(1:9, (1:3)*10), 
                       neighbors = c(0, 1, 3, 5, 7, 9))

set.seed(890)
cb_fit <- train(y ~ ., data = trn, method = "cubist",
                tuneGrid = cb_grid,
                trControl = ctrl)
cb_res <- mod2df(cb_fit, "Cubist")

# ----------------------------------------------------------------

glmn_grid <- expand.grid(alpha = seq(0, 1, by = 0.05),
                         lambda = 2^(-2:2))
set.seed(890)
glmn_fit <- train(y ~ .+ Var05:Var06 + Var19:Var20, 
                  data = trn, method = "glmnet",
                  preProc = c("center", "scale"),
                  tuneGrid = glmn_grid,
                  trControl = ctrl)
glmn_res <- mod2df(glmn_fit, "glmnet")

# ----------------------------------------------------------------

xgb_grid <- 
  getModelInfo("xgbTree")[[1]]$grid(len = 50, search = "random") %>%
  mutate(colsample_bytree = 1)

set.seed(890)
xgb_fit <- train(y ~ ., data = trn, method = "xgbTree",
                 tuneGrid = xgb_grid,
                 trControl = rctrl)
xgb_res <- mod2df(xgb_fit, "boosted tree")

# ----------------------------------------------------------------

set.seed(890)
bag_fit <- train(y ~ ., data = trn, method = "treebag",
                 trControl = ctrl,
                 nbagg = 50)
bag_res <- mod2df(bag_fit, "bagged tree")

# ----------------------------------------------------------------

set.seed(890)
rf_fit <- train(y ~ ., data = trn, method = "ranger",
                tuneLength = 25,
                trControl = rctrl,
                num.trees = 1000,
                verbose = FALSE,
                num.threads = 1)
rf_res <- mod2df(rf_fit, "random forest")

# ----------------------------------------------------------------

all_res <- bind_rows(
  lm_res, glmn_res, 
  mars_res, svmr_res, knn_res, mlp_res,
  rf_res, xgb_res, cb_res, bag_res
) %>%
  mutate(
    extra_vars = extra,
    training_size = n,
    correlation = cor_value,
    seed = seed
  )

# ----------------------------------------------------------------

predictors <- bind_rows(
  vars2df(glmn_fit, "glmnet"),
  vars2df(cb_fit, "Cubist"),
  vars2df(mars_fit, "MARS"),
  vars2df(rf_fit, "random forest"),
  vars2df(xgb_fit, "boosted tree"),
  vars2df(bag_fit, "bagged tree")
) %>%
  mutate(
    extra_vars = extra,
    training_size = n,
    correlation = cor_value,
    seed = seed
  )

# ----------------------------------------------------------------

f_name <- paste0("effects_", n, "_", extra, "_", cor_value, 
                 "_", seed, ".RData")

save(all_res, predictors, file = paste0("../RData/", f_name))

# ----------------------------------------------------------------

print(session_info())

q("no")


