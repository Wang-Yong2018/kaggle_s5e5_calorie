# Using fast tidymodels tech , it will help you to speed up ml speed from hour to minutes.

# setup
library(tidymodels)
library(future)
library(finetune)
library(bonsai)
set.seed(1)
d <- sim_classification(1e5)
set.seed(1)
d_split <- initial_split(d)
d_train <- training(d_split)
d_test <- testing(d_split)
d_folds <- vfold_cv(d_train)

# a first go(very slow)
bt <-
  boost_tree(learn_rate = tune(), trees = tune())|>
  set_mode("classification")|>
  set_engine("lightgbm")
set.seed(1)

# bm_basic <-
#   bench::mark(
#     basic =
#       tune_grid(
#         object = bt,
#         preprocessor = class ~ .,
#         resamples = d_folds,
#         grid = 12
#       )
#   )
# bm_basic


# A speedy go
plan(multisession, workers = 12)
set.seed(1)
bt_grid <- bt %>%
  extract_parameter_set_dials() %>%
  grid_regular(levels = 4)

bt_lgb <- bt %>% set_engine("lightgbm")


set.seed(1)

bm_speedy <-
  bench::mark(
    speedy =
      tune_race_anova(
        object = bt_lgb,
        preprocessor = class ~ .,
        resamples = d_folds,
        grid = bt_grid
      )
)
bm_speedy

# finalize fit

## basic
# fit_basic <-
#   select_best(bm_basic$result[[1]], metric = "roc_auc") %>%
#   finalize_workflow(workflow(class ~ ., bt), parameters = .) %>%
#   last_fit(split = d_split)
# collect_metrics(fit_basic)

## speedy
fit_speedy <-
  select_best(bm_speedy$result[[1]], metric = "roc_auc") %>%
  finalize_workflow(workflow(class ~ ., bt), parameters = .) %>%
  last_fit(split = d_split)
collect_metrics(fit_speedy)

