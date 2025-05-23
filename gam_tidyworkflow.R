# Chapter 22 Generalized additive models
## 22.1 Specifying GAMs in formula notation

mpg ~s(displacement) + s(horsepower) + s(weight) + acceleration + year

## 22.2 GAMS in tidymodels

library(tidymodels)
library(tidyverse)
library(patchwork)

## 22.3 Example GAM for the mpg dataset
auto <- ISLR2::Auto |>
  as_tibble()|>
  mutate(
    cylinders = as.factor(cylinders),
    origin = as.factor(origin),
  ) |>
  select(-name)

## 22.3.1 Utility functions
residual_plot <- function(model_fit, data, outcome) {
  result <- tibble(prediction = predict(model_fit, new_data=data)$.pred)
  result["residual"] <- data[outcome] - result["prediction"]
  g <- ggplot(result, aes(x=prediction, y=residual)) +
    geom_point() +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_smooth(method="loess", formula = "y ~ x") +
    labs(x="Predicted mpg", y="Residuals")
  return(g)
}

# collect and show model metrics
append_model_metrics <- function(model_metrics, model_fit, model_name) {
  model_metrics <- bind_rows(
    model_metrics,
    bind_cols(
      model=model_name,
      metrics(augment(model_fit, new_data=auto), truth=mpg, estimate=.pred)
    )
  )
  return(model_metrics)
}

show_metrics_table <- function(model_metrics) {
  model_metrics %>%
    pivot_wider(names_from=.metric, values_from=.estimate) %>%
    select(-.estimator) %>%
    knitr::kable(digits=3) %>%
    kableExtra::kable_styling(full_width=FALSE)
}

### 22.3.2 Linear regression model
formula <- mpg ~ displacement + horsepower + weight + acceleration + year

lm_model <- linear_reg() |>
  set_engine('lm') |>
  fit(formula, data = auto)

model_metrics <- append_model_metrics(tibble(), lm_model, 'linear model')
show_metrics_table(model_metrics)

residual_plot(lm_model, auto, 'mpg')


### 22.3.3 GAM with splines

gam_formula <- mpg ~s(displacement) + s(horsepower) + s(weight) + s(acceleration) + year
gam_model <- gen_additive_mod()|>
  set_engine('mgcv')|>
  set_mode('regression')|>
  fit(gam_formula, data=auto)
model_metrics <- append_model_metrics(model_metrics , gam_model,'GAM')
show_metrics_table(model_metrics)


g1 <- residual_plot(lm_model, auto, "mpg") +
  labs(title="Linear regression") +
  ylim(-10, 15)
g2 <- residual_plot(gam_model, auto, "mpg") +
  labs(title="GAM") +
  ylim(-10, 15)
g1 + g2


### 22.3.4 GAM in workflows

spec <- gen_additive_mod()|>
  set_mode('regression')|>
  set_engine('mgcv')

wf <- workflow() |>
  add_variables(outcomes=c(mpg),
               predictors = c(displacement, horsepower, weight, acceleration, year)) |>
  add_model(spec, formula = gam_formula)
wf_model <- wf %>% fit(data = auto)
model_metrics <- append_model_metrics(model_metrics, wf_model, "GAM-wf")
show_metrics_table(model_metrics)


## 22.3.5 GAM in workflow with recipes
rec <- recipe(mpg~displacement + horsepower +weight + acceleration + year, data =auto)|>
  step_YeoJohnson(all_numeric_predictors())

wf <- workflow() |>
  add_recipe(rec)|>
  add_model(spec, formula = gam_formula) # it is the keyu for gam modelswf_model_2 <- wf |> fit(data =auto)
wf_model_2 <- wf|> fit(data = auto)
model_metrics <- append_model_metrics(model_metrics, wf_model_2, 'GAM_wf_2')
show_metrics_table(model_metrics)


## 22.4 using the plot function in gam
library(mgcv)  # this is important to load the plot function
opar <- par(mfrow=c(2, 2))
plot(gam_model %>% extract_fit_engine(), scale=0)


library(mgcv)  # this is important to load the plot function
opar <- par(mfrow=c(2, 2))
plot(wf_model_2 %>% extract_fit_engine(), scale=0)
