
## models stratified by month - only transitions from general population
# using cancer overall
# fully adjusted models

library(kableExtra)
library(dplyr)
library(ggplot2)
library(rms)
library(stargazer)
library(knitr)
library(stringr)
library(tidyr)
library(cowplot)


# load data
setwd("M:/Elena - R/Multi-state  cancer")
load("1 data prep/working.data.Rda")

# remove datasets that we don't need
rm(r.hospitalised.death, r.diagnosis.death, r.diagnosis.hospitalised)

# function for pretty numbers
nice.num <- function(x) {
  prettyNum(x, big.mark = ",", nsmall = 0, digits = 0, scientific = FALSE)
}
nice.num2 <- function(x) {
  prettyNum(x, big.mark = ",", nsmall = 2, digits = 2, scientific = FALSE)
}


# function for models ## with cuts in time anf for the overall cancer population
dd <<- datadist(r); options(datadist = "dd")

get.models <- function(transition.data, # data for transition of interest
                       transition.name) {

  # models<-list()
  summary.models <- NULL

  transition.data <- survSplit(Surv(time, status) ~ cancer 
      + age + gender +
    medea + smoke.all_time +
    a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
    a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
  transition.data,
  cut = c(31, 61), episode = "timegroup"
  )
  transition.data.march <- transition.data %>% filter(timegroup == 1)
  transition.data.april <- transition.data %>% filter(timegroup == 2)

  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From general population to death"
    )) {
    m.working <- cph(Surv(time, status) ~ cancer
    + rcs(age, 5) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data,
    iter.max = 200
    )
  } else {
    m.working <- cph(Surv(time, status) ~ cancer
    + pol(age, 2) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data,
    iter.max = 200
    )
  }

  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$month <- "All"
  working.summary$gender <- "Overall"
  working.summary$c_status <- "Overall"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, month,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )


  ## All cancers
  ## March
  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From general population to death"
    )) {
    m.working <- cph(Surv(time, status) ~ cancer +
      rcs(age, 5) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data.march,
    iter.max = 200
    )
  } else {
    m.working <- cph(Surv(time, status) ~ cancer +
      pol(age, 2) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data.march,
    iter.max = 200
    )
  }

  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$month <- "March"
  working.summary$gender <- "Overall"
  working.summary$c_status <- "Overall"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, month,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )

  ## April
  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From general population to death"
    )) {
    m.working <- cph(Surv(time, status) ~ cancer
    + rcs(age, 5) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data.april,
    iter.max = 200
    )
  } else {
    m.working <- cph(Surv(time, status) ~ cancer
    + pol(age, 2) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data.april,
    iter.max = 200
    )
  }

  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$month <- "April"
  working.summary$gender <- "Overall"
  working.summary$c_status <- "Overall"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, month,
      c_status, hr, hr.low, hr.high
    )

  # output
  summary.models <- rbind(summary.models, working.summary)

  summary.models
}


## getting estimates
summary.m.healthy.diagnosis <- get.models(
  transition.data = r.healthy.diagnosis,
  transition.name = "From general population to diagnosed with COVID-19"
)

summary.m.healthy.hospitalised <- get.models(
  transition.data = r.healthy.hospitalised,
  transition.name = "From general population to hospitalised with COVID-19"
)
summary.m.healthy.death <- get.models(
  transition.data = r.healthy.death,
  transition.name = "From general population to death"
)

# gather results
estimates_overall <- rbind(
  summary.m.healthy.diagnosis,
  summary.m.healthy.hospitalised,
  summary.m.healthy.death
)


## getting estimates by years since cancer diagnosis
## function for models
get.models <- function(transition.data, # data for transition of interest
                       transition.name) {


  # models<-list()
  summary.models <- NULL


  transition.data <- survSplit(Surv(time, status) ~ c_status + age + gender +
    medea + smoke.all_time +
    a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
    a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
  transition.data,
  cut = c(31, 61), episode = "timegroup"
  )
  transition.data.march <- transition.data %>% filter(timegroup == 1)
  transition.data.april <- transition.data %>% filter(timegroup == 2)

  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From general population to death"
    )) {
    m.working <- cph(Surv(time, status) ~ c_status
    + rcs(age, 5) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data,
    iter.max = 200
    )
  } else {
    m.working <- cph(Surv(time, status) ~ c_status
    + pol(age, 2) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data,
    iter.max = 200
    )
  }

  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$month <- "All"
  working.summary$gender <- "Overall"
  working.summary$c_status <- "Overall"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, month,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )

  ## All c_status
  ## All together

  ## March
  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From general population to death"
    )) {
    m.working <- cph(Surv(time, status) ~ c_status +
      rcs(age, 5) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data.march,
    iter.max = 200
    )
  } else {
    m.working <- cph(Surv(time, status) ~ c_status +
      pol(age, 2) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data.march,
    iter.max = 200
    )
  }

  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$month <- "March"
  working.summary$gender <- "Overall"
  working.summary$c_status <- "Overall"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, month,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )

  ## April
  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From general population to death"
    )) {
    m.working <- cph(Surv(time, status) ~ c_status
    + rcs(age, 5) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data.april,
    iter.max = 200
    )
  } else {
    m.working <- cph(Surv(time, status) ~ c_status
    + pol(age, 2) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data.april,
    iter.max = 200
    )
  }

  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$month <- "April"
  working.summary$gender <- "Overall"
  working.summary$c_status <- "Overall"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, month,
      c_status, hr, hr.low, hr.high
    )

  # output
  summary.models <- rbind(summary.models, working.summary)

  summary.models
}


# get results
summary.m.healthy.diagnosis <- get.models(
  transition.data = r.healthy.diagnosis,
  transition.name = "From general population to diagnosed with COVID-19"
)

summary.m.healthy.hospitalised <- get.models(
  transition.data = r.healthy.hospitalised,
  transition.name = "From general population to hospitalised with COVID-19"
)


summary.m.healthy.death <- get.models(
  transition.data = r.healthy.death,
  transition.name = "From general population to death"
)

# gather resuñts
estimates_years <- rbind(
  summary.m.healthy.diagnosis,
  summary.m.healthy.hospitalised,
  summary.m.healthy.death
)

# join estimatred for ovrall and by years since diagnosis
all_estimates <- rbind(estimates_overall, estimates_years)

# save(all_estimates, file = "M:/Elena - R/Multi-state  cancer/results/4.estimates_time_strata.Rda")
# load("M:/Elena - R/Multi-state  cancer/results/4.estimates_time_strata.Rda")


## table with HR
all_estimates <- all_estimates %>%
  filter(str_detect(var, "cancer")) %>%
  mutate(var = ifelse(var == "cancer", "Overall",
    ifelse(var == "c_status - < 1 year:cancer-free", "<1 year",
      ifelse(var == "c_status - 1-5 years:cancer-free", "1-5 years",
        ifelse(var == "c_status - >5years:cancer-free", ">5 years", NA)
      )
    )
  )) %>%
  mutate(var = factor(var, levels = c("Overall", "<1 year", "1-5 years", ">5 years")))


summary.table <- all_estimates %>%
  mutate(est = paste0(
    nice.num2(hr), " (",
    nice.num2(hr.low), " to ",
    nice.num2(hr.high), ")"
  )) %>%
  select(transition.name, month, var, month, est)


summary.table <- summary.table %>%
  pivot_wider(names_from = "month", values_from = "est")

summary.table <- summary.table %>%
  mutate(
    transition.name =
      factor(transition.name,
        levels = c(
          "From general population to diagnosed with COVID-19",
          "From general population to hospitalised with COVID-19",
          "From general population to death"
        )
      )
  )

# write.csv2(summary.table, "M:/Elena - R/Multi-state  cancer/results/4.HR_time_strata.csv")

kable(summary.table,
  col.names = c(
    "Transition", "Variable",
    "All", "March", "April"
  )
) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) %>%
  collapse_rows(columns = 1)


# prepare data to plot
plot.data <- all_estimates %>%
  mutate(
    transition.name =
      ifelse(transition.name ==
        "From general population to diagnosed with COVID-19",
      "From general population\nto diagnosed\nwith COVID-19",
      ifelse(transition.name ==
        "From general population to hospitalised with COVID-19",
      "From general population\nto hospitalised\nwith COVID-19",
      transition.name
      )
      )
  ) %>%
  mutate(transition.name = factor(transition.name,
    levels = c(
      "From general population\nto diagnosed\nwith COVID-19",
      "From general population\nto hospitalised\nwith COVID-19",
      "From general population to death"
    )
  )) %>%
  mutate(var = ifelse(var == "<1 year", "<1 year",
                           ifelse(var == ">5 years", "\u22655 years",
                               ifelse(var == "1-5 years", "1-5 years", "Overall"
                           )
  ))) %>%
  mutate(var = factor(var,
                           levels = c("Overall", "\u22655 years", "1-5 years", "<1 year")
  ))


windowsFonts(A = windowsFont("Times New Roman")) # Specify font


plot.data2 <- plot.data %>%
  filter(transition.name == "From general population\nto diagnosed\nwith COVID-19") %>%
  mutate(hr_text = paste0(
    nice.num2(hr), " (",
    nice.num2(hr.low), " to ",
    nice.num2(hr.high), ")"
  )) %>%
  mutate(month = factor(month, levels = c("April", "March", "All")))

plot.time.stratified.model <- plot.data2 %>%
  ggplot(aes(
    x = hr,
    y = month,
    xmin = hr.low,
    xmax = hr.high,
    color = month
  )) +
  scale_x_log10(
    breaks = c(0.5, 0.75, 1, 2, 4),
    labels = c("0.5", "", "1", "2", "4"),
    limits = c(0.5, 4)
  ) +
  facet_grid(var ~ .,
    drop = TRUE,
    switch = "y" # ,
    # scales = "free"
  ) +
  scale_color_brewer(palette = "Dark2") +
  geom_point(size = 3, position = position_dodge(width = 1), shape = 20) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(width = 1)) +
  geom_vline(
    xintercept = 1, colour = "#000000",
    linetype = 2
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "A"),
    panel.spacing = unit(0, "lines"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 12),
    strip.text.x = element_text(size = 10, face = "bold"),
    strip.text.y.left = element_text(size = 10, face = "bold", angle = 0),
    strip.background = element_rect(fill = "#f7f7f7")
  ) +
  xlab("\n aHR (with 95% CI") +
  labs(title = "") +
  guides(colour = guide_legend(reverse = T))

plot.time.stratified.model

# add labels with HR
data_table <- plot.data2 %>%
  ggplot(aes(x = hr, y = month)) +
  facet_grid(var ~ .,
    drop = TRUE, switch = "y"
  ) +
  scale_x_log10(limits = c(0.1, 0.6)) +
  # geom_hline(aes(yintercept = labels, colour = colour), size = 7) +
  geom_text(aes(
    x = 0.2,
    y = month,
    label = hr_text
  ),
  family = "A",
  size = 5
  ) +
  theme(
    text = element_text(family = "A"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text.y.left = element_blank(),
    plot.title = element_text(hjust = 0.2, size = 13, face = "bold")
  ) +
  labs(title = " aHR (95% CI)") +
  xlab("")

# combine table and plot
plot <- plot_grid(plot.time.stratified.model,
  data_table,
  align = "h",
  axis = "bt",
  rel_widths = c(1, 0.35)
)

plot

# save plot
#ggsave("M:/Elena - R/Multi-state  cancer/results/gg.hrs.time.strata.png", plot,
#  dpi = 300,
#  width = 8, height = 9)
