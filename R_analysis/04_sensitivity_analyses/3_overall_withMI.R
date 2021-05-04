library(kableExtra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rms)
library(stringr)
library(purrr)
library(forcats)
library(cowplot)



setwd("M:/Elena - R/Multi-state  cancer/")
load("1 data prep/working.data.Rda")

# change to missing values the missing category for MEDEA and smoking
e <- eapply(.GlobalEnv, function(x) {
  x <- x %>%
    mutate(medea = na_if(medea, "Missing")) %>%
    mutate(smoke.all_time = na_if(smoke.all_time, "Missing"))
  output <- x
})


nms <- names(e)
for (n in nms) {
  assign(n, e[[n]])
}

# load imputed datasets
f <- list.files("1 data prep/imputations_cancer")
lapply(paste0("1 data prep/imputations_cancer/", f),
  load,
  envir = globalenv()
)

dd <<- datadist(r); options(datadist = "dd")

# get models function
get.models <- function(transition.data,
                       transition.data.imp,
                       transition.name) {
  summary.models <- NULL

  ## All cancers

  ## Overall
  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From diagnosed with COVID-19 to hospitalised with COVID-19",
      "From general population to death"
    )) {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + rcs(age, 5) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data, iter.max = 200, pr = FALSE
    )
  } else if (transition.name %in%
    c(
      "From general population to hospitalised with COVID-19",
      "From diagnosed with COVID-19 to death"
    )) {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + pol(age, 2) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data, iter.max = 200, pr = FALSE
    )
  } else {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + rcs(age, 3) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data, iter.max = 200, pr = FALSE
    )
  }
  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$gender <- "Overall"
  working.summary$age_cat <- "Overall"
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
      transition.name, var, model, gender, age_cat,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )


  ## Stratified by years since cancer diagnosis
  # <1 year
  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From diagnosed with COVID-19 to hospitalised with COVID-19",
      "From general population to death"
    )) {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + rcs(age, 5) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "< 1 year"),
    iter.max = 200, pr = FALSE
    )
  } else if (transition.name %in%
    c(
      "From general population to hospitalised with COVID-19",
      "From diagnosed with COVID-19 to death"
    )) {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + pol(age, 2) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "< 1 year"),
    iter.max = 200, pr = FALSE
    )
  } else {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + rcs(age, 3) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "< 1 year"),
    iter.max = 200, pr = FALSE
    )
  }
  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$gender <- "Overall"
  working.summary$age_cat <- "Overall"
  working.summary$c_status <- "< 1 year"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, age_cat,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )

  # 1-5 years
  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From diagnosed with COVID-19 to hospitalised with COVID-19",
      "From general population to death"
    )) {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + rcs(age, 5) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "1-5 years"),
    iter.max = 200, pr = FALSE
    )
  } else if (transition.name %in%
    c(
      "From general population to hospitalised with COVID-19",
      "From diagnosed with COVID-19 to death"
    )) {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + pol(age, 2) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "1-5 years"),
    iter.max = 200, pr = FALSE
    )
  } else {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + rcs(age, 3) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "1-5 years"),
    iter.max = 200, pr = FALSE
    )
  }
  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$gender <- "Overall"
  working.summary$age_cat <- "Overall"
  working.summary$c_status <- "1-5 years"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, age_cat,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )
  # >5 years
  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From diagnosed with COVID-19 to hospitalised with COVID-19",
      "From general population to death"
    )) {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + rcs(age, 5) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data %>%
      filter(cancer == 0 | c_status == ">5years"),
    iter.max = 200, pr = FALSE
    )
  } else if (transition.name %in%
    c(
      "From general population to hospitalised with COVID-19",
      "From diagnosed with COVID-19 to death"
    )) {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + pol(age, 2) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data %>%
      filter(cancer == 0 | c_status == ">5years"),
    iter.max = 200, pr = FALSE
    )
  } else {
    m.working <- fit.mult.impute(Surv(time, status) ~ cancer
    + rcs(age, 3) + gender +
      medea + smoke.all_time +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    cph,
    xtrans = transition.data.imp,
    data = transition.data %>%
      filter(cancer == 0 | c_status == ">5years"),
    iter.max = 200, pr = FALSE
    )
  }
  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$gender <- "Overall"
  working.summary$age_cat <- "Overall"
  working.summary$c_status <- ">5years"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, age_cat,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )


  summary.models
}

summary.m.healthy.diagnosis <- get.models(
  transition.data = r.healthy.diagnosis,
  transition.data.imp = r.healthy.diagnosis.imp,
  transition.name = "From general population to diagnosed with COVID-19"
)

summary.m.healthy.hospitalised <- get.models(
  transition.data = r.healthy.hospitalised,
  transition.data.imp = r.healthy.hospitalised.imp,
  transition.name = "From general population to hospitalised with COVID-19"
)

summary.m.healthy.death <- get.models(
  transition.data = r.healthy.death,
  transition.data.imp = r.healthy.death.imp,
  transition.name = "From general population to death"
)

summary.m.diagnosis.hospitalised <- get.models(
  transition.data = r.diagnosis.hospitalised,
  transition.data.imp = r.diagnosis.hospitalised.imp,
  transition.name = "From diagnosed with COVID-19 to hospitalised with COVID-19"
)

summary.m.diagnosis.death <- get.models(
  transition.data = r.diagnosis.death,
  transition.data.imp = r.diagnosis.death.imp,
  transition.name = "From diagnosed with COVID-19 to death"
)

summary.m.hospitalised.death <- get.models(
  transition.data = r.hospitalised.death,
  transition.data.imp = r.hospitalised.death.imp,
  transition.name = "From hospitalised with COVID-19 to death"
)


## join estimates
estimates <- rbind(
  summary.m.healthy.diagnosis,
  summary.m.healthy.hospitalised,
  summary.m.diagnosis.hospitalised,
  summary.m.diagnosis.death,
  summary.m.hospitalised.death,
  summary.m.healthy.death
)

# save estimates
save(estimates, file = "M:/Elena - R/Multi-state  cancer/results/7.estimates_years_imp.Rda")
#  load("M:/Elena - R/Multi-state  cancer/results/7.estimates_years_imp.Rda")

# functions to format numbers
nice.num <- function(x) {
  prettyNum(x, big.mark = ",", nsmall = 0, digits = 0, scientific = FALSE)
}
nice.num2 <- function(x) {
  prettyNum(x, big.mark = ",", nsmall = 2, digits = 2, scientific = FALSE)
}


# just the estimated effect of cancers
estimates <- estimates %>%
  filter(var == "cancer")

summary.table <- estimates %>%
  mutate(est = paste0(
    nice.num2(hr), " (",
    nice.num2(hr.low), " to ",
    nice.num2(hr.high), ")"
  )) %>%
  mutate(group = paste0(model, "- ", c_status)) %>%
  select(transition.name, group, gender, age_cat, est)

summary.table <- summary.table %>%
  pivot_wider(
    names_from = group,
    values_from = est
  )

# save results in csv
# write.csv2(summary.table, "M:/Elena - R/Multi-state  cancer/results/7.HR_years_imputation.csv",  row.names = TRUE)

summary.table <- summary.table %>%
  select(-gender, -age_cat)


## see results
kable(summary.table,
  col.names = c(
    "Transition",
    "Overall", "Less than 1 year",
    "1-5years", "More than 5 years"
  )
) %>%
  add_header_above(c("", "", "Years since the diagnosis of cancer" = 3)) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) %>%
  collapse_rows(columns = 1)


### prepare dataset for plots

plot.data <- estimates %>%
  mutate(
    transition.name =
      ifelse(transition.name ==
        "From general population to diagnosed with COVID-19",
      "From general population\nto diagnosed\nwith COVID-19",
      ifelse(transition.name ==
        "From general population to hospitalised with COVID-19",
      "From general population\nto hospitalised\nwith COVID-19",
      ifelse(transition.name ==
        "From diagnosed with COVID-19 to hospitalised with COVID-19",
      "From diagnosed with\nCOVID-19 to hospitalised\nwith COVID-19",
      ifelse(transition.name ==
        "From diagnosed with COVID-19 to death",
      "From diagnosed with\nCOVID-19 to\ndeath",
      ifelse(transition.name ==
        "From hospitalised with COVID-19 to death",
      "From hospitalised\nwith COVID-19\nto death",
      ifelse(transition.name ==
        "From general population to death",
      "From general population\nto death",
      NA
      )
      )
      )
      )
      )
      )
  ) %>%
  mutate(transition.name = factor(transition.name,
    levels = c(
      "From general population\nto diagnosed\nwith COVID-19",
      "From general population\nto hospitalised\nwith COVID-19",
      "From diagnosed with\nCOVID-19 to hospitalised\nwith COVID-19",
      "From diagnosed with\nCOVID-19 to\ndeath",
      "From hospitalised\nwith COVID-19\nto death",
      "From general population\nto death"
    )
  ))



plot.data <- plot.data %>%
  mutate(c_status = ifelse(c_status == "< 1 year", "<1 year",
    ifelse(c_status == ">5years", "\u22655 years",
      c_status
    )
  )) %>%
  mutate(c_status = factor(c_status,
    levels = c(
      "<1 year",
      "1-5 years",
      "\u22655 years",
      "Overall"
    )
  ))

# font to use
windowsFonts(A = windowsFont("Times New Roman")) # Specify font




# plot.data <- plot.data %>%
# mutate(c_status=fct_rev(c_status))


# figure plot
hrs.all.years <- plot.data %>%
  ggplot(aes(
    x = hr,
    y = c_status,
    xmin = hr.low,
    xmax = hr.high,
    color = c_status
  )) +
  scale_x_log10(
    breaks = c(0.5, 0.75, 1, 2, 5, 10),
    labels = c("0.5", "", "1", "2", "5", "10"),
    limits = c(0.5, 10)
  ) +
  facet_grid(transition.name ~ .,
    drop = TRUE,
    #    scales = "free_y",
    #   space = "free_y",
    switch = "y"
  ) +
  geom_point(size = 3, position = position_dodge(width = 1), shape = 20) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(width = 1)) +
  scale_colour_manual(values = c("#820719", "#7A40B5", "#33703A", "#2660A4")) +
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
    legend.text = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 14),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y.left = element_text(size = 14, angle = 0, face = "bold"),
    strip.background = element_rect(fill = "#f7f7f7")
  ) +
  xlab("aHR (with 95% CI)") +
  ylab("") +
  guides(
    shape = guide_legend(reverse = T, order = 2),
    colour = guide_legend(reverse = T, order = 1, ncol = 2)
  )

# plot with HR and 95%CI
data_table <- ggplot(
  data = plot.data %>%
    mutate(est = paste0(
      nice.num2(hr), " (",
      nice.num2(hr.low), " to ",
      nice.num2(hr.high), ")"
    )), # %>%mutate(group=recode(group, "Overall"="HR (95% CI)")),
  aes(x = hr, y = c_status)
) +
  facet_grid(transition.name ~ .,
    drop = TRUE, switch = "y"
  ) +
  # geom_hline(aes(yintercept = labels, colour = colour), size = 7) +
  geom_text(aes(x = 0, y = c_status, label = est),
    position = position_dodge(width = 1),
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
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold")
  ) +
  labs(title = " aHR (95% CI)") +
  xlab("")


# join plots
plot <- plot_grid(hrs.all.years, data_table, align = "h", axis = "bt", rel_widths = c(1, 0.35))

plot

ggsave("M:/Elena - R/Multi-state  cancer/results/gg.hrs.overall.years_imputation.png", plot,
  dpi = 400,
  width = 8, height = 9
)



#### plot withot general population to death

# figure plot
hrs.all.years <- plot.data %>%
  filter(transition.name != "From general population\nto death") %>%
  ggplot(aes(
    x = hr,
    y = c_status,
    xmin = hr.low,
    xmax = hr.high,
    color = c_status
  )) +
  scale_x_log10(
    breaks = c(0.5, 0.75, 1, 2, 4),
    labels = c("0.5", "", "1", "2", "4"),
    limits = c(0.5, 4)
  ) +
  facet_grid(transition.name ~ .,
    drop = TRUE,
    #    scales = "free_y",
    #   space = "free_y",
    switch = "y"
  ) +
  geom_point(size = 3, position = position_dodge(width = 1), shape = 20) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(width = 1)) +
  scale_colour_manual(values = c("#820719", "#7A40B5", "#33703A", "#2660A4")) +
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
    legend.text = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 14),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y.left = element_text(size = 14, angle = 0, face = "bold"),
    strip.background = element_rect(fill = "#f7f7f7")
  ) +
  xlab("aHR (with 95% CI)") +
  ylab("") +
  guides(
    shape = guide_legend(reverse = T, order = 2),
    colour = guide_legend(reverse = T, order = 1, ncol = 2)
  )

# plot with HR and 95%CI
data_table <- plot.data %>%
  filter(transition.name != "From general population\nto death") %>%
  mutate(est = paste0(
    nice.num2(hr), " (",
    nice.num2(hr.low), " to ",
    nice.num2(hr.high), ")"
  )) %>%
  ggplot(aes(x = hr, y = c_status)) +
  facet_grid(transition.name ~ .,
    drop = TRUE, switch = "y"
  ) +
  # geom_hline(aes(yintercept = labels, colour = colour), size = 7) +
  geom_text(aes(x = 0, y = c_status, label = est),
    position = position_dodge(width = 1),
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
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold")
  ) +
  labs(title = " aHR (95% CI)") +
  xlab("")


# join plots
plot <- plot_grid(hrs.all.years, data_table, align = "h", axis = "bt", rel_widths = c(1, 0.35))

plot

ggsave("M:/Elena - R/Multi-state  cancer/results/gg.hrs.overall.years_imputation_cov.png", plot,
  dpi = 400,
  width = 8, height = 9
)

## Combine with main model
imputed <- plot.data %>%
  filter(transition.name != "From general population\nto death") %>%
  # filter(c_status=="Overall") %>%
  select(-c(var, model, gender, age_cat)) %>%
  mutate(model = "imputed")

## only overall and adding the overall without missing data

# load("M:/Elena - R/Multi-state  cancer/results/1.estimates_adjustments_gender.Rda")
# load("M:/Elena - R/Multi-state  cancer/results/2.estimates_years_hemato_solid.Rda")


main <- estimates %>%
  filter(var == "cancer" &
    #  filter(model=="Complete adjustment"  & gender=="Overall" & var=="cancer" &
    transition.name != "From general population to death") %>%
  select(-c(var, model, gender)) %>%
  mutate(model = "main") %>%
  mutate(
    transition.name =
      ifelse(transition.name ==
        "From general population to diagnosed with COVID-19",
      "From general population\nto diagnosed\nwith COVID-19",
      ifelse(transition.name ==
        "From general population to hospitalised with COVID-19",
      "From general population\nto hospitalised\nwith COVID-19",
      ifelse(transition.name ==
        "From diagnosed with COVID-19 to hospitalised with COVID-19",
      "From diagnosed with\nCOVID-19 to hospitalised\nwith COVID-19",
      ifelse(transition.name ==
        "From diagnosed with COVID-19 to death",
      "From diagnosed with\nCOVID-19 to\ndeath",
      ifelse(transition.name ==
        "From hospitalised with COVID-19 to death",
      "From hospitalised\nwith COVID-19\nto death",
      ifelse(transition.name ==
        "From general population to death",
      "From general population\nto death",
      NA
      )
      )
      )
      )
      )
      )
  ) %>%
  mutate(transition.name = factor(transition.name,
    levels = c(
      "From general population\nto diagnosed\nwith COVID-19",
      "From general population\nto hospitalised\nwith COVID-19",
      "From diagnosed with\nCOVID-19 to hospitalised\nwith COVID-19",
      "From diagnosed with\nCOVID-19 to\ndeath",
      "From hospitalised\nwith COVID-19\nto death",
      "From general population\nto death"
    )
  )) %>%
  mutate(c_status = ifelse(c_status == "< 1 year", "<1 year",
    ifelse(c_status == ">5years", "\u22655 years",
      c_status
    )
  )) %>%
  mutate(c_status = factor(c_status,
    levels = c(
      "<1 year",
      "1-5 years",
      "\u22655 years",
      "Overall"
    )
  ))






plot.data <- rbind(imputed, main) %>%
  mutate(model = ifelse(model == "imputed", "After multiple imputation",
    ifelse(model == "main", "Main models", NA)
  )) %>%
  mutate(est = paste0(
    nice.num2(hr), " (",
    nice.num2(hr.low), " to ",
    nice.num2(hr.high), ")"
  ))




hrs.imputed.main <- plot.data %>%
  filter(c_status == "Overall") %>%
  ggplot(aes(
    x = hr,
    y = model,
    xmin = hr.low,
    xmax = hr.high,
    color = model
  )) +
  scale_x_log10(
    breaks = c(0.5, 0.75, 1, 2, 4),
    labels = c("0.5", "", "1", "2", "4"),
    limits = c(0.5, 4)
  ) +
  facet_grid(transition.name ~ .,
    drop = TRUE,
    #    scales = "free_y",
    #   space = "free_y",
    switch = "y"
  ) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(width = 1)) +
  scale_colour_manual(values = c("#820719", "#7A40B5", "#33703A", "#2660A4")) +
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
    legend.text = element_text(size = 11),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 14),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y.left = element_text(size = 14, angle = 0, face = "bold"),
    strip.background = element_rect(fill = "#f7f7f7")
  ) +
  xlab("aHR (with 95% CI)") +
  ylab("") +
  guides(
    shape = guide_legend(reverse = T, order = 2, nrow = 2),
    colour = guide_legend(reverse = T, order = 1, ncol = 2)
  )

data_table <- plot.data %>%
  filter(c_status == "Overall") %>%
  ggplot(aes(x = hr, y = model)) +
  facet_grid(transition.name ~ .,
    drop = TRUE, switch = "y"
  ) +
  scale_x_log10(limits = c(0.1, 0.6)) +
  # geom_hline(aes(yintercept = labels, colour = colour), size = 7) +
  geom_text(aes(
    x = 0.2,
    y = model,
    label = est
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



plot <- plot_grid(hrs.imputed.main, data_table, align = "h", axis = "bt", rel_widths = c(1, 0.35))

plot

ggsave("M:/Elena - R/Multi-state  cancer/results/gg.hrs.overall.imputed_cov.png", plot,
  dpi = 400,
  width = 9, height = 9
)





hrs.imputed.main <- plot.data %>%
  ggplot(aes(
    x = hr,
    y = c_status,
    xmin = hr.low,
    xmax = hr.high,
    shape = model,
    color = c_status
  )) +
  scale_x_log10(
    breaks = c(0.5, 0.75, 1, 2, 4),
    labels = c("0.5", "", "1", "2", "4"),
    limits = c(0.5, 4)
  ) +
  facet_grid(transition.name ~ .,
    drop = TRUE,
    #    scales = "free_y",
    #   space = "free_y",
    switch = "y"
  ) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(width = 1)) +
  scale_colour_manual(values = c("#820719", "#7A40B5", "#33703A", "#2660A4")) +
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
    legend.text = element_text(size = 11),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 14),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y.left = element_text(size = 14, angle = 0, face = "bold"),
    strip.background = element_rect(fill = "#f7f7f7")
  ) +
  xlab("aHR (with 95% CI)") +
  ylab("") +
  guides(
    shape = guide_legend(reverse = T, order = 2, nrow = 2),
    colour = guide_legend(reverse = T, order = 1, ncol = 2)
  )
