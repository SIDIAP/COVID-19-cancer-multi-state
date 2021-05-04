
## models for never smokers -
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


# keep only Never smokers
e <- eapply(.GlobalEnv, function(x) {
  x <- x %>%
    filter(smoke.all_time ==  "Never smoker" ) %>%
    select(-smoke.all_time)
  output <- x
})


nms <- names(e)
for (n in nms) {
  assign(n, e[[n]])
}


# functions for nice numbers
nice.num <- function(x) {
  prettyNum(x, big.mark = ",", nsmall = 0, digits = 0, scientific = FALSE)
}
nice.num2 <- function(x) {
  prettyNum(x, big.mark = ",", nsmall = 2, digits = 2, scientific = FALSE)
}

windowsFonts(A = windowsFont("Times New Roman")) # Font for plots



dd <<- datadist(r); options(datadist = "dd")

# function for models - only fully adjusted models

get.models <- function(transition.data, # data for transition of interest
                       transition.name) {

  # models<-list()
  summary.models <- NULL

  ### NON- SMOKERS
  ## All cancers
  ## Overall
  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From diagnosed with COVID-19 to hospitalised with COVID-19",
      "From general population to death"
    )) {
    m.working <- cph(Surv(time, status) ~ cancer
    + rcs(age, 5) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data,
    iter.max = 200
    )
  } else if (transition.name %in%
    c(
      "From general population to hospitalised with COVID-19",
      "From diagnosed with COVID-19 to death"
    )) {
    m.working <- cph(Surv(time, status) ~ cancer
    + pol(age, 2) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data,
    iter.max = 200
    )
  } else {
    m.working <- cph(Surv(time, status) ~ cancer
    + rcs(age, 3) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data ,
    iter.max = 200
    )
  }
  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$gender <- "Overall"
  working.summary$c_status <- "Overall"
  working.summary$model <- "Fully adjusted"
  working.summary$smoke <- "Never smokers"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, smoke,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )

  ##  cancers diagnosed <1year

  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From diagnosed with COVID-19 to hospitalised with COVID-19",
      "From general population to death"
    )) {
    m.working <- cph(Surv(time, status) ~ cancer
    + rcs(age, 5) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "< 1 year") ,
    iter.max = 200
    )
  } else if (transition.name %in%
    c(
      "From general population to hospitalised with COVID-19",
      "From diagnosed with COVID-19 to death"
    )) {
    m.working <- cph(Surv(time, status) ~ cancer
    + pol(age, 2) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "< 1 year") ,
    iter.max = 200
    )
  } else {
    m.working <- cph(Surv(time, status) ~ cancer
    + rcs(age, 3) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "< 1 year") ,
    iter.max = 200
    )
  }
  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$gender <- "Overall"
  working.summary$c_status <- "< 1 year"
  working.summary$smoke <- "Never smokers"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, smoke,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )

  ##  cancers diagnosed 1-5 year
  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From diagnosed with COVID-19 to hospitalised with COVID-19",
      "From general population to death"
    )) {
    m.working <- cph(Surv(time, status) ~ cancer
    + rcs(age, 5) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "1-5 years") ,
    iter.max = 200
    )
  } else if (transition.name %in%
    c(
      "From general population to hospitalised with COVID-19",
      "From diagnosed with COVID-19 to death"
    )) {
    m.working <- cph(Surv(time, status) ~ cancer
    + pol(age, 2) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "1-5 years") ,
    iter.max = 200
    )
  } else {
    m.working <- cph(Surv(time, status) ~ cancer
    + rcs(age, 3) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data %>%
      filter(cancer == 0 | c_status == "1-5 years") ,
    iter.max = 200
    )
  }
  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$gender <- "Overall"
  working.summary$c_status <- "1-5 years"
  working.summary$smoke <- "Never smokers"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, smoke,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )

  ##  cancers diagnosed >5y ago
  if (transition.name %in%
    c(
      "From general population to diagnosed with COVID-19",
      "From diagnosed with COVID-19 to hospitalised with COVID-19",
      "From general population to death"
    )) {
    m.working <- cph(Surv(time, status) ~ cancer
    + rcs(age, 5) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data %>%
      filter(cancer == 0 | c_status == ">5years"),
    iter.max = 200
    )
  } else if (transition.name %in%
    c(
      "From general population to hospitalised with COVID-19",
      "From diagnosed with COVID-19 to death"
    )) {
    m.working <- cph(Surv(time, status) ~ cancer
    + pol(age, 2) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data %>%
      filter(cancer == 0 | c_status == ">5years"),
    iter.max = 200
    )
  } else {
    m.working <- cph(Surv(time, status) ~ cancer
    + rcs(age, 3) + gender +
      medea +
      a_autoimmune_condition + a_chronic_kidney_disease + a_copd + a_dementia +
      a_heart_disease + a_hyperlipidemia + a_hypertension + a_t2_diabetes + a_obesity,
    surv = TRUE, x = TRUE, y = TRUE,
    data = transition.data %>%
      filter(cancer == 0 | c_status == ">5years"),
    iter.max = 200
    )
  }
  working.summary <- as.data.frame(summary(m.working, antilog = FALSE))
  working.summary$var <- row.names(working.summary)
  working.summary$gender <- "Overall"
  working.summary$c_status <- ">5years"
  working.summary$smoke <- "Never smokers"
  working.summary$model <- "Fully adjusted"
  working.summary$transition.name <- transition.name
  working.summary <- working.summary %>%
    mutate(
      hr = exp(Effect),
      hr.low = exp(`Lower 0.95`),
      hr.high = exp(`Upper 0.95`)
    ) %>%
    select(
      transition.name, var, model, gender, smoke,
      c_status, hr, hr.low, hr.high
    )
  # output
  summary.models <- rbind(
    summary.models,
    working.summary
  )

summary.models
}


### get the results
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

summary.m.diagnosis.hospitalised <- get.models(
  transition.data = r.diagnosis.hospitalised,
  transition.name = "From diagnosed with COVID-19 to hospitalised with COVID-19"
)

summary.m.diagnosis.death <- get.models(
  transition.data = r.diagnosis.death,
  transition.name = "From diagnosed with COVID-19 to death"
)

summary.m.hospitalised.death <- get.models(
  transition.data = r.hospitalised.death,
  transition.name = "From hospitalised with COVID-19 to death"
)


estimates <- rbind(
  summary.m.healthy.diagnosis,
  summary.m.healthy.hospitalised,
  summary.m.diagnosis.hospitalised,
  summary.m.diagnosis.death,
  summary.m.hospitalised.death,
  summary.m.healthy.death
)

# save estimates
 save(estimates, file = "M:/Elena - R/Multi-state  cancer/results/6.estimates_never_smokers.Rda")
# load("M:/Elena - R/Multi-state  cancer/results/6.estimates_never_smokers.Rda")

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
  select(transition.name, group, smoke, est)

summary.table <- summary.table %>%
  pivot_wider(
    names_from = group,
    values_from = est
  )


# write.csv2(summary.table, "M:/Elena - R/Multi-state  cancer/results/6.HR_years_smoke.csv",  row.names = TRUE)

kable(summary.table,
  col.names = c(
    "Transition", "Variable",
    "Overall", "Less than 1 year",
    "1-5years", "More than 5 years"
  )
) %>%
  add_header_above(c("", "", "", "Years since the diagnosis of cancer" = 3)) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) %>%
  collapse_rows(columns = 1)


## plotting estimates
# getting the data
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


# load main models
load("M:/Elena - R/Multi-state  cancer/results/main_models_toplot.Rda")

main_models_years <- main_models_years %>%
  mutate(smoke = "All the population") %>%
  select(-group, -age_cat)


plot.data <- rbind(plot.data,main_models_years )%>%
  select(-model,-gender)


## Risks of COVID-19 outcomes for all and non-smokers, by years since cnacer diagnosis

hrs.all.years.never.smoke <- plot.data %>%
  mutate(smoke = factor(smoke, levels = c("Never smokers", "All the population"))) %>%
  filter(transition.name != "From general population\nto death") %>%
  ggplot(aes(
    x = hr,
    y = c_status,
    xmin = hr.low,
    xmax = hr.high,
    color = c_status,
    shape = smoke
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


plot.data.table <- plot.data %>%
  mutate(smoke = factor(smoke, levels = c("Never smokers", "All the population"))) %>%
  filter(transition.name != "From general population\nto death") %>%
  mutate(est = paste0(
    nice.num2(hr), " (",
    nice.num2(hr.low), " to ",
    nice.num2(hr.high), ")"
  )) %>%
  mutate(model = ifelse(c_status == "<1 year" & smoke == "Never smokers", "1-year-never",
    ifelse(c_status == "<1 year" & smoke == "All the population", "1-year-overall",
      ifelse(c_status == "1-5 years" & smoke == "Never smokers", "1-5-year-never",
        ifelse(c_status == "1-5 years" & smoke == "All the population", "1-5-year-overall",
          ifelse(c_status == "\u22655 years" & smoke == "Never smokers", ">5-year-never",
            ifelse(c_status == "\u22655 years" & smoke == "All the population", ">5-year-overall",
              ifelse(c_status == "Overall" & smoke == "Never smokers", "overall-never",
                ifelse(c_status == "Overall" & smoke == "All the population", "overall-overall",
                  NA
                )
              )
            )
          )
        )
      )
    )
  )) %>%
  mutate(model = factor(model, levels = c(
    "1-year-never",
    "1-year-overall",
    "1-5-year-never",
    "1-5-year-overall",
    ">5-year-never",
    ">5-year-overall",
    "overall-never",
    "overall-overall"
  )))

data_table <- plot.data.table %>%
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
  size = 4
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
    plot.title = element_text(hjust = 0.2, size = 11, face = "bold")
  ) +
  labs(title = " aHR (95% CI)") +
  xlab("")


plot <- plot_grid(hrs.all.years.never.smoke, data_table, align = "h", axis = "bt", rel_widths = c(1, 0.3))

plot

ggsave("M:/Elena - R/Multi-state  cancer/results/gg.hrs.all.years.never_smokers_cov.png", plot,
  dpi = 400,
  width = 8, height = 9
)


## 3. Risks of COVID-19 outcomes for the overall population, comparing all the population vs never smokers

hrs.overall.years.never.smoke <- plot.data %>%
  mutate(smoke = factor(smoke, levels = c("Never smokers", "All the population"))) %>%
  filter(transition.name != "From general population\nto death") %>%
  filter(c_status == "Overall") %>%
  ggplot(aes(
    x = hr,
    y = smoke,
    xmin = hr.low,
    xmax = hr.high,
    color = smoke
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

data_table <- plot.data.table %>%
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



plot <- plot_grid(hrs.overall.years.never.smoke, data_table, align = "h", axis = "bt", rel_widths = c(1, 0.35))

plot

ggsave("M:/Elena - R/Multi-state  cancer/results/gg.hrs.all.years.never_smokers_cov.png", plot,
  dpi = 400,
  width = 8, height = 9
)
