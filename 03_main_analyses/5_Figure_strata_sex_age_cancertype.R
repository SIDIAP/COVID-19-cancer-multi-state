

library(ggpubr)
library(dplyr)
library(tidyr)
library(forcats)
library(cowplot)

windowsFonts(A = windowsFont("Times New Roman"))


# Get the data
load("M:/Elena - R/Multi-state  cancer/results/gender_age_toplot.Rda")
load("M:/Elena - R/Multi-state  cancer/results/hemato_solid_toplot.Rda")

# Gender/age
gender_age <- gender_age %>%
  filter(transition.name != "From general population\nto death") %>%
  filter(group != "All") %>%
  select(-gender, -age_cat)

# Hematological / solid
hemato_solid <- hemato_solid %>%
  filter(var != "Cancer") %>%
  mutate(group = var) %>%
  select(-hr_text, -gender)

# join two datasets
plot.data <- rbind(gender_age, hemato_solid) %>%
  mutate(group = recode(group, Hematological = "Haematological")) %>%
  mutate(group = factor(group, levels = c("Haematological", "Solid", "\u226570 years", "<70 years", "Male", "Female")))

## PLOTS
# plot all together - no spaces in between
hrs.strata <- plot.data %>%
  filter(transition.name != "From general population\nto death") %>%
  ggplot(aes(
    x = hr,
    y = group,
    xmin = hr.low,
    xmax = hr.high,
    color = group,
    fill = group
  )) +
  scale_x_log10() +
  facet_grid(c_status ~ transition.name,
    drop = TRUE,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(width = 1)) +
  scale_colour_manual(
    name = "Legend",
    values = c("#CC1B26", "#699C27", "#90323D", "#E0AC00", "#8BAFE5", "#6A449C"),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("#CC1B26", "#699C27", "#90323D", "#E0AC00", "#8BAFE5", "#6A449C"),
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_vline(
    xintercept = 1,
    colour = "grey50",
    linetype = "solid"
  ) +
  geom_hline(
    yintercept = c(2.5, 4.5),
    linetype = "dashed",
    color = "grey25"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "A"),
    panel.spacing = unit(0, "lines"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 14),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y.left = element_text(size = 14, face = "bold", angle = 0),
    strip.background = element_rect(fill = "#f7f7f7")
  ) +
  xlab("\n aHR (with 95% CI)") +
  ylab("")

## add names of stratification variables
data_table <- ggplot(data = plot.data %>%
  filter(transition.name == "From general population\nto diagnosed\nwith COVID-19")) +
  facet_grid(c_status ~ .,
    drop = TRUE, switch = "y"
  ) +
  geom_text(aes(x = 0, y = group, label = group),
    position = position_dodge(width = 1),
    family = "A",
    size = 4,
    hjust = 0
  ) +
  xlab(NULL) +
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
    plot.margin = unit(c(1, 1, 1, -5), "cm")
  )

# combine them
plot <- plot_grid(hrs.strata, data_table, align = "h", axis = "bt", rel_widths = c(1, 0.2))
plot

# save plot
ggsave("M:/Elena - R/Multi-state  cancer/results/hrs.stratified_together.png",
  plot,
  dpi = 400,
  width = 16, height = 9)

### Alternative - we plot each strata for years since cancer diagnosis separately

# We first get the legend for the previous plot
legend <- get_legend(hrs.strata)

plot.data <- plot.data %>%
  mutate(c_status = ifelse(c_status == "Overall", "Overall  ",
    ifelse(c_status == "\u22655 years", "\u22655 years ",
      ifelse(c_status == "<1 year", "<1 year  ", "1-5 years")
    )
  ))

# Plot for overall
hrs.overall <- plot.data %>%
  filter(transition.name != "From general population\nto death") %>%
  filter(c_status == "Overall  ") %>%
  ggplot(aes(
    x = hr,
    y = group,
    xmin = hr.low,
    xmax = hr.high,
    color = group,
    fill = group
  )) +
  scale_x_log10(
    breaks = c(0.5, 1, 2.5, 5, 10),
    labels = c("0.5", "1", "2.5", "5", "10"),
    limits = c(0.5, 20)
  ) +
  facet_grid(c_status ~ transition.name,
    drop = TRUE,
    switch = "y"
  ) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(width = 1)) +
  scale_colour_manual(
    name = "Legend",
    values = c("#CC1B26", "#699C27", "#90323D", "#E0AC00", "#8BAFE5", "#6A449C"),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("#CC1B26", "#699C27", "#90323D", "#E0AC00", "#8BAFE5", "#6A449C"),
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_vline(
    xintercept = 1,
    colour = "grey50",
    linetype = "solid"
  ) +
  geom_hline(
    yintercept = c(2.5, 4.5),
    linetype = "dashed",
    color = "grey25"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "A"),
    panel.spacing = unit(0, "lines"),
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 14),
    plot.margin = unit(c(0.5, 0.1, 0, 0), "cm"),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y.left = element_text(size = 14, face = "bold", angle = 0),
    strip.background = element_rect(fill = "#f7f7f7")
  ) +
  xlab(NULL) +
  ylab(NULL)


data_table <- ggplot(data = plot.data %>%
  filter(c_status == "Overall  ") %>%
  filter(transition.name == "From general population\nto diagnosed\nwith COVID-19")) +
  facet_grid(c_status ~ .,
    drop = TRUE, switch = "y"
  ) +
  geom_text(aes(x = 0, y = group, label = group),
    position = position_dodge(width = 1),
    family = "A",
    size = 4,
    hjust = 0
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
    plot.margin = unit(c(0, 0, 0, -6), "cm")
  ) +
  xlab(NULL)


plot.overall <- plot_grid(hrs.overall, data_table, align = "h", axis = "bt", rel_widths = c(1, 0.2)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


# Plot for >5y cancer
hrs.5y <- plot.data %>%
  filter(transition.name != "From general population\nto death") %>%
  filter(c_status == "\u22655 years ") %>%
  ggplot(aes(
    x = hr,
    y = group,
    xmin = hr.low,
    xmax = hr.high,
    color = group,
    fill = group
  )) +
  scale_x_log10(
    breaks = c(0.5, 1, 2.5, 5, 10),
    labels = c("0.5", "1", "2.5", "5", "10"),
    limits = c(0.5, 20)
  ) +
  facet_grid(c_status ~ transition.name,
    drop = TRUE,
    switch = "y"
  ) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(width = 1)) +
  scale_colour_manual(
    name = "Legend",
    values = c("#CC1B26", "#699C27", "#90323D", "#E0AC00", "#8BAFE5", "#6A449C"),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("#CC1B26", "#699C27", "#90323D", "#E0AC00", "#8BAFE5", "#6A449C"),
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_vline(
    xintercept = 1,
    colour = "grey50",
    linetype = "solid"
  ) +
  geom_hline(
    yintercept = c(2.5, 4.5),
    linetype = "dashed",
    color = "grey25"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "A"),
    panel.spacing = unit(0, "lines"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 14),
    plot.margin = unit(c(0.5, 0.1, 0, 0), "cm"),
    strip.text.x = element_blank(),
    strip.text.y.left = element_text(size = 14, face = "bold", angle = 0),
    strip.background = element_rect(fill = "#f7f7f7")
  ) +
  xlab(NULL) +
  ylab(NULL)


data_table <- ggplot(data = plot.data %>%
  filter(c_status == "\u22655 years ") %>%
  filter(transition.name == "From general population\nto diagnosed\nwith COVID-19")) +
  facet_grid(c_status ~ .,
    drop = TRUE, switch = "y"
  ) +
  geom_text(aes(x = 0, y = group, label = group),
    position = position_dodge(width = 1),
    family = "A",
    size = 4,
    hjust = 0
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
    plot.margin = unit(c(0.5, 0, 0, -6), "cm")
  ) +
  xlab(NULL)


plot.5y <- plot_grid(hrs.5y, data_table, align = "h", axis = "bt", rel_widths = c(1, 0.2)) + theme(
  plot.margin = unit(c(0, 0, 0, 0), "cm")
)


# Plot for 1-5 years cancer

hrs.1_5y <- plot.data %>%
  filter(transition.name != "From general population\nto death") %>%
  filter(c_status == "1-5 years") %>%
  ggplot(aes(
    x = hr,
    y = group,
    xmin = hr.low,
    xmax = hr.high,
    color = group,
    fill = group
  )) +
  scale_x_log10(
    breaks = c(0.5, 1, 2.5, 5, 10),
    labels = c("0.5", "1", "2.5", "5", "10"),
    limits = c(0.5, 20)
  ) +
  facet_grid(c_status ~ transition.name,
    drop = TRUE,
    switch = "y"
  ) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(width = 1)) +
  scale_colour_manual(
    name = "Legend",
    values = c("#CC1B26", "#699C27", "#90323D", "#E0AC00", "#8BAFE5", "#6A449C"),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("#CC1B26", "#699C27", "#90323D", "#E0AC00", "#8BAFE5", "#6A449C"),
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_vline(
    xintercept = 1,
    colour = "grey50",
    linetype = "solid"
  ) +
  geom_hline(yintercept = c(2.5, 4.5), linetype = "dashed", color = "grey25") +
  theme_bw() +
  theme(
    text = element_text(family = "A"),
    panel.spacing = unit(0, "lines"),
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 14),
    plot.margin = unit(c(0.5, 0.1, 0, 0), "cm"),
    strip.text.x = element_blank(),
    strip.text.y.left = element_text(size = 14, face = "bold", angle = 0),
    strip.background = element_rect(fill = "#f7f7f7")
  ) +
  xlab(NULL) +
  ylab(NULL)



data_table <- ggplot(data = plot.data %>%
  filter(c_status == "1-5 years") %>%
  filter(transition.name == "From general population\nto diagnosed\nwith COVID-19")) +
  facet_grid(c_status ~ .,
    drop = TRUE, switch = "y"
  ) +
  geom_text(aes(x = 0, y = group, label = group),
    position = position_dodge(width = 1),
    family = "A",
    size = 4,
    hjust = 0
  ) +
  xlab(NULL) +
  theme(
    text = element_text(family = "A"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text.y.left = element_blank(),
    plot.margin = unit(c(0.5, 0, 0, -6), "cm")
  )


plot.1_5year <- plot_grid(hrs.1_5y, data_table, align = "h", axis = "bt", rel_widths = c(1, 0.2)) + theme(
  plot.margin = unit(c(0, 0, 0, 0), "cm")
)




# Plot for <1 year

hrs.1y <- plot.data %>%
  filter(transition.name != "From general population\nto death") %>%
  filter(c_status == "<1 year  ") %>%
  ggplot(aes(
    x = hr,
    y = group,
    xmin = hr.low,
    xmax = hr.high,
    color = group,
    fill = group
  )) +
  scale_x_log10(
    breaks = c(0.5, 1, 2.5, 5, 10),
    labels = c("0.5", "1", "2.5", "5", "10"),
    limits = c(0.5, 20)
  ) +
  facet_grid(c_status ~ transition.name,
    drop = TRUE,
    switch = "y"
  ) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(width = 1)) +
  scale_colour_manual(
    name = "Legend",
    values = c("#CC1B26", "#699C27", "#90323D", "#E0AC00", "#8BAFE5", "#6A449C"),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("#CC1B26", "#699C27", "#90323D", "#E0AC00", "#8BAFE5", "#6A449C"),
    guide = guide_legend(reverse = TRUE)
  ) +
  geom_vline(
    xintercept = 1,
    colour = "grey50",
    linetype = "solid"
  ) +
  geom_hline(yintercept = c(2.5, 4.5), linetype = "dashed", color = "grey25") +
  theme_bw() +
  theme(
    text = element_text(family = "A"),
    panel.spacing = unit(0, "lines"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 14),
    plot.margin = unit(c(0.5, 0.1, 0, 0), "cm"),
    strip.text.x = element_blank(),
    strip.text.y.left = element_text(size = 14, face = "bold", angle = 0),
    strip.background = element_rect(fill = "#f7f7f7")
  ) +
  xlab(NULL) +
  ylab(NULL)



data_table <- ggplot(data = plot.data %>%
  filter(c_status == "<1 year  ") %>%
  filter(transition.name == "From general population\nto diagnosed\nwith COVID-19")) +
  facet_grid(c_status ~ .,
    drop = TRUE, switch = "y"
  ) +
  geom_text(aes(x = 0, y = group, label = group),
    position = position_dodge(width = 1),
    family = "A",
    size = 4,
    hjust = 0
  ) +
  xlab(NULL) +
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
    plot.margin = unit(c(0.5, 0, 0, -6), "cm")
  )


plot.1year <- plot_grid(hrs.1y, data_table, align = "h", axis = "bt", rel_widths = c(1, 0.2)) + theme(
  plot.margin = unit(c(0, 0, 0, 0), "cm")
)



# combine the plots
join_plots <- ggarrange(plot.overall, plot.5y, plot.1_5year, plot.1year,
  align = "hv",
  heights = c(1.1, 0.8, 0.8, 0.8),
  ncol = 1
) +
  theme(plot.margin = unit(c(0, -3, 0, 0), "cm"))

# add annotations texts
join_plots <- annotate_figure(join_plots,
  bottom = text_grob("aHR (with 95% CI)", color = "black", face = "bold", size = 12, family = "A"),
  left = text_grob("By years since cancer diagnosis                                     \n",
    color = "black", face = "bold", size = 14, family = "A", rot = 90
  )
)

# add legend
final_plot <- ggarrange(join_plots,
  legend = "bottom",
  legend.grob = legend
)

final_plot

# save plot
ggsave("M:/Elena - R/Multi-state  cancer/results/hrs.stratified.png",
  final_plot,
  dpi = 400,
  width = 16, height = 9
)
