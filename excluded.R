
## Quasi first stage

Treated parishes have a greater number of grid connections, controlling for parish area and population.

![](assets/first_stage_log.png){fig-align="center"}

```{r, eval=FALSE}
# Create the tibble
regression_table <- tribble(
  ~Variable, ~Model1, ~Model2, ~Model3,
  "Treated parishes", "0.920***", "0.973***", "1.158",
  "(SE)", "(0.28)", "(0.29)", "(0.65)",
  "Parish area in square kilometers", "0.000", "0.000", "-0.000*",
  "(SE)", "(0.00)", "(0.00)", "(0.00)",
  "Parish population in 1900", "0.000***", "0.000***", "0.000",
  "(SE)", "(0.00)", "(0.00)", "(0.00)",
  "Constant", "4.586***", "4.419***", "5.366***",
  "(SE)", "(0.12)", "(0.12)", "(0.32)"
)

# Add additional statistics
additional_stats <- tribble(
  ~Variable, ~Model1, ~Model2, ~Model3,
  "R-squared", "0.12", "0.11", "0.10",
  "Observations", "511", "511", "511",
  "F-stat", "22.712", "19.937", "3.119",
  "Mean Dependent Var", "5.25", "5.08", "5.45"
)

controls <-
  tribble(
    ~Variable, ~Model1, ~Model2, ~Model3,
    "Parish area in square kilometers", "Yes", "Yes", "Yes",
    "Parish population in 1900", "Yes", "Yes", "Yes"
  )


table <- regression_table %>%
  filter(row_number() %in% c(1, 2)) %>%
  bind_rows(controls) %>%
  bind_rows(additional_stats)

table %>%
  gt() %>%
  cols_label(
    Model1 = md("(1)<br>Ln total power"),
    Model2 = md("(2)<br>Ln total power<br>(transformers)"),
    Model3 = md("(3)<br>Ln total power<br>(generators)")
  ) %>%
  tab_header(title = md("**Quasi first stage**"),
             subtitle = "Total power (logged)") %>%
  tab_footnote("Robust standard errors in parentheses") %>%
  tab_style(
    style = cell_borders(
      sides = c("t", "b"),
      color = "grey60",
      weight = px(3)
    ),
    locations = list(cells_column_labels(), cells_stubhead())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("t"),
      color = "grey60",
      weight = px(2.5)
    ),
    locations =  cells_body(rows = 5)
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("b"),
      color = "grey60",
      weight = px(3)
    ),
    locations =  cells_body(rows = 8)
  ) %>%
  tab_options(table.font.size = "80%") %>%
  gtsave("assets/first_stage_log.png")
```

## What are the new jobs that were created?


```{r}
#| eval: false

library(tidyverse)
theme_set(theme_light())
library(ggtext)
library(showtext)
## Loading Google fonts (https://fonts.google.com/)
font_add_google("IBM Plex Mono", "ibm")
font_add_google("Roboto", "roboto")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)


data <- tribble(
  ~occupation, ~employees, ~classification,
  "Electrical Fitter, General", 1000, "Medium-skilled",
  "Automobile Mechanic", 750, "Medium-skilled",
  "Operations Manager (Postal or Telecommunications Services)", 600, "Higher Manager",
  "Wood Grader", 300, "Lower-skilled",
  "Woodworking Machine Operator, General", 250, "Lower-skilled",
  "Loom Fixer", 240, "Medium-skilled",
  "Show Worker, Specialization Unknown", 230, "Lower-skilled",
  "Waste Collector", 200, "Unskilled",
  "Power-Generating Machine Operator", 200, "Medium-skilled",
  "Welder, General", 180, "Lower-skilled",
  "Pharmaceutical Assistant", 150, "Lower-skilled",
  "Blast Furnace (Ore Smelting)", 120, "Lower-skilled",
  "Highway and Street Construction Engineer", 100, "Higher Professional",
  "Lace Weaver, Machine", 80, "Lower-skilled",
  "Fiber Mixer and Blender", 50, "Lower-skilled",
  "Technical Salesman", 40, "Medium-skilled"
)

# Convert strings to title case
data$occupation <- tools::toTitleCase(data$occupation)

order <- c("Higher Manager", "Higher Professional", "Medium-skilled", "Lower-skilled")

order_tbl <- tibble(classification = order, order = 1:length(order))

# join data and order tbl
data <- data %>%
  left_join(order_tbl, by = "classification")

data %>%
  mutate(occupation = fct_reorder(occupation, employees)) %>%
  filter(classification != "Unskilled") %>%
  mutate(classification = fct_reorder(classification, order)) %>%
  ggplot(aes(x = occupation, y = employees, fill = classification)) +
  geom_col(alpha = .8) +
  coord_flip() +
  labs(x = NULL, y = "Number of employees", title = "New occupations created in Sweden 1910-1930", fill = "Job type") +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_markdown(size = 32, family = 'roboto'),
    plot.title.position = "plot",
    legend.position = "right",
    text = element_text(family = 'ibm', size = 18)
  )


ggsave(filename = here::here("assets/new_jobs_1930.jpeg"), device = "jpeg", width = 450, height = 231, units = "mm", dpi = 300)


employment <- tibble(group = c("Treated", "Control"), employment_share = c(0.6123, 0.6070))

# ggplot column plot with employment
employment %>%
  mutate(employment_share_lab = scales::percent(employment_share)) %>%
  ggplot(aes(y = group, x = employment_share, fill = group)) +
  geom_col(alpha = .8) +
  geom_text(aes(label = employment_share_lab), cex = 8, hjust = -0.1) +
  labs(y = NULL, x = "Share reporting income and occupation",
       title = "Working age population with occupation and income <br> in <b><span style = 'color:#9C6114;'>treated</span></b> and
                          <b><span style = 'color:#000080;'>control</span></b> parishes in 1930", fill = NULL) +
  scale_fill_manual(values = c("#000080", "#9C6114")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(NA, 1)) +
  theme(
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_markdown(size = 32, family = 'roboto'),
    plot.title.position = "plot",
    legend.position = "none",
    text = element_text(family = 'ibm', size = 18)
  )

ggsave(filename = here::here("assets/share_reporting_jobs.jpeg"), device = "jpeg", width = 290, height = 231, units = "mm", dpi = 300)
```

## What can we learn from this era of technological change?

1. New kinds of jobs were created with the new technology 🎛️👩‍💻

2. The new jobs were not necessarily high-skilled 🎓⬇️
- learning by doing / on the job training 🧑‍🔧💡
- creation of middle-skilled jobs 👨‍💼🔨

3. It met people where they were 📍🤝
- the new jobs were created in the same areas as the old jobs 🏭➡️🏢

<br>

  ### Why is this time different? 🕰️🔄🤔

