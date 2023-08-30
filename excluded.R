
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

