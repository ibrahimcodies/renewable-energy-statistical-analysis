# Renewable Energy Transition: A Statistical Analysis for Energy Policy and Sustainable Development
# IRENASTAT renewable share of electricity capacity and generation
# Reproducible analysis script; run from the project root with: Rscript analyze_irena.R

options(stringsAsFactors = FALSE)
required <- c("tidyverse", "httr2", "jsonlite", "fixest", "broom", "scales")
missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) stop("Install required packages first: ", paste(missing_pkgs, collapse = ", "))
invisible(lapply(required, library, character.only = TRUE))

project_root <- getwd()
dir.create("data_raw", showWarnings = FALSE)
dir.create("data_clean", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

api_url <- "https://pxweb.irena.org/api/v1/en/IRENASTAT/Power%20Capacity%20and%20Generation/RE-SHARE_2026_H2_PX.px"
source_page <- "https://www.irena.org/Data/Downloads/IRENASTAT"
retrieved_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

# 1. Download official metadata and all observations from IRENASTAT.
metadata <- request(api_url) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
write_json(metadata, "data_raw/irena_metadata.json", auto_unbox = TRUE, pretty = TRUE)

query <- list(query = list(
  list(code = "Region/country/area", selection = list(filter = "all", values = list("*"))),
  list(code = "Indicator", selection = list(filter = "item", values = list("0", "1"))),
  list(code = "Year", selection = list(filter = "all", values = list("*")))
), response = list(format = "csv"))
response <- request(api_url) |> req_body_json(query, auto_unbox = TRUE) |> req_perform()
raw_csv <- resp_body_string(response)
writeLines(raw_csv, "data_raw/irena_share_raw.csv", useBytes = TRUE)
raw <- read_csv("data_raw/irena_share_raw.csv", na = c("..", "", "NA"), show_col_types = FALSE)

value_col <- names(raw)[4]
raw <- raw |> rename(geography = 1, indicator = 2, year = 3, value = 4) |>
  mutate(year = as.integer(year), value = as.numeric(value))

# The official table's first ten entries are aggregates, not individual countries.
aggregate_names <- c("World", "Africa", "Asia", "C America + Carib", "Eurasia", "Europe",
                     "Middle East", "N America", "Oceania", "S America")
aggregate_audit <- raw |> distinct(geography) |> mutate(excluded_aggregate = geography %in% aggregate_names)
write_csv(aggregate_audit, "results/geography_audit.csv")

# 2. Reshape, validate, and clean. No missing value is imputed or converted to zero.
wide <- raw |>
  filter(!geography %in% aggregate_names) |>
  mutate(indicator = case_when(
    indicator == "RE share of electricity capacity (%)" ~ "capacity_share",
    indicator == "RE share of electricity generation (%)" ~ "generation_share",
    TRUE ~ NA_character_
  )) |>
  select(geography, year, indicator, value) |>
  pivot_wider(names_from = indicator, values_from = value) |>
  rename(country = geography) |>
  mutate(country = as.character(country), year = as.integer(year)) |>
  arrange(country, year)

validation <- tibble(
  raw_rows = nrow(raw), raw_geographies = n_distinct(raw$geography),
  aggregate_geographies_excluded = sum(aggregate_audit$excluded_aggregate),
  country_geographies = n_distinct(wide$country),
  duplicate_country_year_rows = nrow(wide) - nrow(distinct(wide, country, year)),
  min_year = min(wide$year, na.rm = TRUE), max_year = max(wide$year, na.rm = TRUE),
  capacity_nonmissing_year_max = max(wide$year[!is.na(wide$capacity_share)]),
  generation_nonmissing_year_max = max(wide$year[!is.na(wide$generation_share)]),
  capacity_missing = sum(is.na(wide$capacity_share)),
  generation_missing = sum(is.na(wide$generation_share)),
  out_of_range_values = sum(wide$capacity_share < 0 | wide$capacity_share > 100 |
                              wide$generation_share < 0 | wide$generation_share > 100, na.rm = TRUE)
)
write_csv(validation, "results/validation_summary.csv")
if (validation$duplicate_country_year_rows != 0) stop("Duplicate country-year rows detected")
if (validation$out_of_range_values != 0) stop("Percentage outside [0,100] detected")

# The regression requires both shares.  This complete-case restriction removes all 2025
# generation observations in this release and any other country-year with unavailable data.
clean <- wide |>
  filter(!is.na(capacity_share), !is.na(generation_share)) |>
  mutate(country = factor(country), year = as.integer(year))
write_csv(clean, "data_clean/irena_share_clean.csv")

# 3. Descriptive statistics and missingness.
descriptive <- clean |>
  summarise(
    observations = n(), countries = n_distinct(country), min_year = min(year), max_year = max(year),
    capacity_mean = mean(capacity_share), capacity_median = median(capacity_share),
    capacity_sd = sd(capacity_share), capacity_min = min(capacity_share), capacity_max = max(capacity_share),
    generation_mean = mean(generation_share), generation_median = median(generation_share),
    generation_sd = sd(generation_share), generation_min = min(generation_share), generation_max = max(generation_share)
  )
write_csv(descriptive, "results/descriptive_statistics.csv")

by_year <- clean |>
  group_by(year) |>
  summarise(observations = n(), countries = n_distinct(country),
            capacity_median = median(capacity_share), capacity_q25 = quantile(capacity_share, .25),
            capacity_q75 = quantile(capacity_share, .75), generation_median = median(generation_share),
            generation_q25 = quantile(generation_share, .25), generation_q75 = quantile(generation_share, .75),
            .groups = "drop")
write_csv(by_year, "results/yearly_summary.csv")

country_2024 <- clean |> filter(year == max(year)) |> arrange(desc(generation_share))
write_csv(country_2024, "results/country_2024_ranking.csv")

# 4. Primary model: two-way country and year fixed effects, country-clustered SE.
model <- feols(generation_share ~ capacity_share | country + year, data = clean, cluster = ~country)
model_tidy <- broom::tidy(model, conf.int = TRUE, vcov = ~country)
write_csv(model_tidy, "results/model_coefficients.csv")
model_fit <- tibble(n = nobs(model), r_squared_within = as.numeric(fitstat(model, "wr2")),
                    r_squared_overall = as.numeric(fitstat(model, "r2")))
write_csv(model_fit, "results/model_fit.csv")

# Diagnostic summaries: residual distribution, heteroskedasticity-sensitive inspection,
# and influential observations by absolute residual (descriptive, not automatic deletion).
clean <- clean |> mutate(fitted = as.numeric(fitted(model)), residual = as.numeric(resid(model)))
diagnostics <- clean |> summarise(
  residual_mean = mean(residual), residual_sd = sd(residual), residual_min = min(residual),
  residual_max = max(residual), residual_q01 = quantile(residual, .01), residual_q99 = quantile(residual, .99),
  rmse = sqrt(mean(residual^2)), max_abs_residual = max(abs(residual))
)
write_csv(diagnostics, "results/model_diagnostics.csv")
write_csv(clean |> slice_max(abs(residual), n = 20) |> select(country, year, capacity_share, generation_share, fitted, residual),
          "results/largest_residuals.csv")

# Within-country/year relationship data for an honest visualization of the FE signal.
within_capacity <- residuals(feols(capacity_share ~ 1 | country + year, data = clean))
within_generation <- residuals(feols(generation_share ~ 1 | country + year, data = clean))
within_plot <- clean |> mutate(within_capacity = within_capacity, within_generation = within_generation)
write_csv(within_plot |> select(country, year, within_capacity, within_generation), "results/within_relationship.csv")

# 5. Publication-quality ggplot2 figures.
theme_policy <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 14), plot.subtitle = element_text(size = 10),
        plot.caption = element_text(hjust = 0, size = 8, colour = "grey30"),
        panel.grid.minor = element_blank(), legend.position = "bottom")
source_note <- "Source: IRENASTAT, IRENA Renewable Energy Statistics 2026; analyst calculations."

trend_long <- by_year |> select(year, capacity_median, capacity_q25, capacity_q75,
                                generation_median, generation_q25, generation_q75) |> 
  pivot_longer(-year, names_to = "series", values_to = "share") |> 
  mutate(indicator = if_else(str_detect(series, "capacity"), "Renewable capacity share", "Renewable generation share"),
         stat = case_when(str_detect(series, "median") ~ "Median", TRUE ~ "IQR bound"))
fig1 <- ggplot(trend_long, aes(year, share, colour = indicator, group = indicator)) +
  geom_ribbon(data = filter(trend_long, stat == "IQR bound"), aes(ymin = if_else(str_detect(series, "q25"), share, NA_real_),
              ymax = if_else(str_detect(series, "q75"), share, NA_real_)), alpha = 0.12, colour = NA) +
  geom_line(data = filter(trend_long, stat == "Median"), linewidth = 0.9) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  scale_colour_manual(values = c("Renewable capacity share" = "#007C91", "Renewable generation share" = "#D95F02")) +
  labs(title = "Renewable shares across countries over time", subtitle = "Country medians with interquartile ranges; aggregates excluded",
       x = "Year", y = "Share of electricity (%)", colour = NULL, caption = source_note) + theme_policy
# Ribbon with separate bounds is clearer when built directly.
fig1 <- ggplot(by_year, aes(year)) +
  geom_ribbon(aes(ymin = capacity_q25, ymax = capacity_q75), fill = "#007C91", alpha = .15) +
  geom_line(aes(y = capacity_median, colour = "Renewable capacity share"), linewidth = .9) +
  geom_ribbon(aes(ymin = generation_q25, ymax = generation_q75), fill = "#D95F02", alpha = .15) +
  geom_line(aes(y = generation_median, colour = "Renewable generation share"), linewidth = .9) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  scale_colour_manual(values = c("Renewable capacity share" = "#007C91", "Renewable generation share" = "#D95F02")) +
  labs(title = "Renewable shares across countries over time", subtitle = "Country medians with interquartile ranges; aggregates excluded",
       x = "Year", y = "Share of electricity (%)", colour = NULL, caption = source_note) + theme_policy

rank_plot <- country_2024 |> slice_head(n = 10) |> mutate(country = forcats::fct_reorder(country, generation_share)) |>
  pivot_longer(c(capacity_share, generation_share), names_to = "indicator", values_to = "share") |>
  mutate(indicator = recode(indicator, capacity_share = "Capacity share", generation_share = "Generation share"))
fig2 <- ggplot(rank_plot, aes(share, country, fill = indicator)) + geom_col(position = position_dodge(width = .75), width = .65) +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  scale_fill_manual(values = c("Capacity share" = "#007C91", "Generation share" = "#D95F02")) +
  labs(title = "Countries with the highest renewable generation shares", subtitle = "Top 10 countries by generation share in the latest complete year (2024)",
       x = "Renewable share of electricity (%)", y = NULL, fill = NULL, caption = source_note) + theme_policy

fig3 <- ggplot(within_plot, aes(within_capacity, within_generation)) + geom_point(alpha = .18, colour = "#007C91", size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, colour = "#D95F02", linewidth = .8) +
  scale_x_continuous(labels = label_number(accuracy = 1)) + scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(title = "Within-country association between capacity and generation shares",
       subtitle = "Both variables residualised on country and year fixed effects; line is descriptive",
       x = "Capacity share residual (percentage points)", y = "Generation share residual (percentage points)", caption = source_note) + theme_policy

for (i in seq_along(list(fig1, fig2, fig3))) ggsave(sprintf("figures/figure_%02d.png", i), list(fig1, fig2, fig3)[[i]], width = 8.5, height = 5.5, dpi = 300)

# 6. Session and provenance records.
writeLines(capture.output(sessionInfo()), "results/session_info.txt")
writeLines(c(
  paste("Retrieved UTC:", retrieved_at), paste("API:", api_url), paste("Source page:", source_page),
  "The API table was queried for all listed geographies, both indicators, and all listed years.",
  paste("Aggregates excluded:", paste(aggregate_names, collapse = "; ")),
  "Missing values were retained during validation and excluded only through complete-case filtering for the paired regression dataset.",
  "No observations were imputed and no missing value was converted to zero."
), "results/provenance.txt")

message("Analysis complete. Outputs written to data_clean/, figures/, results/, and data_raw/.")
