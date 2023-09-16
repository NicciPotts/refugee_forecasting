# Load required libraries
library(refugees) # loads the population data https://github.com/PopulationStatistics/refugees
library(dplyr) # manipulating the data
library(forecast) # to forecast refugee numbers
library(ggplot2) # plotting the data
library(stringr) # wrapping text labels
library(showtext) # using custom fonts
library(readr) # reading in csv files


# Custom font loading
font_add_google('Patrick Hand', 'labels')
font_add_google('Lato', 'lato')
showtext_auto()

# Global population figures
world_pop <- read_csv("data/world_pop_data.csv")

#write.csv(population, file = "data/refugee_population.csv", row.names = FALSE)



# Change this to the desired country code (coo_iso) or 'none' to include all data
country_filter <- 'none'  

# Filter data based on the country, group by year & sum number of refugees
if (country_filter == 'none') {
  filtered_data <- population %>%
    group_by(year) %>%
    summarise(total_refugees = sum(refugees))
} else {
  filtered_data <- population %>%
    filter(coo_iso != country_filter) %>%
    group_by(year) %>%
    summarise(total_refugees = sum(refugees))
}

# Filter data for the years 1951 to 2015
filtered_data_year <- filtered_data %>%
  filter(year >= 1951 & year <= 2023)

# Convert 'Year' column to a time series object
refugee_time_series <-
  ts(filtered_data$total_refugees,
     start = 1951,
     frequency = 1)

# Choose an appropriate ARIMA model using auto.arima
arima_model <- auto.arima(refugee_time_series)

# Forecast future refugee numbers (e.g., for the next 7 years)
forecast_values <- forecast(arima_model, h = 8)

# Create a data frame for forecasted values
forecast_df <- data.frame(
  year = seq(2022, 2029),
  forecasted_total_refugees = forecast_values$mean,
  lower_CI = forecast_values$lower,
  upper_CI = forecast_values$upper
)

# Create dataframe to find the country that had highest number of refugees in a 5 year period
country_decade <- population %>%
  group_by(year, coo_name) %>%
  filter(year %in% seq(1950, 2025, by = 5)) %>%
  summarise(total_refugees = sum(refugees)) %>%
  arrange(desc(total_refugees)) %>%
  slice(1) %>%
  ungroup()


# Dataframe for timeline with highest number of refugees
country_highest_refugees <- data.frame(
  year_start = c(1955, 1980, 1986, 2015),
  year_end = c(1980, 1986, 2015, 2020),
  country = c("Unknown", "Ethiopia", "Afghanistan", "Syria"),
  middle = c(1965, 1983, 2000, 2017)
)


# Dataframe of key world events
refugee_events_df <- data.frame(
  year = c(
    1955,
    1960,
    1979,
    1983,
    1991,
    2003,
    2011,
    2021
  ),
  event = c(
    "Vietnam War",
    "decolonization Africa",
    "Soviet Invasion Afghanistan",
    "Ethiopian Famine",
    "Balkans Conflict",
    "Iraq War",
    "Syrian Civil War",
    "Invasion Ukraine"
  ),
  
  y_position = c(
    5000000,
    7000000,
    13000000,
    14000000,
    21000000,
    13000000,
    14000000,
    28000000
  )
)


# Renaming time column to year for dataframe merging
world_pop <- world_pop %>%
  rename('year' = Time) 

# Creating global pop ratio variable for plotting
global_pop <- world_pop %>%
  left_join(filtered_data, by= 'year') %>%
  filter(year >= 1951 & year <= 2022) %>%
  mutate(pct_global_pop = total_refugees/Value *100,
         plotting = pct_global_pop * 20e6) 

# Creating forecasted global pop ratio for plotting
forecast_global_pop <- forecast_df %>%
  left_join(world_pop, by= 'year') %>%
  mutate(pct_global_pop = forecasted_total_refugees/Value *100,
         plotting = pct_global_pop * 20e6) 


# Colors for plot
purple <-"#8A2BE2"
yellow <- "#FFFF00"
forecast_navy <- "#000080"
forecast_orange <- "#FFA500"
text_color <- "#8B8682"
timeline_color <- "#8B8682"
  #"#8A2BE2"
  #"#008080"
global_color <- "#6A5ACD"

# Subtitle text
subtitle_text <- paste("The number of refugees increased throughout the 1970's and 80's, 
reaching a peak in the 1990's. Refugee figures began to decrease until the early 2000's 
and then the late 2010's. Recent events have seen the number of refugees hit an all time 
high at 30 million. From 1985 to 2015, the majority of refugees originated in Afghanistan, 
a consequence of ongoing conflict in the country. Civil War in Syria, has led to an increase 
in more refugees originating from Syria than any other country. The trendline of number of 
refugees compared to global population figures suggests that the increasing number of refugees 
isn't necessarily directly correlated to increasing global populations. Forecast modelling 
suggests that the number of people forcibly displaced from the homes, and country of residence,
could hit as high as 40 or 50 million by 2030.")

wrapped_subtitle <- str_wrap(subtitle_text, 150)
  

# Create a ggplot object for refugee counts over time
refugee_plot <- ggplot() +
  
  # Forecast confidence intervals (80% and 95%)
  geom_ribbon(
    data = forecast_df,
    aes(
      x = year,
      ymin = lower_CI.95.,
      ymax = upper_CI.95.,
      fill = "95% CI"
    ),
    alpha = 0.8
  ) +
  
  geom_ribbon(
    data = forecast_df,
    aes(
      x = year,
      ymin = lower_CI.80.,
      ymax = upper_CI.80.,
      fill = "80% CI"
    ),
    alpha = 0.8
  ) +

  
  # Forecasted refugee counts
  geom_line(
    data = forecast_df,
    aes(x = year, y = forecasted_total_refugees),
    linetype = "dashed",
    linewidth = 1.3
  ) +
  
  # Actual refugee counts
  geom_line(
    data = filtered_data,
    aes(x = year, y = total_refugees),
    linewidth = 1.3
  ) +
  
  # Plot labels and styling
  labs(
    title = "The Global Refugee Crisis: Past, Present, and Future Insights",
    subtitle = wrapped_subtitle,
    x = "",
    y = "Refugee count"
  ) +
  scale_fill_manual(values = c("80% CI" = forecast_orange, "95% CI" = forecast_navy)) +
  scale_x_continuous(breaks = seq(1950, 2030, by = 5)) +
  scale_y_continuous(
    labels = scales::comma_format(
      scale = 1,
      big.mark = ",",
      accuracy = 1
    ),
    limits = c(0, 50000000),
    breaks = seq(0, 50000000, by = 10000000),
    expand = expansion(add = c(0, 0.05))
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "lato"),
    axis.title.y = element_text(hjust = 0.05, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold", color = forecast_navy),
    plot.subtitle = element_text(size = 10)
  ) +
  
  guides(fill = guide_legend(title = "", 
                             override.aes = list(alpha = 0.8)
                             ))

# Add labels and lines for event data
refugee_plot <-
  refugee_plot +
  geom_text(
    data = refugee_events_df,
    aes(x = year, y = y_position, label = event),
    color = text_color,
    family = 'labels',
    vjust = -1,
    size = 3.5,
    angle = 90,
    nudge_x = 0.5
  ) +
  
  geom_segment(
    data = refugee_events_df,
    aes(
      x = year,
      xend = year,
      y = 0,
      yend = y_position + 5e6
    ),
    linetype = "dotted",
    color = text_color,
    linewidth = 0.5
  ) +
  
  # Labels and lines for countries with most refugees
  geom_text(
    data = country_highest_refugees,
    aes(x = middle, y = 47500000, label = country),
    color = timeline_color,
    family = 'labels',
    size = 4
  ) +
  
  geom_segment(
    data = country_highest_refugees,
    aes(
      x = year_start,
      xend = year_end,
      y = 46500000,
      yend = 46500000
    ),
    color = timeline_color,
    linetype = "solid",
    linewidth = 1
  ) +
  
  geom_segment(
    data = country_highest_refugees,
    aes(
      x = year_start,
      xend = year_start,
      y = 47000000,
      yend = 46000000
    ),
    color = timeline_color,
    linetype = "solid",
    linewidth = 1
  ) +

  geom_segment(
    data = country_highest_refugees,
    aes(
      x = year_end,
      xend = year_end,
      y = 47000000,
      yend = 46000000
    ),
    color = timeline_color,
    linetype = "solid",
    linewidth = 1
  ) +
  
  geom_text(aes(x = 1988, 
                y = 45000000),
            label ="where the highest number of refugees originate from during the depicted time period.",
            color = timeline_color,
            family = 'labels',
            size = 4) 



refugee_plot +
  geom_line(
    data = global_pop,
    aes(x = year, y = plotting),
    linetype = "dashed",
    color = global_color
  ) +
  geom_line(
    data = forecast_global_pop,
    aes(x = year, y = plotting),
    linetype = "dashed",
    color = global_color
  ) +
  geom_text(aes(x = 2025, y = 3000000),
            label = "trendline of refugees 
            as a percentage of 
            global population",
            color = global_color,
            family = 'labels',
            size = 3)
