# Load required libraries
library(refugees) # loads the population data https://github.com/PopulationStatistics/refugees
library(dplyr) # manipulating the data
library(tidyr)
library(forecast) # to forecast refugee numbers
library(ggplot2) # plotting the data
library(ggalt)
library(stringr) # wrapping text labels
library(showtext) # using custom fonts
library(readr) # reading in csv files


# Custom font loading
font_add_google('Patrick Hand', 'labels')
font_add_google('Lato', 'lato')
showtext_auto()

# Global population figures
world_pop <- read_csv("data/world_pop_data.csv")


# idp data
filtered_idp_data <- idmc %>%
  group_by(year) %>%
  summarise(total_idp = sum(total))

# Convert 'Year' column to a time series object
idp_time_series <-
  ts(filtered_idp_data$total_idp,
     start = 1989,
     frequency = 1)

# Choose an appropriate ARIMA model using auto.arima
arima_idp_model <- auto.arima(idp_time_series)

# Forecast future refugee numbers (e.g., for the next 7 years)
forecast_idp_values <- forecast(arima_idp_model, h = 8)

# Create a data frame for forecasted values
forecast_idp_df <- data.frame(
  year = seq(2022, 2029),
  forecasted_idp = forecast_idp_values$mean,
  lower_CI = forecast_idp_values$lower,
  upper_CI = forecast_idp_values$upper
)


# refugee data 
refugee_data <- refugees::population %>%
  group_by(year) %>%
  filter(year >1988) %>%
  summarise(total_refugees = sum(refugees))

# Convert 'Year' column to a time series object
refugee_time_series <-
  ts(refugee_data$total_refugees,
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

# Renaming time column to year for dataframe merging
world_pop <- world_pop %>%
  rename('year' = Time) 

# Creating global pop ratio variable for plotting
global_pop_idp <- filtered_idp_data %>%
  left_join(world_pop, by= 'year') %>%
  mutate(pct_global_pop = total_idp/Value *100,
         plotting = pct_global_pop * 20e6) 

# Creating forecasted global pop ratio for plotting
forecast_global_pop_idp <- forecast_idp_df %>%
  left_join(world_pop, by= 'year') %>%
  mutate(pct_global_pop = forecasted_idp/Value *100,
         plotting = pct_global_pop * 20e6) 




# Colors for plot
purple <- "#8A2BE2"
yellow <- "#FFFF00"
forecast_navy <- "#000080"
forecast_orange <- "#FFA500"
text_color <- "#8B8682"
timeline_color <- "#008080"
second_text_color <-  "#8A2BE2"
global_color <- "#EE4C97FF"

# Subtitle text
subtitle_text <- paste("The number of internally displaced people has been increasing since
the 1990s. Figures peaked in the mid-1990's, around the time of the Balkans Conflict, then began
to steeply rise in the early 2010s'. This correlates to the Civil War in Syria. The steep incline
in internally displaced people figures in the late 2020's is decoupled from the global population
trend. Forecasted figures  suggest an increase of 60 million people internally displaced between 2010 
and 2030. The combined number of forcibly displaced people (refugees and internally displaced) could 
reach more than 140 million by 2030.")

wrapped_subtitle <- str_wrap(subtitle_text, 150)


# Create a ggplot for idp counts over time
idp_plot <-
  ggplot() +
  
  # Forecast confidence intervals (80% and 95%)
  geom_ribbon(
    data = forecast_idp_df,
    aes(
      x = year,
      ymin = lower_CI.95.,
      ymax = upper_CI.95.,
      fill = "95% confidence interval"
    ),
    alpha = 0.8
  ) +
  
  geom_ribbon(
    data = forecast_idp_df,
    aes(
      x = year,
      ymin = lower_CI.80.,
      ymax = upper_CI.80.,
      fill = "80% confidence interval"
    ),
    alpha = 0.8
  ) +
  
  
  # Forecasted refugee counts
  geom_line(
    data = forecast_idp_df,
    aes(x = year, y = forecasted_idp),
    linetype = "dashed",
    linewidth = 1.3
  ) +
  
  # Actual idp counts
  geom_line(data = filtered_idp_data,
            aes(x = year, y = total_idp),
            linewidth = 1.3) +
  
  # Plot labels and styling
  labs(
    title = "Internal Displacement: Unseen Struggles and Future Challenges",
    subtitle = wrapped_subtitle,
    x = "",
    y = "Internally displaced count",
    caption = "Data source: UNHCR  | Visualisation by Nicci Potts"
  ) +
  scale_fill_manual(
    values = c(
      "80% confidence interval" = forecast_orange,
      "95% confidence interval" = forecast_navy
    )
  ) +
  scale_x_continuous(breaks = seq(1950, 2030, by = 5)) +
  scale_y_continuous(
    labels = scales::comma_format(
      scale = 1,
      big.mark = ",",
      accuracy = 1
    ),
    limits = c(0, 120000000),
    breaks = seq(0, 120000000, by = 20000000),
    expand = expansion(add = c(0, 0.05))
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "lato"),
    axis.title.y = element_text(hjust = 0.05, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold", color = forecast_navy),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(family = 'labels', color = caption_color),
    plot.background = element_rect(fill = "grey88", color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = "grey88", color = NA)
  ) +
  
  guides(fill = guide_legend(title = "",
                             override.aes = list(alpha = 0.8)))


idp_plot <-
idp_plot +

  # Forecasted refugee counts
  geom_line(
    data = forecast_df,
    aes(x = year, y = forecasted_total_refugees),
    linetype = "dotted",
    linewidth = 1,
    color = timeline_color
  ) +
  
  # Actual refugee counts
  geom_line(
    data = refugee_data,
    aes(x = year, y = total_refugees),
    linewidth = 1,
    linetype = 'dashed',
    color = timeline_color
  ) +
  
  geom_text(
    aes(x = 2026, y = 30000000),
    label = str_wrap("forecasted refugee count",10),
    color = timeline_color,
    family = 'labels'
  ) +
  
  geom_text(
    aes(x = 2005, y = 15000000),
    label = "global refugee trendline",
    color = timeline_color,
    family = 'labels'
  ) 
  
  

idp_plot +
  geom_line(
    data = global_pop_idp,
    aes(x = year, y = plotting),
    linetype = "dashed",
    color = global_color
  ) +
  geom_line(
    data = forecast_global_pop_idp,
    aes(x = year, y = plotting),
    linetype = "dotted",
    color = global_color
  ) +
  geom_text(
    aes(x = 2020, y = 6000000),
    label = str_wrap("trendline of internally displaced people
            as a percentage of global population", 50),
    color = global_color,
    family = 'labels')
  
  



