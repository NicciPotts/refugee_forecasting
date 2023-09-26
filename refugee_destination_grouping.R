# Packages for data manipulation
library(dplyr)

# Packages for visualization
library(ggplot2)
library(ggstream)
library(gridExtra)
library(grid)
library(showtext)
library(colorspace)
library(paletteer)

# Packages for spatial data
library(sf)
library(rnaturalearth)
library(refugees)


# Load custom fonts and set up showtext
font_add_google('Patrick Hand', 'labels')
font_add_google('Lato', 'lato')
showtext_auto()


## Data manipulation

# Load world spatial data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Function to filter and summarize data
filter_and_summarize_data <- function(data, group_var, year_var, filter_start, filter_end) {
  data %>%
    group_by({{ group_var }}) %>%
    filter({{ year_var }} >= filter_start & {{ year_var }} <= filter_end) %>%
    summarise(total_refugees = sum(refugees))
}

# Filter and aggregate refugee data
refugee_df <- refugees::population %>%
  filter_and_summarize_data(coa_iso, year, 2000, 2010)

# Select relevant columns from the world dataset
filtered_world <-
  world %>% select(pop_est, economy, 
                   income_grp, iso_a3, 
                   continent, subregion)

# Function to merge data
merge_data <- function(data, world_data, by_column) {
  merge(data, world_data, by.x = by_column, by.y = 'iso_a3')
}

# Merge refugee data with world data
merged_refugee_data <- merge_data(refugees::population, filtered_world, 'coa_iso')



# Group and summarize refugee data by economy
economy_df <- merged_refugee_data %>%
  group_by(year, economy) %>%
  summarise(total_refugees = sum(refugees)) %>%
  mutate(label = "economy")

# Group and summarize refugee data by income group
income_df <- merged_refugee_data %>%
  group_by(year, income_grp) %>%
  summarise(total_refugees = sum(refugees)) %>%
  mutate(label = "income")

# Group and summarize refugee data by continent
continent_df <- merged_refugee_data %>%
  group_by(year, continent) %>%
  summarise(total_refugees = sum(refugees)) %>%
  mutate(label = "continent")


## Plotting

pal <- c(
  "#EE4C97FF", lighten("#EE4C97FF", .25, space = "HLS"),
  "#FFDC91FF", lighten("#FFDC91FF", .2, space = "HLS"),
  "#6F99ADFF", lighten("#6F99ADFF", .25, space = "HLS"),
  "#7876B1FF", lighten("#7876B1FF", .2, space = "HLS"),
  "#20854EFF", lighten("#20854EFF", .15, space = "HLS"),
  "#E18727FF", lighten("#E18727FF", .2, space = "HLS"),
  "#0072B5FF", lighten("#0072B5FF", .15, space = "HLS"),
  "#BC3C29FF", lighten("#BC3C29FF", .2, space = "HLS")
)

pal2 <- c(
  "#EE4C97FF", 
  "#FFDC91FF",
  "#6F99ADFF",
  "#7876B1FF",
  "#20854EFF",
  "#E18727FF",
  "#0072B5FF",
  "#BC3C29FF"
)


caption_color <- "#6A5ACD"
text_color <- "#8B8682"
forecast_navy <- "#000080"

# Set theme settings
theme_set(theme_minimal(base_family = "lato", base_size = 12))

# Update theme with custom settings
theme_update(
  plot.title = element_text(
    size = 16,
    family = 'lato',
    face = "bold",
    hjust = 0,
    color = forecast_navy
  ),
  axis.text.y = element_blank(),
  axis.title = element_blank(),
  plot.background = element_rect(fill = "grey88", color = NA),
  panel.background = element_rect(fill = NA, color = NA),
  panel.grid = element_blank(),
  panel.spacing.y = unit(0, "lines"),
  legend.text = element_text(size = 9, color = text_color),
  plot.margin = margin(rep(20, 4)),
  plot.caption = element_text(family = 'labels', color = caption_color, hjust = 0.9)
)



# Define color and size variables
stream_contour_color <- "white"
stream_contour_size <- 2
stream_polygon_bw <- 0.45
stream_polygon_size <- 0










# Function to apply common plot styling
apply_common_plot_styling <- function(p) {
  p +
    geom_stream(
      geom = "contour",
      color = stream_contour_color,
      size = stream_contour_size,
      bw = stream_polygon_bw
    ) +
    geom_stream(geom = "polygon", bw = stream_polygon_bw, size = stream_polygon_size) +
    geom_vline(xintercept = seq(1951, 2022, by = 10), linetype = "dashed", color = "#8B8682", alpha = 0.5) +
    scale_color_manual(expand = c(0, 0), values = pal, guide = "none") +
    scale_fill_manual(values = pal, name = NULL) +
    scale_x_continuous(breaks = seq(1951, 2022, by = 10)) +
    labs(caption = "Data source: UNHCR  | Visualisation by Nicci Potts") +
    theme(legend.position = "bottom")
}

## Plotting

# Create  continent plot
continent_plot <-
  ggplot(data = continent_df,
         aes(
           x = year,
           y = total_refugees,
           fill = continent,
           color = continent
         )) +
  geom_vline(xintercept = seq(1951, 2022, by = 10), linetype = "dashed", color = "#8B8682", alpha = 0.5) +
  geom_stream(
    geom = "contour",
    color = stream_contour_color,
    size = stream_contour_size,
    bw = stream_polygon_bw
  ) +
  geom_stream(geom = "polygon", bw = stream_polygon_bw, linewidth = stream_polygon_size) +
  scale_color_manual(expand = c(0, 0), values = pal2, guide = "none") +
  scale_fill_manual(values = pal2, name = NULL) +
  scale_x_continuous(breaks = seq(1951, 2022, by = 10)) +
  labs(caption = "Data source: UNHCR  | Visualisation by Nicci Potts",
       title = "Continents of Asylum: Refugee Destinations Over Time") +
  theme(  
    legend.position = "bottom")

# Create economy and income plot
economy_plot <-
  ggplot(data = economy_df,
         aes(
           x = year,
           y = total_refugees,
           fill = economy,
           color = economy
         )) +
  labs(title = "Economies of Asylum: Refugee Destinations Over Time") 

income_plot <-
  ggplot(data = income_df,
         aes(
           x = year,
           y = total_refugees,
           fill = income_grp,
           color = income_grp
         )) +
  labs(title = "Incomes of Asylum: Refugee Destinations Over Time") 

# Apply common plot styling to economy and income plots
economy_plot <- apply_common_plot_styling(economy_plot) 
income_plot <- apply_common_plot_styling(income_plot)
