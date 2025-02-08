library(tidyverse)
library(sf)
library(ggplot2)

path_data <- "/Users/panjialalam/Documents/GitHub/3.-Spatial-Data-and-Shiny-App-for-Insurance/2. Data files/"
path_script <- "/Users/panjialalam/Documents/GitHub/3.-Spatial-Data-and-Shiny-App-for-Insurance/1. Scripts/"

##--------------------------------------------------------------
## Section 1: Set Up and Clean The Data
##--------------------------------------------------------------
insurance_data <- read_csv(paste0(path_data, "nhgis_insurance.csv"))

# Filter the map
us_map <- st_read(
  file.path(path_data, "cb_2018_us_state_500k.shp"))

us_map <- us_map |>
  filter(!STUSPS %in% c("PR", "AS", "VI", "HI", "GU", "MP", "AK", "RI"))

# Data cleaning
insurance_data <- insurance_data |>
  filter(!STUSAB %in% c("PR", "HI", "AK", "RI"))

insurance_data_tr <- insurance_data |>
  select(STUSAB, STATE,
         AQIYE004, AQIYE005,
         AQIYE009, AQIYE010,
         AQIYE014, AQIYE015,
         AQIYE019, AQIYE020,
         AQIYE024, AQIYE025
  ) |>
  rename(Lower_Private = AQIYE004, Lower_Public = AQIYE005,
         LowerMid_Private = AQIYE009, LowerMid_Public = AQIYE010,
         Middle_Private = AQIYE014, Middle_Public = AQIYE015,
         UpperMid_Private = AQIYE019, UpperMid_Public = AQIYE020,
         Upper_Private = AQIYE024, Upper_Public = AQIYE025,
  )

# Use loop to calculate the proportion
column_pairs <- list(
  c("Lower_Private", "Lower_Public"),
  c("LowerMid_Private", "LowerMid_Public"),
  c("Middle_Private", "Middle_Public"),
  c("UpperMid_Private", "UpperMid_Public"),
  c("Upper_Private", "Upper_Public")
)

for (pair in column_pairs) {
  private_col <- pair[1]
  public_col <- pair[2]
  
  total <- insurance_data_tr[[private_col]] + insurance_data_tr[[public_col]]
  
  # Calculate the proportion
  insurance_data_tr[[paste0(private_col)]] <- insurance_data_tr[[private_col]] / total
  insurance_data_tr[[paste0(public_col)]] <- insurance_data_tr[[public_col]] / total
} 

insurance_data_tr <- insurance_data_tr |>
  pivot_longer(
    cols = Lower_Private:Upper_Public,
    names_to = "CAT",
    values_to = "VAL"
  ) |>
  separate(CAT, into = c("INC_CAT", "TYPE"), sep = "_") |>
  pivot_wider(
    names_from = "INC_CAT",
    values_from = "VAL"
  )

# Write the data to csv to create the shiny app
write_csv(insurance_data_tr, paste0(path_data, "insurance_income.csv"))
write_csv(insurance_data_tr, paste0(path_script, "insurance_income.csv"))

##--------------------------------------------------------------
## Section 2: Generate Choropleth Maps
##--------------------------------------------------------------

# Join the map and the data frame
data_joined <- insurance_data_tr |>
  left_join(us_map, by = c("STUSAB" = "STUSPS"))

data_sf <- st_sf(data_joined)

# Create and test choropleth maps
data_map <- data_sf |>
  st_transform(2163) |>
  filter(TYPE == "Private") |>
  select(c("STUSAB", "STATE", "TYPE", "Lower"))

pvt_palette <- c("white", "skyblue", "steelblue")

pub_palette <- c("white", "#FDB777", "#FD9346")

# Create the label of states
chosen_states <- subset(data_sf, STUSAB %in% c("IL", "CA")) |> st_transform(2163)
centroids <- st_centroid(st_geometry(chosen_states)) |> 
  st_transform(crs = st_crs(chosen_states))

labels_df <- data.frame(geometry = centroids, label = chosen_states$STUSAB) |> 
  st_sf()

map_plot <- ggplot() +     
  geom_sf(data = data_map, aes(fill = Lower), alpha = 1) +
  geom_sf(data = subset(data_map, STUSAB %in% c("IL", "CA")), 
          color = "firebrick", 
          fill = NA, 
          size = 6) +
  geom_sf_text(data = labels_df, 
               aes(label = label), 
               color = "firebrick", 
               size = 2, nudge_y = 0.1) +
  scale_fill_gradientn(colors = pvt_palette) +
  labs(
    title = "Map of Private Insurance Ownership",
    fill = "Lower Income",
    caption = "Source: NHGIS Data, 2022"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Arial Narrow"),
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"))

map_plot

##--------------------------------------------------------------
## Section 3: Add Two-State Details on The Map
##--------------------------------------------------------------
data_plot <- data_joined |>
  filter(STUSAB %in% c("IL", "CA")) |>
  filter(TYPE == "Private") |>
  select(c("STUSAB", "STATE", "TYPE", 4:8)) |>
  pivot_longer(
    cols = Lower:Upper,
    names_to = "INCOME",
    values_to = "COUNT"
  ) |>
  mutate(STUSAB = factor(STUSAB, levels = c("IL", "CA")))

bar_plot <- ggplot(data = data_plot, aes(x = INCOME, y = COUNT, fill = STATE)) +
  geom_col() +
  facet_wrap(~STUSAB, scales = "free") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(x = "Income Level",
       y = "Proportion",
       title = "Private Insurance Distribution in two states",
       caption = "Source: NHGIS Data, 2022") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Arial Narrow"),
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"))

bar_plot

##--------------------------------------------------------------
## Section 4: Create Time Series Graphs
##--------------------------------------------------------------
election <- read_csv(paste0(path_data, "1976-2020-president.csv"))
insurance_ts <- read_csv(paste0(path_data, "acs_insurance_2013_2022.csv"))

# Clean and calculate the average sample of private insurance coverage
insurance_ts_cleaned <- insurance_ts |>
  mutate(HCOVPRIV = case_when(HCOVPRIV %in% 1 ~ 0,
                              HCOVPRIV %in% 2 ~ 1),
         HCOVPUB = case_when(HCOVPUB %in% 1 ~ 0,
                             HCOVPUB %in% 2 ~ 1)) |> # Change the category into binary variables
  group_by(STATEFIP, YEAR) |>
  summarize(PROP_PRIV = mean(HCOVPRIV))

# Clean the election data
election_cleaned <- election |>
  filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN"),
         year == 2020) |>
  select(1:4, candidatevotes, party_simplified) |>
  pivot_wider(
    names_from = "party_simplified",
    values_from = "candidatevotes"
  ) |>
  mutate(
    winner2020 = if_else(DEMOCRAT > REPUBLICAN, "Democrat", "Republican")
  )

# Join the data sets
ins_elect_joined <- insurance_ts_cleaned |>
  left_join(election_cleaned, by = c("STATEFIP" = "state_fips")) |>
  select(!c("year", "DEMOCRAT", "REPUBLICAN")) |>
  group_by(YEAR, winner2020) |>
  summarize(avg_private = mean(PROP_PRIV))

ins_year_joined <- ins_elect_joined |>
  group_by(YEAR) |>
  summarize(avg_private_all = mean(avg_private))

# Create the plot
ins_elect_joined$winner2020 <- factor(ins_elect_joined$winner2020, 
                                      levels = c("Democrat", "Republican"))

# Create the plot
ts_plot <- ggplot(ins_elect_joined, aes(x = YEAR, y = avg_private, group = winner2020)) +
  geom_line(aes(color = winner2020)) +
  geom_point(aes(color = winner2020)) +
  scale_color_manual(values = c("Democrat" = "steelblue", "Republican" = "firebrick")) +
  scale_x_continuous(breaks = seq(from = min(ins_elect_joined$YEAR), 
                                  to = max(ins_elect_joined$YEAR), 
                                  by = 2)) +
  labs(title = "Change in Private Insurance Coverage",
       x = "Year",
       y = "Avg. Private Insurance",
       caption = "Source: ACS Data, 2022 & MIT Election Lab",
       color = "Winner Party (2020)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Arial Narrow"),
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"))

ts_plot

ts_plot2 <- ggplot(ins_year_joined, aes(x = YEAR, y = avg_private_all)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(from = min(ins_elect_joined$YEAR), 
                                  to = max(ins_elect_joined$YEAR), 
                                  by = 2)) +
  labs(title = "Change in Private Insurance Coverage",
       x = "Year",
       y = "Avg. Private Insurance",
       caption = "Source: ACS Data, 2022") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Arial Narrow"),
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"))

ts_plot2

# Save the plots
save_path <- "/Users/panjialalam/Documents/GitHub/3.-Spatial-Data-and-Shiny-App-for-Insurance/3. Outputs/"

ggsave(paste0(save_path, "Choropleth Map.jpg"), plot = map_plot)
ggsave(paste0(save_path, "Comparison Bar Graph.jpg"), plot = bar_plot)
ggsave(paste0(save_path, "Time Series Graph.jpg"), plot = ts_plot)
