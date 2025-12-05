rm(list=ls())

# install rnaturalearthhires
# install.packages("remotes")
# install.packages("devtools")
# devtools::install_github("ropensci/rnaturalearthhires")


# load libraries ####
library(tidyverse)
library(sf)
library(readxl)
library(ggplot2)
library(sf)
library(rnaturalearth)
#library(rnaturalearthhires)
library(rnaturalearthdata)
library(dplyr)

# Define countries ####
countries <- c("Finland", "Sweden", "Norway", "Denmark", "Russia", "Germany",
               "Estonia", "Latvia", "Lithuania", "Belarus", "Poland")

# Get country borders as sf object
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin %in% countries)

# Plot backgroung map only
ggplot() +
  geom_sf(data = world_sf, fill = "grey", color = "black", size = 0.5, alpha=0.8) +
  coord_sf(xlim = c(6, 30), ylim = c(53.5, 66.5), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = NA))


# get ices gpkg ####
st_layers("maps/ices_grid.gpkg")

# ices rectangles ####
ices <- read_sf("maps/ices_grid.gpkg", 
                layer="ices_rectangles_eco")

ices <- ices |> select(-ID)

# c-squares
cs <- read_sf("orig/2025/2025_FDI_spatial_data/EU27/EU27_total_effort_csquares.shp")

# ADD DATA ####
data <-  read_csv("orig/2025/2025_FDI_spatial_data/EU27/spatial_landings_tableau_pts_2024_EU27.csv") # where are the rectangles???
data <- data %>% rename(ICESNAME = icesname, LONGITUDE = rectangle_lon, LATITUDE = rectangle_lat)

cs_data <- cs |> left_join(data)

# add lon/lat to ices spatial layers ####
source("spatial.R")

midpoints <- latlon(ices$ICESNAME,midpoint=TRUE)

ices <- tibble::rowid_to_column(ices, "ID")
midpoints <- tibble::rowid_to_column(midpoints, "ID")

ices <- left_join(ices, midpoints, copy = TRUE)

ices <- ices %>% rename(LATITUDE = SI_LATI, LONGITUDE = SI_LONG) %>% select(-ID)

# ADD DATA ####
data <-  read_csv("orig/2025/2025_FDI_spatial_data/EU27/spatial_landings_tableau_pts_2024_EU27.csv") # where are the rectangles???
data <- data %>% rename(ICESNAME = icesname, LONGITUDE = rectangle_lon, LATITUDE = rectangle_lat)



# testing
data2 <- ices |> select(ICESNAME, LONGITUDE, LATITUDE) |> left_join(data)

data3 <- data2 |> filter(species=="HER")

# combine all years of data
data4 <- data3 |> group_by(ICESNAME) |>
  summarise(annual.landings = sum(totwghtlandg, na.rm=TRUE)) |> 
  ungroup() 
  #|> 
  #group_by(ICESNAME) |>
  #summarise(EFFORT = mean(annual.EFFORT, na.rm=TRUE))

#effort2$text_color <- ifelse(effort2$EFFORT > 5000, "white", "black")

data4$LANDED_BIN <- factor(cut(data4$annual.landings,
                                 breaks = c(1, 100, 500, 1000, 5000, 10000, Inf),
                                 labels = c("1–100", "100–500", "500–1000", "1000–5000", "5000–10000", ">10000"),
                                 include.lowest = TRUE))


# viridisLite::viridis(n = 10, option = "mako", direction=-1)

# Plot landings by ices ####
ggplot() +
  geom_sf(data = data4, aes(fill = LANDED_BIN)) +
  geom_sf(data = world_sf, fill = "grey", color = "black", size = 1, alpha=0.8) +
  geom_sf_text(data = data4, aes(label = ICESNAME), size = 2, color = data4$text_color) +
  scale_fill_viridis_d(option = "mako", name = "2024 landed HER tons", na.value = "transparent", direction=-1) +
  coord_sf(xlim = c(6, 30), ylim = c(53.5, 66.5), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = NA))




# combine all years of data

cs_data2 <- cs_data |> filter(species == "HER")

cs_data3 <- cs_data2 |> group_by(cscode) |>
  summarise(annual.landings = sum(totwghtlandg, na.rm=TRUE)) |> 
  ungroup() 
#|> 
#group_by(ICESNAME) |>
#summarise(EFFORT = mean(annual.EFFORT, na.rm=TRUE))

#effort2$text_color <- ifelse(effort2$EFFORT > 5000, "white", "black")

cs_data3$LANDED_BIN <- factor(cut(cs_data3$annual.landings,
                               breaks = c(1, 100, 500, 1000, 5000, 10000, Inf),
                               labels = c("1–100", "100–500", "500–1000", "1000–5000", "5000–10000", ">10000"),
                               include.lowest = TRUE))

#remove rectangles with less than 1 ton landed catches
cs_data3 <- cs_data3 %>% filter(!is.na(LANDED_BIN))

# Plot landings by CS ####
ggplot() +
  geom_sf(data = cs_data3, aes(fill = LANDED_BIN)) +
  geom_sf(data = world_sf, fill = "grey", color = "black", size = 0.5, alpha=0.8) +
  geom_sf_text(data = world_sf, aes(label = adm0_a3), size = 2, color = "black") +
  #geom_sf_text(data = cs_data, aes(label = cscode), size = 2, color = cs_data3$text_color) +
  scale_fill_viridis_d(option = "mako", name = "2024 landed HER tons", na.value = "transparent", direction=-1) +
  coord_sf(xlim = c(8, 30), ylim = c(52.5, 66.5), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") + 
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = NA))