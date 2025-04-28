# packages
library(tidyverse)
library(haven)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthhires)
library(mapproj)
library(broom)
library(gghighlight)
library(lubridate)  # date parsing
library(patchwork)

# theme
custom_theme <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "serif", color = "#333333"),
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.grid = element_blank(),
    axis.line = element_line(color = "#666666", linewidth = 0.5),
    plot.title = element_text(size = 20, face = "bold", color = "#222222"),
    axis.title = element_text(size = 15, color = "#444444"),
    axis.text = element_text(size = 10, color = "#555555"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.text = element_text(color = "#333333", face = "bold")
  )

# data cleaning
ised <- read_dta('data/ised.dta') %>%
  filter(country_name == 'India') %>%
  rename(country = country_name, state = state_name) %>%
  mutate(state = ifelse(state == "Orissa", "Odisha", state)) %>%
  select(country, state, year, everything()) %>%
  mutate(year = as.integer(year))
acled <- read.csv('data/acled.csv') %>%
  mutate(year = as.integer(year),
         civilian_targeting = if_else(civilian_targeting == "Civilian targeting", 1, 0),
         violent_event = if_else(
           event_type %in% c("Battles", "Violence against civilians", "Explosions/Remote violence"),
           1, 0
         ),
         fatalities = if_else(is.na(fatalities), 0, fatalities),
         population = as.integer(population_best),
         date = dmy(event_date))

# dataset merge
data <- acled %>%
  left_join(ised, by = c("country", "admin1" = "state", "year")) %>%
  filter(!is.na(ised))

# plot 1: regression/confidence interval
ised_event <- tidy(lm(
  ised_i ~ civilian_targeting + violent_event + fatalities + population,
  data = data
), conf.int = TRUE, conf.level = 0.95) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       "civilian_targeting" = "Were civilians targeted?",
                       "violent_event" = "Was the event violent?",
                       "fatalities" = "Number of fatalities",
                       "population" = "Location population (best estimate)"
  ))



plot1<-ggplot(ised_event, aes(x = estimate, y = term)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "gray40") +
  theme_minimal(base_size = 14) +
  labs(
    title = "In India, all key event characteristics\nsignificantly reduce electoral democracy",
    subtitle = "Largest negative impact is from event violence \nclassification, least is from population",
    x = "Estimated Effect on ISED Score") + custom_theme +
  theme(axis.title.y = element_blank(), plot.title = element_text(size=13), axis.title.x = element_text(size=12))


# plot 2: choropleth map of median ised scores by state
ised_med <- ised %>%
  group_by(state) %>%
  summarise(med_ised = median(ised_i, na.rm = TRUE)) %>%
  mutate(state = str_to_title(state))

india <- ne_states(country = "India", returnclass = "sf") %>%
  mutate(state = str_to_title(name)) %>%
  left_join(ised_med, by = "state")

plot2 <- ggplot(india) +
  geom_sf(aes(fill = med_ised), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  coord_sf() +
  labs(
    title = "Before the 2023 conflict, Manipur was India's\nmost democratic state",
    subtitle = "While data for Telengana State and Ladakh Territory is\nmissing, most Northern heartland states face backsliding.",
    fill = "Median ISED Score",
  ) + custom_theme +
  theme(plot.title = element_text(size=13),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

# plot 3: density plot

plot3 <- data %>%
  filter(!is.na(ised_i), event_type!="") %>%
  mutate(contention_type = case_when(
    event_type %in% c("Battles", "Riots", "Violence against civilians", "Explosions/Remote violence") ~ "Violent Events",
    event_type %in% c("Protests", "Strategic developments") ~ "Nonviolent Events")) %>%
  ggplot(aes(x = ised_i, fill = contention_type)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Violence is more common where democracy is weaker.",
    subtitle = "Violent events cluster at lower levels of subnational electoral democracy, while nonviolent contention is concentrated in more democratic \nstates. Both are less frequent at the extreme ends of the scale.",
    x = "ISED score",
    y = "Event density",
    fill = "") +
  scale_fill_manual(values = c("Violent Events" = "purple", "Nonviolent Events" = "yellow"))+
  custom_theme + theme(axis.text.y = element_blank(), axis.title = element_text(size=12))


# plot 4: boxplot

plot4 <- data %>%
  mutate(contention_type = case_when(
    event_type %in% c("Battles", "Violence against civilians", "Explosions/Remote violence", "Riots") ~ "Violent Events",
    event_type %in% c("Protests", "Strategic developments") ~ "Nonviolent Events",
  )) %>%
  ggplot(aes(x = contention_type, y = ised_i)) +
  geom_boxplot() +
  labs(
    title = "In India at a subnational level, non-violent contention is common with \nstronger democratic norms.",
    x = "Type of event",
    y = "ISED score",
  ) +
  custom_theme +theme(axis.title = element_text(size=12))

# patchwork and save

figure1<-(plot3 / plot4) +
  plot_annotation(
    caption = "Data Source: ACLED Data (http://acleddata.com/) and ISED Dataset (Harvard Dataverse)")
ggsave("figure1.png", figure1, width = 10, height = 10, dpi = 300)

figure2<-(plot1 | plot2) +
  plot_annotation(
    caption = "Data Source: ACLED Data (http://acleddata.com/) and ISED Dataset (Harvard Dataverse)")
ggsave("figure2.png", figure2, width = 10, height = 7, dpi = 300)
