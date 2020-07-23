library(tidyverse)
library(glue)
library(rvest)
library(ggtext)
library(gganimate)

# --- get covid data -----

covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  filter(state == "Georgia", date >= "2020-05-01", county != "Unknown") %>%
  select(date, county, cases, deaths)


counties <- covid %>%
  distinct(county) %>%
  pull(county)



# ---- get county boundaries  -----
get_county_boundaries <- function(counties) {
  map_df(counties, function(x) {
    url <- glue("https://raw.githubusercontent.com/johan/world.geo.json/master/countries/USA/GA/{x}.geo.json")

    tf <- tempfile(fileext = ".geojson")
    download.file(url, tf, quiet = TRUE)
    json <- jsonlite::read_json(tf)

    tmp_df <- unlist(json$features[[1]]$geometry$coordinates) %>%
      as_tibble(x = .) %>%
      mutate(
        sign = case_when(
          value < 0 ~ "lon",
          TRUE ~ "lat"
        ),
        county = x
      ) %>%
      mutate(rn = row_number())

    pivot_wider(tmp_df,
      names_from = sign,
      values_from = value
    ) %>%
      mutate(lon = lag(lon)) %>%
      filter(!is.na(lat) & !is.na(lon)) %>%
      select(county, lon, lat)
  })
}


map_data <- get_county_boundaries(counties)


map_data %>%
  filter(county == "Appling") %>%
  ggplot(aes(x = lon, y = lat, group = 1)) +
  geom_polygon() +
  coord_map() +
  theme_void()


distinct(covid, county) %>%
  anti_join(map_data)


# -- get data on georgia county populations ---
pops <- read_html("https://www.georgia-demographics.com/counties_by_population",
  css = "th , td+ td"
) %>%
  html_table() %>%
  as.data.frame() %>%
  as_tibble() %>%
  filter(!str_detect(Rank, "Census Bur")) %>%
  transmute(
    county = County %>% str_remove_all("County") %>% trimws(),
    pop = str_remove_all(Population, ",") %>% as.numeric()
  )


# --- join datasets -----
d <- left_join(covid, pops, by = "county") %>%
  mutate(cases_per_thousand = 1000 * cases / pop) %>%
  inner_join(map_data, by = "county")


# ------ plot and animate -----
p <- ggplot() +
  geom_polygon(
    data = d,
    aes(
      x = lon,
      y = lat,
      group = county,
      fill = cases_per_thousand
    ),
    color = "lightgrey"
  ) +
  theme_void() +
  coord_map() +
  scale_fill_viridis_c(
    breaks = c(10, 20, 30, 40, 50),
    labels = c(10, 20, 30, 40, 50),
    name = "Cases"
  ) +
  labs(
    title = "Daily Covid Cases by County: {closest_state}",
    subtitle = "Cases Per 1,000 People in Georgia",
    caption = "Code: **github.com/bweiher/covid19_data_analysis**"
  ) +
  theme(plot.caption = element_markdown(lineheight = 1.2)) +
  transition_states(date,
    transition_length = 1,
    state_length = 2
  )



animate(
  p,
  nframes = 2 * length(unique(d$date))
)
