library(tidyverse)
library(glue)
#library(ggtext)
library(rvest)
library(gganimate)

#  ---- get and transform geojson data ----

tf <- tempfile(fileext = '.geojson')
download.file('https://opendata.arcgis.com/datasets/dc20713282734a73abe990995de40497_68.geojson', tf)
json <- jsonlite::read_json(tf)


len <- json$features %>% length


map_data <- map_df(1:len, function(i){
  
  df <- unlist(json$features[[i]]$geometry$coordinates) %>% 
    as_tibble(x = .) %>% 
    mutate(sign = case_when(
      value < 0 ~ 'lon', 
      TRUE ~ 'lat'
    ),
    county = json$features[[i]]$properties$NAME10
    ) %>% 
    distinct() %>% 
    mutate(rn = row_number())
  
  pivot_wider(df, 
              names_from = sign, 
              values_from = value) %>% 
    mutate(lon = lead(lon)) %>% 
    filter(!is.na(lat))
  
}) %>% 
  select(-rn)




# --- get covid data ----- 

covid <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>% 
  filter(state == 'Georgia') %>% 
  filter(date >= '2020-05-01') %>% 
  select(date, county,  cases) 


distinct(covid, county) %>% 
  anti_join(map_data)



covid <- covid %>%
  distinct(date) %>% 
  mutate(rn =  row_number()) %>% 
  inner_join(covid)


# -- get data on georgia county populations ---

pops <- read_html('https://www.georgia-demographics.com/counties_by_population',
          css = 'th , td+ td') %>% 
  html_table() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  filter(!str_detect(Rank, 'Census Bur')) %>% 
  transmute(
    county =  County %>% str_remove_all('County') %>% trimws(),
    pop =  str_remove_all(Population, ",") %>% as.numeric()
  )


d <- left_join(covid, pops, by = 'county') %>% 
  mutate(cases_per_thousand =  1000 * cases / pop ) %>% 
  filter(county != 'Unknown')


# join final dataset
d <- inner_join(d, map_data, by = 'county')

d <- filter(d, date >= '2020-07-01')

d %>% 
  distinct(date)


# d <- filter(d, date >= '2020-07-17') %>%
#   sample_frac(0.05)

# plot and animate -----
ggplot() + 
  geom_polygon(data = d, 
               aes( x = lon, 
                    y = lat, 
                    group = county, 
                    fill = cases_per_thousand), 
               color= "lightgrey") +
  theme_void() +
  coord_map() +
  scale_fill_viridis_c(breaks = c(10, 20, 30, 40 , 50),
                       labels = c(10, 20, 30, 40,  50) ,
                       name = 'Cases') + 
  labs(title = 'Georgia County Covid Infections: {closest_state}',
       subtitle='Cases Per 1,000 People') + 
  transition_states(date) +
  ease_aes('linear')

