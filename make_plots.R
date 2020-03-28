library(tidyverse)
library(lubridate)
library(glue)
library(highcharter)
library(htmltools)
options(highcharter.theme = bwmisc::theme_ipsum())


rstudioapi::jobRunScript('get_data.R')


# ----  functions  ------
agg_cases <- function(df){
  df %>% 
    summarise(
      confirmed=sum(confirmed, na.rm=TRUE),
      deaths=sum(deaths, na.rm=TRUE),
      recovered=sum(recovered,na.rm=TRUE)
    ) %>% 
    mutate(active =  confirmed -  (deaths + recovered)) 
}


df <- read_csv("data/data.csv",
               col_types = cols(
  country = col_character(),
  province = col_character(),
  confirmed = col_double(),
  deaths = col_double(),
  recovered = col_double(),
  ds = col_date(format = ""),
  lat = col_double(),
  lon = col_double()
))

 # find the top N countries with by their max-confirmed
top <- df %>% 
  group_by(country , ds) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  filter(confirmed == max(confirmed, na.rm=TRUE)) %>% 
  arrange(-confirmed, country, -desc(ds)) %>% 
  ungroup() %>% 
  slice(1:10) %>% 
  pull(country)

# find each countries 100 cases ds
by_country <- df %>% 
  group_by(country,ds) %>% 
  summarise(
    confirmed = sum(confirmed, na.rm = TRUE)
  ) %>% 
  ungroup()
  
tipping_points <- by_country %>% 
  filter(country %in% top) %>% 
  filter(confirmed >= 100) %>% 
  distinct(country,ds) %>% 
  select(country, first_ds = ds) %>% 
  group_by(country) %>% 
  filter(first_ds ==  min(first_ds)) %>% 
  ungroup()


by_country %>% 
  filter(country %in% top) %>% 
  left_join(tipping_points, by = "country") %>% 
  filter(ds >= first_ds) %>% 
  group_by(country) %>% 
  mutate(rn =  row_number()) -> by_country_plot_data

by_country_plot_data %>% 
  filter(country=='Italy',
         rn == max(rn)) %>% 
  pull(rn) -> max_country_rn


by_country_plot_data %>% 
  filter(rn <= max_country_rn) %>% 
  hchart("line", hcaes(
    x = rn, 
    y = confirmed,
    group = country,
    name = cou %>% ntry
  )) %>% 
  hc_title(text="Confirmed COVID-19 Cases by Country") %>% 
  hc_subtitle(text='By Days Since Country Reached 100 Cases') %>% 
  hc_xAxis(crosshair=TRUE,
           title= list(text="<i><b>Day Since 100 Cases")) %>% 
  hc_yAxis(
    title= list(text="<i><b> Confirmed Cases")) %>% 
  hc_colors(colors=RColorBrewer::brewer.pal(length(top),'Set3'))


# country specific plots
us <- df %>% 
  filter(country == 'United States') %>% 
  group_by(ds, province) %>% 
  agg_cases()


# see if the states look right ...
 us %>% 
   group_by(province) %>% 
   summarise(n=sum(confirmed)) %>% 
   arrange(-n) %>% 
   print(n=Inf)
 
 
top_states <- us %>% 
  filter(confirmed==max(confirmed)) %>% 
  arrange(-confirmed) %>% 
  head(7) %>% 
  ungroup() %>%
  pull(province)


states_tipping_point <- us %>% 
  filter(confirmed >= 100) %>% 
  group_by(province) %>% 
  filter(ds == min(ds)) %>% 
  ungroup() %>% 
  select(province, min_ds  = ds)

# 
#   
# us %>% 
#   inner_join(states_tipping_point) %>% 
#   filter( ds >= min_ds) %>% 
#   filter(province %in% top_states) %>% 
#   group_by(province) %>% 
#   mutate(rn=row_number()) %>% 
#   ggplot(aes(
#     x = rn, 
#     y = confirmed,
#     group = province ,
#     color = province
#   )) +
#   geom_line() +
#   hrbrthemes::theme_ipsum_tw() +
#   theme(legend.position = 'bottom') +
#   labs(
#     x = "Day Since 100 Cases",
#     y="Confirmed Cases",
#     title="Confirmed COVID-19 Cases by State",
#     subtitle='By Days Since State Reached 100 Cases'
#   )
# 


us %>% 
  inner_join(states_tipping_point) %>% 
  filter( ds >= min_ds) %>% 
  filter(province %in% top_states) %>% 
  group_by(province) %>% 
  mutate(rn=row_number()) %>% 
  hchart("line", hcaes(
    x = rn, 
    y = confirmed,
    group = province,
    name = province
  )) %>% 
  hc_title(text="Confirmed COVID-19 Cases by State") %>% 
  hc_subtitle(text='By Days Since State Reached 100 Cases') %>% 
  hc_xAxis(crosshair=TRUE,
           title= list(text="<i><b>Day Since 100 Cases")) %>% 
  hc_yAxis(
           title= list(text="<i><b> Confirmed Cases"))

  


# incorporate more vars


plots <- map(top, function(x){
  
  df %>% 
    filter(country == x) %>% 
    group_by(country,ds) %>% 
    summarise(
      confirmed=sum(confirmed, na.rm=TRUE),
      deaths=sum(deaths, na.rm=TRUE),
      recovered=sum(recovered,na.rm=TRUE)
    ) %>% 
    inner_join(tipping_points, by = "country") %>% 
    filter( ds >= first_ds) %>% 
    mutate(active =  confirmed -  (deaths + recovered)) %>% 
    select(
      ds,  country, active, deaths, recovered
    ) %>%  
    arrange(country,ds) %>%
    group_by(country) %>% 
    mutate(rn = row_number()) %>% 
    select(-ds) %>% 
    gather(
      metric, value, -rn, -country
    ) %>% 
    arrange(metric,rn) %>% 
    hchart('area',
           hcaes(x = rn,
                 y = value,
                 fill = metric,
                 group = metric),
           stacking='stacked') %>% 
    hc_title(text=glue("COVID-19 Stats: {x}")) %>% 
    hc_subtitle(text = glue('By Days Since Country Reached 100 Cases')) %>%
    hc_xAxis(crosshair=TRUE,
             title= list(text="<i><b>Day Since 100 Cases"),
             #max = max_country_rn,
             allowDecimals=FALSE) %>% 
    hc_yAxis(
      title= list(text="<i><b> Number"))
  
})


# TODO linked ? 
# TODO calculate ACTIVE cases.

tagList(
  tags$head(
   tags$style(HTML("* {font-family: Arial Narrow;}")),
   h1("Covid Stats by Country")),
   # tags$li("NOTE: Some of the recovered cases data seems incomplete for certain countries"),
   hw_grid(plots, ncol = 1L) 
) %>% 
  browsable()



# by country fill 


# get top provinces by country 
df %>% 
  filter(country == 'United States') %>% 
  arrange(-confirmed)
  



