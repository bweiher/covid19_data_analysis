library(tidyverse)
library(lubridate)
library(glue)
library(highcharter)
options(highcharter.theme = bwmisc::theme_ipsum())

df <- read_csv("data/data.csv")

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
  left_join(tipping_points) %>% 
  filter(ds >= first_ds) %>% 
  group_by(country) %>% 
  mutate(rn =  row_number()) %>%
  hchart("line", hcaes(
    x = rn, 
    y = confirmed,
    group = country,
    name = country
  )) %>% 
  hc_title(text="Confirmed COVID-19 Cases by Country") %>% 
  hc_subtitle(text='By Days Since Country Reached 100 Cases') %>% 
  hc_xAxis(crosshair=TRUE,
           title= list(text="<i><b>Day Since 100 Cases")) %>% 
  hc_yAxis(
    title= list(text="<i><b> Confirmed Cases")) %>% 
  hc_colors(colors=RColorBrewer::brewer.pal(length(top),'Set3'))




# look at within the US 

us <- df %>% 
  filter(country == 'US') %>% 
  group_by(province,ds) %>% 
  summarise(
    confirmed=sum(confirmed, na.rm=TRUE),
    deaths=sum(deaths, na.rm=TRUE),
    recovered=sum(recovered,na.rm=TRUE)
    )


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


  
us %>% 
  inner_join(states_tipping_point) %>% 
  filter( ds >= min_ds) %>% 
  filter(province %in% top_states) %>% 
  group_by(province) %>% 
  mutate(rn=row_number()) %>% 
  ggplot(aes(
    x = rn, 
    y = confirmed,
    group = province ,
    color = province
  )) +
  geom_line() +
  hrbrthemes::theme_ipsum_tw() +
  theme(legend.position = 'bottom') +
  labs(
    x = "Day Since 100 Cases",
    y="Confirmed Cases",
    title="Confirmed COVID-19 Cases by State",
    subtitle='By Days Since State Reached 100 Cases'
  )



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
    inner_join(tipping_points) %>% 
    filter( ds >= first_ds) %>% 
    select(
      ds,  country, confirmed, deaths, recovered
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
    hc_title(text=glue("COVID-19 by Country {x}")) %>% 
    hc_subtitle(text = glue('By Days Since State Reached 100 Cases')) %>% 
    
    hc_xAxis(crosshair=TRUE,
             title= list(text="<i><b>Day Since 100 Cases")) %>% 
    hc_yAxis(
      title= list(text="<i><b> Number"))
  
  
  
  
})


# TODO linked ? 
library(htmltools)
tagList(
  tags$head(tags$style(HTML("{fontFamily: Arial Narrow;}"))),
   h1("Covid Stats by Country"),
   hw_grid(plots) 
) %>% 
  browsable()


