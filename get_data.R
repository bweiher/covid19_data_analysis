library(tidyverse)
library(lubridate)
library(glue)
library(data.table)


# generate sequence of dates from first date data is avail to now
ds <- seq.Date(ymd(20200122), Sys.Date(), by = "days")


# read in CSVs from this github repo
data <- map(ds, function(x) {

  # Sys.sleep(1)

  print(x)

  ds_formatted <- format(x, "%m-%d-%Y")

  url <- glue("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/{ds_formatted}.csv")


  tryCatch(suppressMessages(read_csv(url)) %>%
    mutate(
      ds = x
    ),
  error = function(e) NA
  )
}) %>%
  .[!is.na(.)] # drop NAs incase data isn't here for current DS



# format data since it changes over time
df <- map_df(1:length(data), function(x) {
  tmp <- data[[x]]

  if ("Province_State" %in% colnames(tmp)) {
    select(
      tmp,
      country = Country_Region,
      province = Province_State,
      lat = Lat,
      lon = Long_,
      confirmed = Confirmed,
      # active = Active ,
      deaths = Deaths,
      recovered = Recovered,
      ds
    )
  } else {
    select(
      tmp,
      country = `Country/Region`,
      province = `Province/State`,
      lat = NA_real_,
      lon = NA_real_,
      confirmed = Confirmed,
      # active = NA_real_ ,
      deaths = Deaths,
      recovered = Recovered,
      ds
    )
  }
})


# imputations for changing names of some countries ...
df <- df %>%
  mutate(
    country = str_trim(country, side = "both"),
    province = str_trim(province, side = "both"),
    country = case_when(
      str_detect(country, "Iran") ~ "Iran",
      str_detect(country, "Republic of Korea|South Korea|Korea, South") ~ "South Korea",
      str_detect(country, "Mainland China|China") ~ "China",
      country == "US" ~ "United States",
      TRUE ~ country
    ),
    province = case_when(
      country == "United States" & province %like% "CA$" ~ "California",
      country == "United States" & province %like% "WA$" ~ "Washington",
      country == "United States" & province %like% "NY$" ~ "New York",
      country == "United States" & province %like% "NJ$" ~ "New Jersey",
      country == "United States" & province %like% "IL$" ~ "Illinois",
      country == "United States" & province %like% "WA$" ~ "Washington",
      country == "United States" & province %like% "OR$" ~ "Oregon",
      country == "United States" & province %like% "TX$" ~ "Texas",
      country == "United States" & province %like% "CO$" ~ "Colorado",
      country == "United States" & province %like% "AZ$" ~ "Arizona",
      country == "United States" & province %like% "MA$" ~ "Massachusetts",
      country == "United States" & province %like% "WI$" ~ "Wisconsin",
      country == "United States" & province %like% "NH$" ~ "New Hampshire",
      country == "United States" & province %like% "PA$" ~ "Pennsylvania",
      country == "United States" & province %like% "NC$" ~ "North Carolina",
      country == "United States" & province %like% "SC$" ~ "South Carolina",
      TRUE ~ province
    ),
  ) %>%
  arrange(country, province, ds)


write_csv(df, "data/data.csv")
