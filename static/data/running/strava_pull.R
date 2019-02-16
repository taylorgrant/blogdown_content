pacman::p_load(tidyverse, rStrava)

app_name <- 'get_running' 

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, 
                                            app_client_id = Sys.getenv("STRAVA_CLIENT_ID"), 
                                            app_secret = Sys.getenv("STRAVA_APP_SECRET")))

# pull the data out of my profile
my_acts <- get_activity_list(stoken)
act_data <- compile_activities(my_acts)

# create full sequence for merge
dateseq <- seq.Date(from = as.Date("2017-03-26"), to = as.Date("2019-12-31"), by = "days") %>%
  tibble(date = .)

# clean strava data
strava <- act_data %>% 
  select(start_date_local, distance) %>%
  mutate(start_date_local = str_replace_all(start_date_local, "T", ""),
         start_date_local = str_replace_all(start_date_local, "Z", ""),
         start_date_local = as.POSIXct(start_date_local, format = "%Y-%m-%d %H:%M:%S"),
         date = as.Date(start_date_local, format = "%Y-%m-%d %H:%M:%S"))

# create overall data set 
df <- dateseq %>%
  left_join(select(strava, c(date, distance))) %>%
  mutate(distance = ifelse(is.na(distance), 0, distance)) %>%
  mutate(distance = ifelse(is.na(distance), 0, distance)) %>%
  mutate(week = lubridate::floor_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarise(distance = sum(distance)) %>%
  mutate(year = lubridate::year(week), 
         month = lubridate::month(week, label = TRUE, abbr = TRUE)) %>%
  group_by(year, month) %>%
  mutate(week_ct = row_number(),
         distance = distance/1.609, # convert KM to miles
         week_ct = ifelse(week == "2017-03-26", 4, week_ct),
         distance_fixed = ifelse(distance == 0, .001, distance)) # if using logarithmic

saveRDS(df, here("static", "data", 'running', "strava.RDS"))















