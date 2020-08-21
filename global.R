library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(stringr)
library(janitor)
library(ggthemes)
library(zoo)
library(plotly)
library(tidyr)
library(shinythemes)
library(shinyWidgets)
library(ggdark)
library(reactable)

#newyork <- read_csv("https://raw.githubusercontent.com/katiesegreti/health/master/New_York_State_Statewide_COVID-19_Testing.csv")
newyork <- read_csv("https://raw.githubusercontent.com/katiesegreti/health/master/newyork_covid.csv")
#testing <- do.call(rbind.data.frame, fromJSON(file = "https://health.data.ny.gov/resource/xdss-u53e.json") )

# testing <- testing %>%
#   mutate(test_date = ymd(str_split(test_date, pattern = "T", simplify = TRUE)[,1]),
#          county = as.factor(county),
#          new_positives = as.numeric(new_positives),
#          cumulative_number_of_positives = as.numeric(cumulative_number_of_positives),
#          total_number_of_tests = as.numeric(total_number_of_tests),
#          cumulative_number_of_tests = as.numeric(cumulative_number_of_tests),
#          pct_pos_today = if_else((new_positives > 0 & total_number_of_tests > 0),
#                                  new_positives / total_number_of_tests, 0))

 
#  
 names(newyork) <- make_clean_names(names(newyork))
 newyork$county <- as.factor(newyork$county) 
# 
 newyork <- newyork %>%
  mutate(test_date = mdy(test_date),
         pct_pos_today = if_else((new_positives > 0 & total_number_of_tests_performed > 0),
                                 new_positives / total_number_of_tests_performed, 0)
  )


#filter testing data by county function
# county_data <- function(county_selected) {
#   filter(testing, county == county_selected) %>%
#     mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
# }
# county_data_long <- function(county_selected) {
#   filter(testing_long, county == county_selected)
# }

#redo with different states
#filter testing data by county function
county_data <- function(state, county_selected) {
  if(state == "NY") {
    filter(newyork, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  } else if(state == "TX") {
    filter(texas, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  } else if(state == "CA") {
    filter(california, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  } else if(state == "CT") {
    filter(connecticut, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  } else if(state == "MI") {
    filter(michigan, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  } else if(state == "MD") {
    filter(maryland, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  } else if(state == "OH") {
    filter(ohio, county == county_selected) %>%
      mutate(seven_day_avg = rollmean(new_positives, 7, fill = NA))
  }
}
county_data_long <- function(state, county_selected) {
  if(state == "NY") {
    filter(newyork_long, county == county_selected)
  }
  
}

newyork_wide <- newyork %>%
  select(test_date, county, new_positives, total_number_of_tests_performed) %>%
  mutate(new_negatives = total_number_of_tests_performed - new_positives) %>%
  select(test_date, county, positive = new_positives, negative = new_negatives)

newyork_long <- newyork_wide %>%
  pivot_longer(
    cols = c(positive, negative),
    names_to = "result"
  )

#get latest date


bg_color = "#FDFDFD"
counties_theme <- theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    strip.background = element_rect(fill = bg_color),
    #axis.text.x = element_blank(),
    legend.position = "none",
    axis.line.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    panel.grid.major.y = element_line( colour = "darkgray"),
    plot.subtitle = element_text(size = 18),
    plot.title = element_text(size = 22),
    plot.caption = element_text(size = 12)
  )

counties_theme_stacked <- theme_wsj() +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    strip.background = element_rect(fill = bg_color),
    #axis.text.x = element_blank(),
    legend.position = "top",
    #legend.key.size = unit(0.2, "npc"),
    legend.title = element_blank(),
    #legend.direction = "vertical",
    legend.background = element_rect(fill = bg_color),
    axis.line.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    panel.grid.major.y = element_line( colour = "darkgray"),
    plot.subtitle = element_text(size = 18),
    plot.title = element_text(size = 22),
    plot.caption = element_text(size = 12)
  )
#dark theme
dark_theme <- dark_theme_bw() +
  theme(
    
    plot.subtitle = element_text(size = 14),
    plot.title = element_text(size = 18),
    plot.caption = element_text(size = 12)
  )

dark_theme_stacked <- dark_theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.subtitle = element_text(size = 14),
    plot.title = element_text(size = 18),
    plot.caption = element_text(size = 12)
  )


#write a state data function
state_data <- function(state) {
  if(state == "NY") {df = newyork}
  else if(state == "CA") {df = california}
  else if(state == "CT") {df = connecticut}
  else if(state == "MI") {df = michigan}
  else if (state == "TX") {df = texas}
  else if(state == "MD") {df = maryland}
  else if(state == "OH") {df = ohio}
  df %>%
    group_by(test_date) %>%
    mutate(total = sum(new_positives)) %>%
    select(test_date, total) %>%
    unique()
}

#function for most recent day
recent_day <- function(state_name) {
  if(state_name == "NY") {
    max(newyork$test_date)
  } else if(state_name == "CT") {
    max(connecticut$test_date)
  }else if(state_name == "CA") {
    max(california$test_date)
  }else if(state_name == "MD") {
    max(maryland$test_date)
  }else if(state_name == "MI") {
    max(michigan$test_date)
  } else if(state_name == "OH") {
    max(ohio$test_date)
  }else if(state_name == "TX") {
    max(texas$test_date)
  }
}




#14 day total new cases for each county
fourteenday_counties <- function(db) {
  
  db %>% filter(test_date > (recent_day("NY") - 14)) %>%
    group_by(county) %>%
    arrange(test_date, .by_group = TRUE) %>%
    mutate(fourteen_day_total = sum(new_positives)) %>%
    select(county, fourteen_day_total) %>%
    unique() 
}

fourteenday_counties(newyork)

#prev_14day counties
prev_14day_counties <- function(db) {
  db %>% filter(test_date > (recent_day("NY") - 28) & test_date < (recent_day("NY") - 13)) %>%
    group_by(county) %>%
    arrange(test_date, .by_group = TRUE) %>%
    mutate(prev_14_day_total = sum(new_positives)) %>%
    select(county, prev_14_day_total) %>%
    unique() 
}


#needs to call fourteenday_counties and prev_14day
fourteenday_summary <- function(db) {
  current <- fourteenday_counties(db)
  previous <- prev_14day_counties(db)
  
  df <- current %>% 
    left_join(previous, by = "county") %>%
    mutate(difference = fourteen_day_total - prev_14_day_total,
           pct_chg = difference / prev_14_day_total) %>%
    select(county, fourteen_day_total, prev_14_day_total, pct_chg) %>%
    arrange(desc(fourteen_day_total))
  return(df)
}

# for county date table
ny_counties <- as.character(unique(newyork$county))

#write function to make county data

ny_county <- function(selected_county) {
  newyork %>%
    filter(county == selected_county) %>%
    # mutate(seven_day_pos = rollmean(new_positives, 7, fill = NA),
    #        seven_day_pct = rollmean(pct_pos_today, 7, fill = NA)) %>%
    select(county, test_date, new_positives, pct_pos_today)
}

all_counties <- ny_counties %>% map(function(x) ny_county(x))
names(all_counties) <- ny_counties