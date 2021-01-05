library(shiny)
library(shinythemes)
library(dplyr)
library(stringr)
library(leaflet)
library(footprint)
library(airportr)
library(geosphere)
library(maps)
library(dqshiny)


# list of airports ----

airports <- airportr::airports %>%
  select(IATA, Name) %>%
  mutate(name2 = paste(IATA, " - ", Name)) %>%
  select(name2) %>% pull()

# list of cities ----

citydata <- maps::world.cities %>%
  mutate(name2 = paste(name, ", ", country.etc)) %>%
  select(name2) %>% pull()
