#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##################################################
# load packages
##################################################

library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(RCurl)
library(shinythemes)
library(rsconnect)

##################################################
# Import and modify data
##################################################

##################################################
# Load two csv's that I created: state population (from census), and state order dates (from New York Times)
##################################################

# State population data from 2010 census
state_pops <- filter(select(read_csv("us_state_pops_2010.csv"),
                            StateNum = STATE, 
                            Province_State = NAME, 
                            Population_2010 = CENSUS2010POP), 
                     StateNum != 0,
                     Province_State != "District of Columbia",
                     Province_State != "Puerto Rico")

# Load dates of stay at home orders by US states. From https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html
lockdown <- select(mutate(read_csv("US_stay_at_home_orders.csv"), 
                          date = mdy(Order_Date)), -Order_Date)

##################################################
# Load US COVID19 infection and death data from JHU GitHub Repositories
##################################################

# United States COVID 19 Infection Data
us_covid <- read_csv(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
us_covid <- us_covid %>%
    gather(date, cases, 12:ncol(us_covid)) %>%
    mutate(date = mdy(date))

# Consolidate US counties COVID 19 data into state data
us_covid_states <- mutate(us_covid %>%
                              group_by(Province_State, date) %>%
                              summarize(cases = sum(cases)),
                          new_cases = cases - lag(cases))

# Load and manipulate death count data as I did for case data above
us_covid_deaths <- read_csv(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
us_covid_deaths <- us_covid_deaths %>%
    gather(date, deaths, 13:ncol(us_covid_deaths)) %>%
    mutate(date = mdy(date))

us_covid_deaths_states <- mutate(us_covid_deaths %>%
                                     group_by(Province_State, date) %>%
                                     summarize(deaths = sum(deaths)),
                                 new_deaths = deaths - lag(deaths))

#Add death count to us_covid_states dataframe, and remove American Samoa, DC, and Puerto Rico so that data only includes states
us_covid_states <- filter(left_join(us_covid_states, 
                             us_covid_deaths_states, 
                             by = c("Province_State", 
                                    "date")),
                          Province_State != "American Samoa",
                          Province_State != "District of Columbia",
                          Province_State != "Puerto Rico",
                          Province_State != "Guam",
                          Province_State != "Diamond Princess",
                          Province_State != "Grand Princess",
                          Province_State != "Northern Mariana Islands",
                          Province_State != "Virgin Islands")

##################################################
# Add state population data into the us_covid_states dataframe and use that to create per capita infections, new infections, deaths, and new deaths
# Isolate infection and death data on the date of stay at home orders into its own data frame, then add that back to us_covid_states (there may be a more efficient way to do this, but this is the way that flowed logically as I was writing this)
##################################################

us_covid_states <- mutate(select(full_join(us_covid_states, 
                                           state_pops, 
                                           by = "Province_State"),
                                 -StateNum), 
                          Population_2010_m = Population_2010/1000000,
                          cases_pc = cases/Population_2010_m,
                          new_cases_pc = new_cases/Population_2010_m,
                          deaths_pc = deaths/Population_2010_m,
                          new_deaths_pc = new_deaths/Population_2010_m)

# Add Covid case and death data from us_covid_states dataframe corresponding to the specific dates of stay at home orders
lockdown_data <- select(semi_join(us_covid_states, 
                                  lockdown, 
                                  by = c("Province_State", "date")),
                        Province_State,
                        "lockdown_date" = date,
                        "cases_at_lockdown" = cases,
                        "deaths_at_lockdown" = deaths,
                        "new_cases_at_lockdown" = new_cases,
                        "new_deaths_at_lockdown" = new_deaths,
                        "cases_pc_at_lockdown" = cases_pc,
                        "deaths_pc_at_lockdown" = deaths_pc,
                        "new_cases_pc_at_lockdown" = new_cases_pc,
                        "new_deaths_pc_at_lockdown" = new_deaths_pc)

#Add the lockdown data to us_covid_states data
us_covid_states <- left_join(us_covid_states, lockdown_data, 
                             by = "Province_State")

##################################################
#Create a lockdown_speed data frame that renames the column headings for four columns from the lockdown ranking dataframe.
#This is for the purpose of making a radable dataframe to include in the app ui
##################################################

lockdown_speed <- select(lockdown_data,
                         "State" = Province_State,
                         "Order Date" = lockdown_date,
                         "Cases (per million people) on order date" = cases_at_lockdown,
                         "Deaths (per million people) on order date" = deaths_at_lockdown)

##################################################
# Create character vectors to use in ui widgets and axes/legend labels
##################################################

data <- c("Total cases" = "cases", 
          "Daily new cases" = "new_cases",
          "Total deaths" = "deaths",
          "Daily new deaths" = "new_deaths",
          "Cases per million people" = "cases_pc", 
          "Daily new cases per million people" = "new_cases_pc",
          "Deaths per million people" = "deaths_pc",
          "Daily new deaths per million people" = "new_deaths_pc")

earliness_indicator <- c("COVID19 cases per million people" = 
                             "cases_pc_at_lockdown",
                         "COVID19-related deaths per million people" = 
                             "deaths_pc_at_lockdown")

##################################################
# Define UI
##################################################

shinyUI(navbarPage("", #navbarPage adds mmultiple tabs to your app, each with a separate ui defined under tabPanel()
                 
                 tabPanel("State Infection Curve", # this tab displays a sidebar layout. The sidebar contains four widgets: dateRangeInput(), two selectInput(), and radioButtons(). The main panel displays an explanation of the figure the graph. The code defining the graph is in the server.
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      # the functions used to generate text in the user interface use a kind of R-adapted html code: h#() generates a heading with h1() being the largest; strong() = boldface, br() = new line; p() = normal text; a() produces a hyperlink
                                      h4(strong("Graph Controls"),
                                         align = "center"),
                                      h5(strong("Use these controls to change the date range plotted on the x-axis, the data plotted on the y-axis, the state(s) to plot, and the indicator of infection status in each state on their respective date of state-issued stay at home orders.")),
                                      br(),
                                      dateRangeInput("range", "Date Range:",
                                                     start = "2020-03-01",
                                                     min = "2020-03-01"),
                                      # This allows the user to select the date range which wil be plotted on the x-axis
                                      
                                      selectInput("data",
                                                  "Select Data:",
                                                  choices = data,
                                                  selected = data[1]),
                                      # This allows the user to select the variable to plot on the y-axis
                                      
                                      selectInput("states",
                                                  "Select States:",
                                                  choices = 
                                                      c(us_covid_states$Province_State),
                                                  selected = c("California",
                                                               "Florida"),
                                                  multiple = TRUE),
                                      # This allows the user to  select the states to plot
                                      
                                      radioButtons("indicator",
                                                   "Compare states based on their number of ___________ on the date of state-issued stay at home order.",
                                                   choices = earliness_indicator,
                                                   selected = earliness_indicator[1])
                                      #This allows the user to select the earliness_indicator (which is the COVID19 infection status in each state on the date of the state-issued stay at home order).
                                  ),
                                  
                                  mainPanel(
                                      h1("US COVID19 Infection Curve by State",
                                         align = "center"),
                                      h4(strong("Will the states that issued stay at home orders earlier see a flatter COVID19 infection curve?"),
                                         align = "center"),
                                      p("Much of the U.S. public has been ordered to practice social distancing in order to slow the spread of COVID19.", a("Many U.S. states have issued 'stay at home orders',", href = "https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html"), "however, the timing of these orders have varied widely between states with respect to the extent of COVID19 infections at the time of the order. The interactive figure below is intended to allow the user to explore the relationship between the most current U.S. COVID19 infection data (provided by", a("Johns Hopkins University CSSE)", href = "https://github.com/CSSEGISandData/COVID-19"), "and the the timing of the states' stay at home orders. Use the table on the next tab to compare the infection status in each state on their respective stay at home order date."),
                                      br(),
                                      plotOutput("plot")
                                  )
                              )
                          )
                 ),
                 #This tab displays the lockdown speed ranking table plus information about the data sources
                 tabPanel("Stay at home orders",
                          # This tab displays some text at the top, and a data table underneath that displays the lockdown_speed data frame (data on the COVID19 infection status in each state on the date of state-issued stay at home order)
                          h1("U.S. State-Wide Stay At Home Orders"),
                          strong("This table displays the date each US state-issued stay at home orders, plus the number of confirmed cases per million people and the number of COVID19-related deaths per million people on the date of that order."),
                          br(),
                          br(),
                          p("Not all states have issued stay at home orders as of the last update of", a("this New York Times article.", href = "https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html"), "These states are not included in this table, but they can be viewed in the graph on the previous tab."),
                          br(),
                          # Data table was generated using DT package
                          DT::dataTableOutput("ranking")), 
                 # this changes the aesthetics of the entire app. The shinytheme packages includes some nice aesthetics
                 theme = shinytheme("flatly")
))
