#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(RCurl)
library(shinythemes)
library(rsconnect)

##################################################
# To publish this on shinyapps.io i needed to specify the environment variables in both the ui.R and server.R code
##################################################

state_pops <- filter(select(read_csv("us_state_pops_2010.csv"),
                            StateNum = STATE, 
                            Province_State = NAME, 
                            Population_2010 = CENSUS2010POP), 
                     StateNum != 0)

# Load the csv I created listing the dates of stay at home orders by US states. From https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html
lockdown <- select(mutate(read_csv("US_stay_at_home_orders.csv"), 
                          date = mdy(Order_Date)), -Order_Date)

#Load United States COVID 19 Infection Data from JHU CSSE Github repository
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

#Add death count to us_covid_states dataframe
us_covid_states <- left_join(us_covid_states, 
                             us_covid_deaths_states, 
                             by = c("Province_State", 
                                    "date"))

# Add state population data into the us_covid_states dataframe and use that to create per capita infections, new infections, deaths, and new deaths
# Remove American Samoa, DC, and Puerto Rico so that data only includes states
us_covid_states <- filter(mutate(select(full_join(us_covid_states, 
                                                  state_pops, 
                                                  by = "Province_State"),
                                        -StateNum), 
                                 Population_2010_m = Population_2010/1000000,
                                 cases_pc = cases/Population_2010_m,
                                 new_cases_pc = new_cases/Population_2010_m,
                                 deaths_pc = deaths/Population_2010_m,
                                 new_deaths_pc = new_deaths/Population_2010_m),
                          Province_State != "American Samoa",
                          Province_State != "District of Columbia",
                          Province_State != "Puerto Rico",
                          Province_State != "Guam",
                          Province_State != "Diamond Princess",
                          Province_State != "Grand Princess",
                          Province_State != "Northern Mariana Islands",
                          Province_State != "Virgin Islands")

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

#Create a lockdown_speed data frame that renames the column headings for four columns from the lockdown ranking dataframe.
#This is for the purpose of making a radable dataframe to include in the app ui
lockdown_speed <- select(lockdown_data,
                         "State" = Province_State,
                         "Order Date" = lockdown_date,
                         "Cases (per million people) on order date" = cases_at_lockdown,
                         "Deaths (per million people) on order date" = deaths_at_lockdown)

# Create character vectors to use in ui widgets and axes/legend labels
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

# Define server logic
shinyServer(function(input, output) {
    
    #produce the lockdown_speed data table for tab 2 in the ui using DT package
    output$ranking <- DT::renderDataTable({
        DT::datatable(lockdown_speed)
    })
    
    #produce the ggplot line graph based on the selection criteria in the side panel of tab 1
    output$plot <- renderPlot({
        
        #First I create a subset of the us_covid_states dataframe that only includes states that were selected by the user
        state_comp <- filter(us_covid_states, 
                             Province_State %in% c(input$states))
        
        #create a line graph of the states that were selected. In many places, user selections could be included directly in the figure code by input$____
        ggplot(state_comp, aes_string(x = "date", 
                                      y = input$data, 
                                      group = "Province_State",
                                      color = input$indicator)) +
            geom_line(size = 0.7) +
            scale_x_date(limits = ymd(c(input$range[1], 
                                        input$range[2] + 5.5))) +
            # To only label the end of the graph, I used the subset code with the tail() function to get just the final date/data combination. This way I dont have to update this code as the Johns Hopkins data updates.
            geom_text(data = subset(state_comp, 
                                    date == tail(state_comp$date , n = 1)),
                      aes_string(label = "Province_State", 
                                 x = tail(state_comp$date, n = 1) + 0.3, 
                                 y = input$data),
                      hjust = 0,
                      check_overlap = TRUE,
                      size = 4.2) +
            geom_point(data = subset(state_comp, date == lockdown_date),
                       aes_string(x = "lockdown_date", 
                                  y = input$data),
                       shape = "diamond",
                       size = 3.5) +
            theme_bw() +
            labs(caption = "Points represent the date of state-issued stay at home orders.\nStates that have not issued stay at home orders are plotted in gray.",
                 x = "Date", 
                 y = names(data[which(data == input$data)])) +
            # In the labs(y = ...) code above and (name = ...) code below, I got this code from stack exchange, which uses the names(), and which() functions to correctly label the axes and legend based on user input.
            scale_color_continuous(name = paste(names(earliness_indicator[
                which(earliness_indicator == input$indicator)]), "\non the date of state-issued stay at home order"),
                low = "blue",
                high = "red") +
            # guides() changes the dimensions of the legend
            guides(color = guide_colorbar(barwidth = 10, barheight = 1)) +
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14, face = "bold"),
                  panel.grid.minor = element_blank(),
                  legend.position = "top",
                  legend.title = element_text(size = 14),
                  legend.text = element_text(size = 10),
                  plot.caption = element_text(size = 14))
        
    })
    
})

