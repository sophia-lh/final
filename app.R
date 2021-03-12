#
library(shiny)
library(tidyverse)
library(openintro) 
library(leaflet) 
library(ggplot2)
library(plotly)
library(devtools)
library(maps)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(stringr) 
data(county.regions)


page_one <-tabPanel(
  "Introduction", # label for the tab in the navbar
  titlePanel("Page 1"),
  fluidPage(
    
    h3("blah blah blah blah blah")
    
  )
)

page_two <- tabPanel( ##### AUDREYS PAGE
  "Second Page", # label for the tab in the navbar
  titlePanel("AUDREY PAGE"), # show with a displayed title
  
  # This content uses a sidebar layout
  sidebarLayout(
    sidebarPanel(
      h3("sidebar panel...")
    ),
    mainPanel(
      h3("Primary Content"),
      p("Plots, data tables, etc. would go here")
    )
  )
)

# Define content for the second page
page_three <- tabPanel( #### THOMAS PAGE
  "Beliefs", # label for the tab in the navbar
  titlePanel("How Do Individual Beliefs Effect COVID Cases?"), # show with a displayed title
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("radio", label = h3("Compare cases to..."),
                   choices = list("% of people who would get vaccine right now" = "vax_acceptance", "% of people afraid of getting sick" = "worried_sick"), 
                   selected = "vax_acceptance"),
      
      selectInput("changestate", label = h3("Change the State"), 
                  choices = list("all" = "all", "Alabama" = "Alabama",
                                 "Alaska" ="Alaska" ,
                                 "Arizona" = "Arizona",
                                 "Arkansas" = "Arkansas" ,
                                 "California" = "California" ,
                                 "Colorado" = "Colorado",
                                 "Connecticut" = "Connecticut",
                                 "Delaware" = "Delaware",
                                 "Florida" = "Florida",
                                 "Georgia" = "Georgia",
                                 "Hawaii" = "Hawaii",
                                 "Idaho" = "Idaho",
                                 "Illinois" = "Illinois",
                                 "Indiana" = "Indiana",
                                 "Iowa" = "Iowa",
                                 "Kansas" = "Kansas",
                                 "Kentucky" = "Kentucky",
                                 "Louisiana" = "Louisiana",
                                 "Maine" = "Maine",
                                 "Maryland" = "Maryland",
                                 "Massachusetts" = "Massachusetts",
                                 "Michigan" = "Michigan",
                                 "Minnesota" = "Minnesota",
                                 "Mississippi" = "Mississippi",
                                 "Missouri" = "Missouri",
                                 "Montana" = "Montana" ,
                                 "Nebraska" = "Nebraska" ,
                                 "Nevada" = "Nevada" ,
                                 "New Hampshire" = "New Hampshire",
                                 "New Jersey" = "New Jersey",
                                 "New Mexico" = "New Mexico"  ,
                                 "New York" = "New York" ,
                                 "North Carolina" = "North Carolina",
                                 "North Dakota" = "North Dakota",
                                 "Ohio" = "Ohio",
                                 "Oklahoma" = "Oklahoma",
                                 "Oregon" = "Oregon",
                                 "Pennsylvania" = "Pennsylvania",
                                 "Rhode Island" = "Rhode Island",
                                 "South Carolina" = "South Carolina" ,
                                 "South Dakota" = "South Dakota",
                                 "Tennessee" = "Tennessee",
                                 "Texas" = "Texas" ,
                                 "Utah" = "Utah",
                                 "Vermont" = "Vermont",
                                 "Virginia" = "Virginia",
                                 "Washington" = "Washington",
                                 "West Virginia" = "West Virginia",
                                 "Wisconsin" = "Wisconsin",
                                 "Wyoming" = "Wyoming"),
                  selected = "1"
      ),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("myPlot")
    )
  )
)



# Define content for the third page
state_names <- unique(county.regions$state.name)

state_input <- selectInput(
  inputId = "state",
  choices = state_names,
  label = "Choose a state below to start exploring:"
)

page_four <- tabPanel( ##### SOPHIA PAGE
  "Behavior", # label for the tab in the navbar
  titlePanel("How are individual behaviors reflected in COVID-19 cases?"), # show with a displayed title
  state_input,
  plotlyOutput(outputId = "rest_map"),
  plotlyOutput(outputId = "mask_map"),
  plotlyOutput(outputId = "covid_map")
)



page_five <-tabPanel(
  "Conclusion", # label for the tab in the navbar
  titlePanel("Page 5"),
  fluidPage(
    
    h3("blah blah blah blah blah")
    
  )
)

# Pass each page to a multi-page layout (`navbarPage`)
ui <- navbarPage(
  "My Application", # application title
  page_one,         # include the first page content
  page_two,         # include the second page content
  page_three,        # include the third page content
  page_four, 
  page_five
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #### JACK CODE ####
  
  
  #### AUDREY CODE ####
  
  
  #### THOMAS CODE ####
  output$myPlot <- renderPlotly({
    
    ## WRANGLING
    
    #calling data
    
    vax_data <- read.csv("vax_data.csv")
    worried_sick_data <- read.csv("worried_sick_data.csv")
    nyt_covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
    
    # collecting whats important 
    
    worried_sick_data <- worried_sick_data %>% 
      select(geo_value,time_value,value)
    
    vax_data <- vax_data %>% 
      select(geo_value,time_value,value)
    
    nyt_covid <-nyt_covid %>% 
      select(state,cases,date,fips)
    
    #making MIT sick/vax data not abbreviated
    
    worried_sick_data$geo_value <-  abbr2state(worried_sick_data$geo_value)
    vax_data$geo_value <-  abbr2state(vax_data$geo_value)
    
    #naming different values before joining
    
    names(worried_sick_data)[1] <- "state"
    names(worried_sick_data)[2] <- "date"
    names(worried_sick_data)[3] <- "worried_sick"
    names(vax_data)[1] <- "state"
    names(vax_data)[2] <- "date"
    names(vax_data)[3] <- "vax_acceptance"
    
    #rounding to be cleaner before joining
    
    vax_data$vax_acceptance <- round(vax_data$vax_acceptance)
    worried_sick_data$worried_sick <- round(worried_sick_data$worried_sick)
    
    ## merging data sets
    vax_and_sick <- left_join(vax_data, worried_sick_data, by = c("state", "date"))
    vax_sick_cases_data <- left_join(vax_and_sick, nyt_covid, by = c("state","date"))
    
    ## GRAPHING
    
    # change state
    if(input$changestate == "all"){
      vax_sick_cases_data <- vax_sick_cases_data %>% 
        filter(state == state)
    }else{
      vax_sick_cases_data <- vax_sick_cases_data %>% 
        filter(state == input$changestate)
    }
    
    
    
    g <- ggplot(data = vax_sick_cases_data) +
      geom_point(mapping = aes(x = .data[[input$radio]], y = cases, color=cases)) +
      geom_smooth(mapping = aes(x = .data[[input$radio]],y=cases, color = cases))+
      labs(x = input$radio, y = "COVID cases") 
    
    ggplotly(g)
    
    
    
  })
  
  
  ### SOPHIA CODE ####
  output$rest_map <- renderPlotly({
    
    restaurant_data <- read.csv("rest_data.csv")
    restaurant_data
    
    new_rest <- restaurant_data %>% 
      group_by(geo_value) %>% 
      summarize(geo_value, value = mean(value)) %>% 
      distinct(geo_value, value) %>% 
      mutate(region = as.numeric(str_remove(geo_value, "^0+"))) 
    
    
    rest_map <- county_choropleth(new_rest,
                                  title = "Proportion of Respondents Who Visited a Bar, Restaurant, or Cafe in the Past 24 Hours",
                                  legend = "Proportion of Respondents",
                                  num_colors = 1,
                                  state_zoom = input$state)
    
    ggplotly(rest_map)
    
    
  })
  
  output$mask_map <- renderPlotly({
    
    
    
    mask_data <- read.csv("mask_data.csv")
    mask_data
    
    new_mask <- mask_data %>% 
      group_by(geo_value) %>% 
      summarize(geo_value, value = mean(value)) %>% 
      distinct(geo_value, value) %>% 
      mutate(region = as.numeric(str_remove(geo_value, "^0+"))) 
    
    
    
    mask_map <- county_choropleth(new_mask,
                                  title = "Percentage of People Who Report Wearing a Mask Most or All of the Time in Public",
                                  legend = "Pecentage of Population",
                                  num_colors = 1,
                                  state_zoom = input$state)
    
    ggplotly(mask_map)
  })
  
  output$covid_map <- renderPlotly({
    
    
    
    covid_data <- read.csv("covid_cast_data.csv")
    covid_data
    
    
    new_covid <- covid_data %>% 
      group_by(geo_value) %>% 
      summarize(geo_value, value = mean(value)) %>% 
      distinct(geo_value, value) %>% 
      mutate(region = as.numeric(str_remove(geo_value, "^0+"))) 
    
    
    
    covid_map <- county_choropleth(new_covid,
                                   title = "Cumulative Reported COVID-19 Cases per 100,000 People",
                                   legend = "Cases per 100,000 People",
                                   num_colors = 1,
                                   state_zoom = input$state)
    
    ggplotly(covid_map)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
