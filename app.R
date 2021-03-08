#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(openintro) 
library(leaflet) 
library(ggplot2)
library(plotly)

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
    "Behavior", # label for the tab in the navbar
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
page_four <- tabPanel( ##### SOPHIA PAGE
    "Fourth Page", # label for the tab in the navbar
    titlePanel("SOPHIA PAGE "), # show with a displayed title
    
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
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
