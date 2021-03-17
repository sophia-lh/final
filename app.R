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
library(scales)
data(county.regions)


page_one <-tabPanel(
  "Introduction", # label for the tab in the navbar
  fluidPage(
    div(
    h3("Authors"),
    p("Audrey Dennis, Sophia Lowe-Hines, Jack Sherman, Thomas Wilson"),
    p(br()),
    h3("Research Context"),
    p("Over the past year, our social norms and mores have changed drastically in response to the global public health crisis of COVID-19. Quotidian gestures such as the handshake seem now a kind of archaic imprudence resigned to the back-then and keeping up to date on current events seems to necessitate becoming an amateur epidemiologist. However, as the imperfect U.S. response to the pandemic has shown, opinions concerning everything from proper shutdown stringency to mask-wearing and vaccine safety can differ greatly. Indeed, last year has shown that normal epidemiological proscriptions have not adequately addressed the difficult-to-quantify social irregularities that directly influence the spread of the disease. More often than not, discussions around preventative measures and epidemiological policies do not consider the socio-political sentiments, social practices, and personal beliefs that affect the efficacy of disease control at the community level. In spike forecasting, mandating preventative guidelines, and planning vaccine distribution, understanding local sentiments and beliefs about COVID-19 is crucial."),
    p(br()),
    h3("Project Description"),
    p("In this project, the authors hope to provide a dynamic visual exploration of how local social practices, beliefs, and sentiments concerning the pandemic might influence the spread and death toll of the disease in the U.S. at the county and state levels. While misinformation and conspiracism have become central to U.S. politics, the extent to which COVID-19 denialism and misinformation have influenced the spread of the disease remains unclear. Through interactive visualizations, the viewer can easily choose particular relationships to better understand links between localized opinions and public health metrics."),
    p(br()),
    h3("Datasets"),
    p("This project uses the Carnegie Mellon University Delphi Group research, specifically the Delphi's COVID-19 Survey data. This is a current geographic location tagged social media survey in which participants report personal and local social-distancing practices, personal beliefs concerning vaccines, and mental health information. Additionally, this project uses the New York Times COVID-19 data which provides continuously updated data on coronavirus spread and death counts at the county, state, and national levels."),
    p(br())
    )
  )
)
# Define content for third page
anxious_data <- read.csv("anxious_data.csv")
depressed_data <- read.csv("depressed_data.csv")
isolated_data <- read.csv("felt_isolated_data.csv")

page_two <- tabPanel( ##### AUDREYS PAGE
  "Emotions", # label for the tab in the navbar
  titlePanel("How are individual emotions related to COVID rates"), # show with a displayed title
  
  # This content uses a sidebar layout
  sidebarLayout(
    sidebarPanel(
      selectInput("states", "Choose a State", unique(depressed_data$geo_value), selected = "ak"), 
    ),
    mainPanel(
      plotlyOutput("my_scatterplot"),
      div(
        p(br()),
        p("Plot of responses to survey about COVID-induced mental health difficulties over time (since Jan 2021). The green curve represents the percentage of participants who felt isolated, the red curve for those who felt anxious, and the blue curve for those who felt depressed.")
      )
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
      plotlyOutput("myPlot"),
      
      div(
        p(br()),
        p("Plots the percentage of participants afraid of getting sick/the percentage of participants who would receive a COVID-19 vaccine against cases by U.S. state.")
      )
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
    div( p(br()),
    p("Maps percentage of participants who visited social eateries, percentage of participants who wear a mask in public, and COVID cases by county for each U.S. state. Please allow for load-time."),
    p(br())
  ),
  state_input,
  plotlyOutput(outputId = "rest_map"),
  plotlyOutput(outputId = "mask_map"),
  plotlyOutput(outputId = "covid_map")
)



page_five <-tabPanel(
  "Conclusion", # label for the tab in the navbar
  fluidPage(
    div( 
      h3("Emotions Visual"),
      p("Since January 2021, part of the Delphi research has been a daily social media survey on participants' mental health and coping with the pandemic by state. Overall, most states exhibit downward trends but appear to have relatively higher rates in locations with recent spikes (NY, NJ, etc). The link between mental health and pandemic-influenced stress is self-evident, but not apparently useful as an indicator of spread. However, being able to represent localized sentiments and to gauge the mental health effects of containment measures at the community level may be of value to researchers attempting to predict the broader local sentiments that do affect COVID's spread. Furthermore, the results of the survey appear promising. Texas's trends, for instance, suggest that the survey data is responsive, if not accurate, as the three curves spike suddenly around the time of the February 2021 Texas power crisis. While it would not be reasonable to conclude that these exact measures of mental distress affect spread, the authors argue that the data could be indicative of other, more useful metrics."),
      p(br()),
      h3("Beliefs Visual"),
      p("Plots for prevalence of vaccine acceptance and cases by state appear to generally follow a decreasing trend. As survey sentiment scores for vaccine acceptance increase, the county case numbers generally decrease. While this relationship is promising, the potential of a particular sentiment to be used as an indicator for spread MUST be tempered with an effort to avoid an overly reductionist approach to the complex relationship (if indeed it exists) between community beliefs and case counts. That is, these models do not imply that the existence of antivax sentiments in a community necessarily mean higher cases. Rather, it implies that the metric of one particular sentiment might be indicative of a larger host of beliefs that do influence local spread. "),
      p(br()),
      h3("Behavior Visual"),
      p("Through comparing covid cases by county with survey behavior data by county, certain patterns do emerge. Taking the state of California as an example, one sees that many of the counties reporting lower relative masking rates are also the counties with the higher reported cases. As masking is a proven measure against spread, the objective of this comparison is not to reiterate a known, but rather to draw attention to the correlation between the survey results and the COVID  cases. For California, the survey results for masking practices appear to be weakly correlated with case numbers and might have potential as an indicator. Unfortunately, for many states, there is weak to moderate correlation between masking and restaurant visit survey data and case numbers. Additionally, this, as well as the prevalence of missing county data, renders the relationships difficult to trust. Thus, for these comparisons, conclusions should be limited to flagging elements for further analysis."),
      p(br())
    )
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
  output$my_scatterplot <- renderPlotly({
    state <- input$states
    # prepare data for anxiety
    data1 <- anxious_data %>%
      filter(geo_value == state)
    data1$time_value <- as.Date(data1$time_value)
    #prepare data for depressed
    data2 <- depressed_data %>%
      filter(geo_value == state)
    data2$time_value <- as.Date(data2$time_value)
    
    # prepare data for isolated
    data3 <- isolated_data%>%
      filter(geo_value == state)
    data3$time_value <- as.Date(data3$time_value)
    
    scatterplot<-ggplot()+
      geom_line(data=data1, mapping = aes(x = time_value, y=value, label = "Anxious"), color = "red")+
      geom_line(data=data2, mapping = aes(x = time_value, y=value, label = "Depressed"), color = "blue")+
      geom_line(data=data3, mapping = aes(x = time_value, y=value, label = "Isolated"), color = "green")+
      scale_x_date(labels = date_format("%m-%Y")) +
      scale_fill_discrete(labels = c("Control", "red" = "Anxiety", "Treatment 2")) +
      labs(y = "% Participants Feeling",
           x = "Date")

    ggplotly(scatterplot)
  }) 
  
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
