#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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
    "Third Page", # label for the tab in the navbar
    titlePanel("THOMAS PAGE"), # show with a displayed title
    
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
    

    ### SOPHIA CODE ####
}

# Run the application 
shinyApp(ui = ui, server = server)
