#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Prognosis for chronic lymphocytic leukemia patients"),
    #"This website calculates a prognosis for treatment or death without treatment for newly diagnosed CLL patients based on their prediagnostic lymphocyte trajectories"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Patient characteristics"),
            sliderInput("ipi",
                        "IPI",
                        min = 0,
                        max = 10,
                        value = 1),
            selectInput("sex",
                        "Sex",
                        choices = list("Female"="female", "Male"="male"), 
                        selected = "Kvinde"),
            dateInput("date0","Date of diagnosis/index date before treatment"),
            h4(HTML(paste0(paste0("ALC (10",tags$sup("9"))," cells/L)"))),
            helpText("Only ALC measured within three years prior to diagnosis/index are used"),
            actionButton("add","Add observations"),
            fluidRow(
                column( 1),
                column( 5, 
                    fluidRow( dateInput("date1","Date", value = Sys.Date()-1*365.25)),
                    fluidRow( dateInput("date2","Date", value = Sys.Date()-2*365.25))
                 ),  
                 column( 1),
                 column( 3, 
                    fluidRow( numericInput("alc1","ALC", value = 6, min = 0.001, max = 860)),
                    fluidRow( numericInput("alc2","ALC", value = 3, min = 0.001, max = 860))
                 )      
            
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel( type = "tabs",
                tabPanel("Data",tableOutput("dataTable")),
                tabPanel("Plot of prediagnostic ALC",plotOutput("alcPlot")),
                tabPanel("Plot of risk of treatment or death",plotOutput("eventPlot"))
            )    
        )
    )
))
