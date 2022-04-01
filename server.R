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
library(riskRegression)
library(lubridate)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #Update allowed observation dates to be within 3 years prior of the selected diagnosis date
    observe({
      updateDateInput( 
        session = getDefaultReactiveDomain(),
        inputId = "date1",
        label = "Date",
        value = input$date0-1*365.25,
        min = input$date0 - 3*365.25,
        max = input$date0 -1
      )
      updateDateInput( 
        session = getDefaultReactiveDomain(),
        inputId = "date2",
        label = "Date",
        value = input$date0-2*365.25,
        min = input$date0 - 3*365.25,
        max = input$date0 -1
      )
    })
  
  
    #Define slope group cutoffs, which were calculated in 53RandomEffectsAnalysis.r
    slopecuts <- c(0.3176038,0.4421347)
    
    #Get parameter estimates from the competing risk model
    load( file = "55crfit")
    
    #Make reactive input - ie. allow user to select number of alc observations
    observeEvent( input$add,{
        insertUI(
            selector = "#add",
            where = "afterEnd",
            ui = fluidRow(
                column( 1),
                column( 5, fluidRow( dateInput( paste0("date",input$add+2),"Date", value = Sys.Date()-2*365.25))),
                column( 1),
                column( 3, fluidRow( numericInput( paste0("alc",input$add+2),"ALC", value = 3, min = 0.001, max = 860)))
            ) 
            
        )
    })
    
    # Collect user input data in a tibble
    df  <- reactive({
        tibble(
            ipi = input$ipi,
            sex = input$sex,
            datediag = input$date0,
            datealc = as.Date(sapply( 1:(input$add+2), FUN = function(ii){ input[[paste0("date",ii)]]}), origin = "1970-01-01"),
            alc = sapply( 1:(input$add+2), FUN = function(ii){ input[[paste0("alc",ii)]]})
         ) %>% 
         mutate(
            gender = ifelse( sex == "female", "Kvinde","Mand"),
            #timedays = as.numeric( datealc - datediag),
            #time = timedays/365.25,
            time = decimal_date(datealc)-decimal_date(datediag),
            measurementDate = as.character(datealc),
            diagnosisDate = as.character(datediag)
         ) %>%   
        filter( -3 <= time , time <= 0)
    })   
    
    dffit <- reactive({ lm( log(alc) ~ time, data = df()) })
    
    int <- reactive({ coef(dffit())["(Intercept)"] })
    slope <- reactive({ coef(dffit())["time"] })
    slopegrp <- reactive({
        ifelse( slope() <= slopecuts[1], "Low slope", 
                          ifelse( slope() <= slopecuts[2], "Medium slope",
                            ifelse( slope() > slopecuts[2], "High slope", "")))    
    })
    
    
    output$dataTable <- renderTable({
        df() %>% 
        select( ipi, sex, diagnosisDate, measurementDate, time, alc)
        
    })
    
    output$alcPlot <- renderPlot({
        
        #Plot parameters
        ymax <- max( df() %>% pull(alc) %>% max(), exp(int()))
        
        pbase <- tibble( 
            time = c(-3,0),
            newint = int()-3*slope()
        )
        low <- pbase %>% 
            mutate(
                minlymf = 0.01,
                maxlymf = exp( newint + slopecuts[1]*(time+3)),
                slopegrp = "Low slope"
            )
        medium <- pbase %>% 
            mutate(
                minlymf = exp(newint + slopecuts[1]*(time+3)),
                maxlymf = exp(newint  + slopecuts[2]*(time+3)),
                slopegrp = "Medium slope"
            )
        high <- pbase %>% 
            mutate(
                minlymf = exp(newint  + slopecuts[2]*(time+3)),
                maxlymf = exp(ymax),
                slopegrp = "High slope"
            )
        pgroup <- bind_rows( low, medium, high)
        
        pobs <- tibble( time = c(-3,0)) %>% 
            mutate(
                alc = exp(int()  + slope()*time)
            )
        
        #Plot
        ggplot( df(), aes( x = time))+
            geom_point( aes( y = alc, shape = "Observations"))+
            geom_line( data = pobs, aes( y = alc, linetype = "Estimated trajectory"))+
            geom_ribbon( data = pgroup, aes( ymin = minlymf, ymax = maxlymf, fill = slopegrp), alpha = 0.3)+
            coord_cartesian( xlim = c(-3,0), ylim = c(0.01,ymax))+
            guides( fill = guide_legend(title = NULL),  linetype = guide_legend(title = NULL),  shape = guide_legend(title = NULL))+
            scale_y_log10()+
            theme_bw()+
            xlab("Time before diagnosis in years")+
            ylab( expression(ALC~(10^9~cells/L)))+
            theme(legend.position = "bottom") 
        

    })
    
    output$eventPlot <- renderPlot({
        
    
      #Plot parameters
      ptimes <- seq( 0, 5, by = 0.01)
      newdata <- df() %>% 
          slice(1) %>% 
          select( gender, ipi) %>% 
          mutate( 
              slopegrp = factor( slopegrp(), levels = c("Low slope","Medium slope","High slope"))
          )
      
      p1 <- predict(crfit, times = ptimes, cause = 1, newdata = newdata, band = T)
      p2 <- predict(crfit, times = ptimes, cause = 2, newdata = newdata, band = T)
      
      pd <- rbind(
        cbind(
          newdata,
          data.frame(
            cause = "1",
            time = ptimes,
            pred = unlist(p1["absRisk"]),
            cblow = unlist(p1["absRisk.lowerBand"]),
            cbupp = unlist(p1["absRisk.upperBand"])
          )
        ), 
        cbind(
          newdata,
          data.frame(
            cause = "2",
            time = ptimes,
            pred = unlist(p2["absRisk"]),
            cblow = unlist(p2["absRisk.lowerBand"]),
            cbupp = unlist(p2["absRisk.upperBand"])
          )
        ) 
      )
      
      
      #Plot
      pd %>% 
        ggplot( aes( time, pred))+
        geom_step( aes(col = cause)) +
        geom_ribbon( aes(ymin = cblow, ymax = cbupp, fill = cause), alpha = 0.2)+
        theme_bw() +
        scale_color_manual(
          breaks = c("1", "2"), 
          labels = c("Death", "Treatment"),
          values = c("#FF7F0EFF", "black"), 
          name = "Event"
        ) +
        scale_fill_manual(
          breaks = c("1", "2"), 
          labels = c("Death", "Treatment"),
          values = c("#FF7F0EFF", "black"), 
          name = "Event"
        ) +
        theme(legend.position = "bottom") + 
        ylab("Probability of event (95% confidence bands)")+
        #guides( col = F)+
        coord_cartesian( ylim = c(0,1), xlim = c(0,5)) + 
        xlab("Time from diagnosis in years")
      
        
        
    })

})
