library(readr)
library(dplyr)
library(ggplot2)

acled <- read_csv('./US-ACLED.csv')
head(acled)

# keep only 2020 events
acled <- acled %>% filter(year == 2020)

# format date
acled$event_date <- as.Date(acled$event_date, '%d-%b-%y')

ggplot(acled, aes(x = event_type)) + geom_bar() +
  labs(x = 'Event Type',
       y = 'Count',
       title = 'Protests Were the Most Common Event in 2020')

# if we filter this by event type then we can see the sub event type
ggplot(acled %>% filter(event_type == ''), aes(x = sub_event_type)) + geom_bar() +
  labs(x = 'Sub Event Type',
       y = 'Count')



# drill down sub event type

library(shiny)

ui <- shinyUI(
  fluidPage(
    titlePanel("Event Sub-Types"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("eventInput")
      ),
      mainPanel(
        plotOutput("sub_eventPlot")
      )
    )
  ))

server <- shinyServer(function(input, output) {

  output$eventInput <- renderUI({
    selectInput(
      inputId = "event",
      label = "Select an Event Type:",
      choices = levels(as.factor(acled$event_type))
    )
  })

  output$sub_eventPlot <- renderPlot({
    ggplot(acled %>% filter(event_type == input$event), aes(x = sub_event_type)) + geom_bar() +
      labs(x = 'Sub Event Type',
           y = 'Count')
  })
})

shinyApp(ui = ui, server = server)



Here we can see a wide variety of different protest events in the US in 2020; the vast majority were peaceful protests, but we also see a large number of protests with intervention and violent demonstrations.


### time series

library(lubridate)

acled_time <- acled %>% group_by(admin1, event_date) %>% summarize(count = n())


# view by state


library(shiny)

ui <- shinyUI(
  fluidPage(
    titlePanel("State"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("stateInput")
      ),
      mainPanel(
        plotOutput("statePlot")
      )
    )
  ))

server <- shinyServer(function(input, output) {

  output$stateInput <- renderUI({
    selectInput(
      inputId = "state",
      label = "Select a State:",
      choices = levels(as.factor(acled_time$admin1))
    )
  })

  output$statePlot <- renderPlot({
    ggplot(acled_time %>% filter(admin1 == input$state), aes(x = event_date, y = count)) + geom_line() +
      labs(x = 'Event Date',
           y = 'Count')
  })
})

shinyApp(ui = ui, server = server)

# notable things: Minnesota and Georgia (location of murder of George Floyd and Ahmaud Arbery, respectively)
# peak earlier in the summer than most other states
# DC has higher concentration of protests in fall (election) but fewer overall than most states

