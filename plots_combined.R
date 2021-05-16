
shinyApp(

  ui = fluidPage(theme = shinytheme("cerulean"),
                 titlePanel("Protest Events During 2020"),
                 inputPanel(
                   sliderInput("slider", "Dates:",
                               min = as.Date("2020-01-01","%Y-%m-%d"),
                               max = as.Date("2020-12-31","%Y-%m-%d"),
                               value = c(as.Date("2020-04-01"), as.Date("2020-04-30")),
                               timeFormat = "%Y-%m-%d"),
                   uiOutput("eventInput")

                 ),
                 mainPanel(plotlyOutput("sub_eventPlot",
                                        height = '250px')),
                 leafletOutput("event_map")
  ),

  server = function(input, output) {
    output$event_map <- renderLeaflet({
      leaflet(acled %>% filter(event_date <= input$slider[2] & event_date >= input$slider[1] & event_type == input$event)) %>%
        addTiles() %>%
        addCircles(lng = ~longitude, lat = ~latitude, label = ~lapply(lab, htmltools::HTML), popup = ~notes)

      output$eventInput <- renderUI({
        selectInput(
          inputId = "event",
          label = "Select an Event Type:",
          choices = levels(as.factor(acled$event_type)))})

      output$sub_eventPlot <- renderPlotly({
        ggplotly(ggplot(acled %>% filter(event_type == input$event), aes(x = sub_event_type)) + geom_bar() +
                   labs(x = 'Sub Event Type',
                        y = 'Count',
                        title = 'Count of Sub-Event Type'),
                 tooltip = 'y')})
      })
    },

    options = list(height = 550)
    )
    ```
