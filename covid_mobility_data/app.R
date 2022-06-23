#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(effects)

# Read in effects csv
us_mobility <- read_csv("data/us_place_effect_mobility.csv")
az_mobility <- read_csv("data/az_place_effect_mobility.csv")

year_options <- c(2020, 2021, 2022)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Google Mobility Data - COVID"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            p('Click on the tabs to choose different visualizations of the data.'),
            HTML('Data is from the <a href="https://www.google.com/covid19/mobility/">Google Community Mobility Reports</a>')
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Timeline",
                               selectInput("year_chosen",
                                           "Select Year to Highlight:",
                                           choices = year_options),
                        plotOutput("error_plot"),
                        plotOutput("error_plot_2")),
                      tabPanel("Map",
                               selectInput("year_chosen",
                                           "Select Year to Highlight:",
                                           choices = year_options),
                               plotOutput("map_plot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$error_plot <- renderPlot({
        data_to_plot <- az_mobility %>%
          group_by(year)

        chosen_year <- az_mobility %>%
          filter(year == input$year_chosen)

        data_to_plot %>%
          ggplot(aes(y = place,
                     x = fit,
                     xmax = upper,
                     xmin = lower,
                     group = year)) +
          geom_errorbar(width = 0.5,
                        color = "grey") +
          geom_errorbar(width = 0.5,
                        color = "red",
                        data = chosen_year) +
          geom_vline(xintercept = 0) +
          geom_label(aes(label = format(fit, digits = 3)),
                     data = chosen_year, nudge_y=0.4) +
          labs(title = "Mobility Change in Arizona",
               subtitle = "across Arizona, pre-pandemic baseline",
               x = "mobility change percent change from baseline",
               y = "place")
    })

    output$error_plot_2 <- renderPlot({
      data_to_plot <- us_mobility %>%
        group_by(year)

      chosen_year <- us_mobility %>%
        filter(year == input$year_chosen)

      data_to_plot %>%
        ggplot(aes(y = place,
                   x = fit,
                   xmax = upper,
                   xmin = lower,
                   group = year)) +
        geom_errorbar(width = 0.5,
                      color = "grey") +
        geom_errorbar(width = 0.5,
                      color = "red",
                      data = chosen_year) +
        geom_vline(xintercept = 0) +
        geom_label(aes(label = format(fit, digits = 3)),
                   data = chosen_year, nudge_y=0.4) +
        labs(title = "Mobility Change in the United States",
             subtitle = "across US, pre-pandemic baseline",
             x = "mobility change percent change from baseline",
             y = "place")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
