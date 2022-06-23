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

# read in data from all three years
mobility_data_2020 <- read_csv("data/2020_US_Region_Mobility_Report.csv")
mobility_data_2021 <- read_csv("data/2021_US_Region_Mobility_Report.csv")
mobility_data_2022 <- read_csv("data/2022_US_Region_Mobility_Report.csv")

tidy_data_2020 <- mobility_data_2020 %>%
  pivot_longer(cols = retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline,
               names_to = "place",
               values_to = "percent_change_from_baseline") %>%
  mutate(place = gsub("_percent_change_from_baseline", "", place))
tidy_data_2021 <- mobility_data_2021 %>%
  pivot_longer(cols = retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline,
               names_to = "place",
               values_to = "percent_change_from_baseline") %>%
  mutate(place = gsub("_percent_change_from_baseline", "", place))
tidy_data_2022 <- mobility_data_2022 %>%
  pivot_longer(cols = retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline,
               names_to = "place",
               values_to = "percent_change_from_baseline") %>%
  mutate(place = gsub("_percent_change_from_baseline", "", place))


az_data_2020 <- tidy_data_2020 %>%
  filter(sub_region_1 == "Arizona")
az_data_2021 <- tidy_data_2021 %>%
  filter(sub_region_1 == "Arizona")
az_data_2022 <- tidy_data_2022 %>%
  filter(sub_region_1 == "Arizona")


model_2020 <- lm(percent_change_from_baseline ~ sub_region_1 + place, data = tidy_data_2020)
model_2021 <- lm(percent_change_from_baseline ~ sub_region_1 + place, data = tidy_data_2021)
model_2022 <- lm(percent_change_from_baseline ~ sub_region_1 + place, data = tidy_data_2022)
az_model_2020 <- lm(percent_change_from_baseline ~ place, data = az_data_2020)
az_model_2021 <- lm(percent_change_from_baseline ~ place, data = az_data_2021)
az_model_2022 <- lm(percent_change_from_baseline ~ place, data = az_data_2022)


us_mobility_2020 <- effect("place", model_2020) %>%
  data.frame()
us_mobility_2021 <- effect("place", model_2021) %>%
  data.frame()
us_mobility_2022 <- effect("place", model_2022) %>%
  data.frame()
az_mobility_2020 <- effect("place", az_model_2020) %>%
  data.frame()
az_mobility_2021 <- effect("place", az_model_2021) %>%
  data.frame()
az_mobility_2022 <- effect("place", az_model_2022) %>%
  data.frame()


us_mobility_2020$year <- 2020
us_mobility_2021$year <- 2021
us_mobility_2022$year <- 2022
az_mobility_2020$year <- 2020
az_mobility_2021$year <- 2021
az_mobility_2022$year <- 2022


#us_mobility <- merge(us_mobility_2020, us_mobility_2021, all=TRUE)
#us_mobility <- merge(us_mobility, us_mobility_2022, all=TRUE)
#az_mobility <- merge(az_mobility_2020, az_mobility_2021, all=TRUE)
#az_mobility <- merge(az_mobility, az_mobility_2022, all=TRUE)

us_mobility <- bind_rows(us_mobility_2020, us_mobility_2021, us_mobility_2022)
az_mobility <- bind_rows(az_mobility_2020, az_mobility_2021, az_mobility_2022)

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
          ggplot(aes(y = reorder(place, fit),
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
                     data = year_chosen) +
          labs(title = "Mobility Change in Arizona",
               subtitle = "across Arizona, pre-pandemic baseline",
               x = "mobility change percent change from baseline",
               y = "place")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
