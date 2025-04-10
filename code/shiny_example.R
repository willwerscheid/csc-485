library(tidyverse)
library(shiny)

emissions <- read_csv("https://github.com/open-numbers/ddf--gapminder--systema_globalis/raw/refs/heads/master/countries-etc-datapoints/ddf--datapoints--co2_emissions_tonnes_per_person--by--geo--time.csv")
countries <- read_csv("https://github.com/open-numbers/ddf--gapminder--systema_globalis/raw/refs/heads/master/ddf--entities--geo--country.csv")

emissions <- emissions |>
  left_join(countries |> select(country, name), by = c("geo" = "country")) |>
  rename(country_code = geo, country = name, year = time) |>
  filter(year >= 1950)

map <- map_data("world")
map <- map |>
  #mutate(region = str_replace_all(region, "USA", "United States")) |>
  #mutate(region = str_replace_all(region, "UK", "United Kingdom")) |>
  mutate(region = str_replace_all(region, "Ivory Coast", "Cote d'Ivoire")) |>
  mutate(region = str_replace_all(region, "Republic of Congo", "Congo, Rep.")) |>
  mutate(region = str_replace_all(region, "Democratic Republic of the Congo", "Congo, Dem. Rep.")) |>
  mutate(region = str_replace_all(region, "Kyrgyzstan", "Kyrgyz Republic")) |>
  mutate(region = str_replace_all(region, "Swaziland", "Eswatini")) |>
  mutate(region = str_replace_all(region, "United Arab Emirates", "UAE")) |>
  mutate(region = str_replace_all(region, "Laos", "Lao")) |>
  mutate(region = str_replace_all(region, "Slovakia", "Slovak Republic"))

ui <- fluidPage(
  
  # App title ----
  titlePanel("CO2 emissions per person per year (tonnes)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the year ----
      sliderInput(inputId = "year",
                  label = "Year:",
                  min = min(emissions$year),
                  max = max(emissions$year),
                  value = mean(range(emissions$year)),
                  sep = "")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Map ----
      plotOutput(outputId = "mapPlot")
      
    )
  )
)

# Define server logic required to draw a map ----
server <- function(input, output) {

  output$mapPlot <- renderPlot({
    emissions_1yr <- emissions |>
      filter(year == input$year)
    
    ggplot(emissions_1yr, aes(map_id = country)) +
      geom_map(aes(fill = co2_emissions_tonnes_per_person), map = map) +
      expand_limits(x = map$long, y = map$lat) +
      scale_fill_gradient(low = "white", high = "red", 
                          limits = range(emissions$co2_emissions_tonnes_per_person),
                          transform = "log1p",
                          breaks = c(1, 10, 100)) +
      theme_minimal()
   })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
