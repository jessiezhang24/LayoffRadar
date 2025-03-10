options(shiny.port = 8050, shiny.autoreload = TRUE)

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(bslib)
library(here)
library(rworldmap) 
library(RColorBrewer) 

# Load dataset
df <- read.csv(here("data", "processed", "processed_tech_layoffs.csv"), stringsAsFactors = FALSE)

# Convert column names to lowercase
colnames(df) <- tolower(colnames(df))

# Rename column for clarity
df <- df %>%
  rename(layoff_count = laid_off)

# Load world map data
world_map <- getMap(resolution = "low")

# Create country name mapping (align dataset with rworldmap names)
country_mapping <- c(
  "USA" = "United States of America",
  "UK" = "United Kingdom")

# Replace incorrect country names in df
df$country <- ifelse(df$country %in% names(country_mapping), country_mapping[df$country], df$country)

# UI Layout
ui <- page_fluid(
    titlePanel("LayoffRadar"),
    
    layout_sidebar(
        sidebar = sidebar(
            selectInput("company", "Select Company:", choices = unique(df$company), selected = NULL, multiple = TRUE),
            selectInput("country", "Select Country:", choices = unique(df$country), selected = NULL, multiple = TRUE),
            selectInput("industry", "Select Industry:", choices = unique(df$industry), selected = NULL, multiple = TRUE)
        ),
        
        mainPanel(
            layout_column_wrap(
                width = 1,
                card(full_screen = TRUE, card_body(leafletOutput("layoff_map"), title = "Layoff Count by Country")),
                layout_columns(
                    card(full_screen = TRUE, card_body(plotOutput("layoff_stacked_bar"), title = "Yearly Layoff Trend")),
                    card(full_screen = TRUE, card_body(plotOutput("layoff_bar"), title = "Layoff Count by Industry"))
                )
            )
        )
    )
)

# Server Logic
server <- function(input, output, session) {
    
    filtered_data <- reactive({
        data <- df
        if (!is.null(input$company) && length(input$company) > 0) {
            data <- data %>% filter(company %in% input$company)
        }
        if (!is.null(input$country) && length(input$country) > 0) {
            data <- data %>% filter(country %in% input$country)
        }
        if (!is.null(input$industry) && length(input$industry) > 0) {
            data <- data %>% filter(industry %in% input$industry)
        }
        return(data)
    })

    # World Map Visualization (Country-Level)
    output$layoff_map <- renderLeaflet({
        map_data <- filtered_data() %>%
            group_by(country) %>%
            summarise(total_layoffs = sum(layoff_count, na.rm = TRUE))

        # Ensure country names match between map data and dataset
        bins <- c(0, 100, 500, 1000, 5000, 10000, Inf)
        pal <- colorBin("Reds", domain = map_data$total_layoffs, bins = bins)

        leaflet(world_map) %>%
            addTiles() %>%
            addPolygons(
                fillColor = ~pal(map_data$total_layoffs[match(world_map$ADMIN, map_data$country)]),
                weight = 1,
                color = "white",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE),
                popup = ~paste0("<b>", ADMIN, "</b><br>Layoffs: ", 
                                ifelse(!is.na(map_data$total_layoffs[match(ADMIN, map_data$country)]), 
                                       map_data$total_layoffs[match(ADMIN, map_data$country)], 0))
            ) %>%
            addLegend(pal = pal, values = map_data$total_layoffs, title = "Total Layoffs", position = "bottomright")
    })

    # Stacked Bar Chart for Yearly Layoffs (Top 10 Companies + Others)
    output$layoff_stacked_bar <- renderPlot({
        trend_data <- filtered_data() %>%
            group_by(year, company) %>%
            summarise(total_layoffs = sum(layoff_count, na.rm = TRUE), .groups = 'drop')
        
        # Identify the top 10 companies
        top_companies <- trend_data %>%
            group_by(company) %>%
            summarise(total = sum(total_layoffs)) %>%
            arrange(desc(total)) %>%
            slice(1:10) %>%
            pull(company)
        
        # Replace non-top companies with "Others"
        trend_data <- trend_data %>%
            mutate(company = ifelse(company %in% top_companies, company, "Others")) %>%
            group_by(year, company) %>%
            summarise(total_layoffs = sum(total_layoffs), .groups = 'drop')

        ggplot(trend_data, aes(x = factor(year), y = total_layoffs, fill = company)) +
            geom_bar(stat = "identity", position = "stack") +
            scale_fill_brewer(palette = "Set2") +
            labs(title = "Yearly Layoff Trend by Company", x = "Year", y = "Total Layoffs", fill = "Company") +
            theme_minimal()
    })

    # Bar Chart - Layoffs by Industry
    output$layoff_bar <- renderPlot({
        bar_data <- filtered_data() %>%
            group_by(industry) %>%
            summarise(total_layoffs = sum(layoff_count, na.rm = TRUE)) %>%
            arrange(desc(total_layoffs))

        ggplot(bar_data, aes(x = reorder(industry, -total_layoffs), y = total_layoffs)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            labs(title = "Layoff Count by Industry", x = "Industry", y = "Total Layoffs") +
            theme_minimal() +
            coord_flip()
    })
}

# Run the Shiny App
shinyApp(ui, server)