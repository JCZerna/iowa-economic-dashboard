# 1. Loading libraries ####

library(pacman)
p_load(tidyverse, shiny, leaflet, bslib, bsicons, plotly)


# 2. Loading dataset ####

iowa_insurance_raw <- read_csv("Iowa_Unemployment_Insurance_Benefit_Payments_and_Recipients_by_County__Monthly_.csv")

# 3. Reviewing the dataset ####
str(iowa_insurance_raw)
glimpse(iowa_insurance_raw)

# Checking for missing values
sum(is.na(iowa_insurance_raw))

# 4. Data cleaning ####
iowa_clean <- iowa_insurance_raw %>% 
    rename_all(~tolower(gsub(" ", "_", .))) %>% 
    mutate(date_formatted = mdy_hms(month),
           year_month = floor_date(date_formatted, "month"),
           ) %>% 
    rename(lat = primary_lat_dec, lng = primary_long_dec) %>% 
    select(date_formatted, year_month, county_name,
           benefits_paid, recipients, weeks_compensated, lat, 
           lng)


glimpse(iowa_clean)

# 5. Defining UI
ui <- page_sidebar(
    title = "Iowa Economic Dashboard",
    theme = bs_theme(bootswatch = "superhero"),
    
    sidebar = sidebar(
        title = "Timeline Control",
        sliderInput("date_range", "Select Month:",
                    min = min(iowa_clean$year_month),
                    max = max(iowa_clean$year_month),
                    value = max(iowa_clean$year_month),
                    timeFormat = "%b %Y",
                    animate = TRUE),
        hr(),
        downloadButton("download_data", 
                       "Export Filtered Data", 
                       class = "btn-success")
    ),
    
    # KPI Section
    layout_column_wrap(
        width = 1/2,
        value_box(
            title = "Total Benefits Paid",
            value = textOutput("total_money"),
            showcase = bs_icon("cash-stack"),
            theme = "success"
        ),
        value_box(
            title = "Active Recipients",
            value = textOutput("total_people"),
            showcase = bs_icon("people-fill"),
            theme = "primary"
        )
    ),
    
    # Visualization Section
    layout_columns(
        card(
            card_header("County-Level Distribution"),
            leafletOutput("map")
        ),
        card(
            card_header("Statewide Trend (All Time)"),
            plotlyOutput("trend_plot")
        )
    )
)

# 6. Server Section
server <- function(input, output) {
    
    # Filter data reactively
    filtered_df <- reactive({
        req(input$date_range)
        
        iowa_clean %>%
            filter(
                format(as.Date(year_month), "%Y-%m") == format(as.Date(input$date_range), "%Y-%m")
            )
    })
    
    # KPI 1: Money
    output$total_money <- renderText({
        paste0("$", format(sum(filtered_df()$benefits_paid, na.rm = TRUE), 
                           big.mark = ","))
    })
    
    # KPI 2: People
    output$total_people <- renderText({
        format(sum(filtered_df()$recipients, na.rm = TRUE), big.mark = ",")
    })
    
    # PART 1:
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -93.6, lat = 42.03, zoom = 7) 
    })
    
    # PART 2: 
    observe({
        leafletProxy("map", data = filtered_df()) %>%
            clearMarkers() %>%  
            addCircleMarkers(
                lng = ~lng, lat = ~lat,
                radius = ~sqrt(benefits_paid) / 100,
                popup = ~county_name,
                color = "orange", 
                stroke = FALSE, 
                fillOpacity = 0.7
            )
    })
    
    # Plotly Trend Output
    output$trend_plot <- renderPlotly({
        p <- iowa_clean %>%
            group_by(year_month) %>%
            summarise(total = sum(benefits_paid, na.rm = TRUE)) %>%
            ggplot(aes(x = year_month, y = total)) +
            geom_line(color = "orange") +
            scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
            theme_minimal() +
            labs(y = "Total Benefits Paid", x = NULL)
        
        ggplotly(p)
    })
    # Download Handler: Captures the 'filtered_df' and saves it
    output$download_data <- downloadHandler(
        filename = function() {
            paste0("Iowa_Benefits_", input$date_range, ".csv")
        },
        content = function(file) {
            write.csv(filtered_df(), file, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)

