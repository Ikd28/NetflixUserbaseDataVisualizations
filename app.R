library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(colourpicker)

# Load and prepare the data
netflix_df <- read.csv("Netflix Userbase.csv")

# Cleaning the data
numeric_cols <- sapply(netflix_df, is.numeric)
netflix_df_clean <- netflix_df[!apply(netflix_df[numeric_cols], 1, function(row) any(is.na(row) | is.infinite(row))), ]

# UI for the application
ui <- fluidPage(
  titlePanel("Netflix User Base Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("visualization", "Choose a Visualization:", 
                  choices = c("Distribution of Subscription Types", 
                              "Scatter Plot of Age vs. Monthly Revenue",
                              "Distribution of Age Among Users",
                              "Distribution of Gender Among Users",
                              "Heatmap of Age vs. Subscription Type",
                              "Heatmap of Age vs. Device Type",
                              "Heatmap of User Join Dates",
                              "Trend of Monthly Revenue Over Time"),
                  selected = "Distribution of Subscription Types"),
      
      selectInput("country", "Choose a Country:", 
                  choices = unique(netflix_df$Country), selected = "USA"),
      
      sliderInput("ageRange", "Select Age Range:", 
                  min = min(netflix_df_clean$Age, na.rm = TRUE), 
                  max = max(netflix_df_clean$Age, na.rm = TRUE), 
                  value = c(18, 35)),
      
      colourInput("lowColor", "Choose Low Color for Heatmaps", "white"),
      colourInput("highColor", "Choose High Color for Heatmaps", "blue"),
      numericInput("plotSize", "Adjust Plot Size:", value = 1, min = 0.5, max = 5)
    ),
    
    mainPanel(
      plotOutput("dynamicPlot")
    )
  )
)

# Server logic
server <- function(input, output) {
  output$dynamicPlot <- renderPlot({
    data_filtered <- subset(netflix_df_clean, Country == input$country & Age >= input$ageRange[1] & Age <= input$ageRange[2])
    
    switch(input$visualization,
           "Distribution of Subscription Types" = {
             return(ggplot(data_filtered, aes(x = Subscription.Type)) +
                      geom_bar() +
                      labs(title = "Distribution of Subscription Types",
                           x = "Subscription Type",
                           y = "Count"))
           },
           "Scatter Plot of Age vs. Monthly Revenue" = {
             return(ggplot(data_filtered, aes(x = Age, y = Monthly.Revenue)) +
                      geom_point(size = input$plotSize) +
                      labs(title = "Age vs. Monthly Revenue",
                           x = "Age",
                           y = "Monthly Revenue"))
           },
           "Distribution of Age Among Users" = {
             return(ggplot(data_filtered, aes(x = Age)) +
                      geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
                      labs(title = "Distribution of Age Among Users",
                           x = "Age",
                           y = "Count"))
           },
           "Distribution of Gender Among Users" = {
             return(ggplot(data_filtered, aes(x = Gender)) +
                      geom_bar(fill = "steelblue") +
                      labs(title = "Distribution of Gender Among Users",
                           x = "Gender",
                           y = "Count"))
           },
           "Heatmap of Age vs. Subscription Type" = {
             agg_data_age_sub <- data_filtered %>%
               group_by(Age, Subscription.Type) %>%
               summarise(count = n(), .groups = 'drop')
             return(ggplot(agg_data_age_sub, aes(x = Age, y = Subscription.Type, fill = count)) +
                      geom_tile() +
                      scale_fill_gradient(low = input$lowColor, high = input$highColor) +
                      labs(title = "Heatmap of Age vs. Subscription Type. Darker shades represent higher counts.",
                           x = "Age",
                           y = "Subscription Type",
                           fill = "Count") +
                      theme_minimal())
           },
           "Heatmap of Age vs. Device Type" = {
             agg_data_device_age <- data_filtered %>%
               group_by(Device, age_group = cut(Age, breaks = c(0, 18, 30, 45, Inf))) %>%
               summarise(count = n(), .groups = 'drop')
             return(ggplot(agg_data_device_age, aes(x = age_group, y = Device, fill = count)) +
                      geom_tile() +
                      scale_fill_gradient(low = input$lowColor, high = input$highColor) +
                      labs(title = "Heatmap of Age vs. Device Type. Darker shades represent higher counts.",
                           x = "Age Group",
                           y = "Device Type",
                           fill = "Count") +
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1)))
           },
           "Heatmap of User Join Dates" = {
             agg_data_join_date <- data_filtered %>%
               group_by(year = year(Join.Date), month = month(Join.Date)) %>%
               summarise(count = n(), .groups = 'drop')
             return(ggplot(agg_data_join_date, aes(x = month, y = year, fill = count)) +
                      geom_tile() +
                      scale_fill_gradient(low = input$lowColor, high = input$highColor) +
                      labs(title = "Heatmap of User Join Dates. Darker shades represent higher counts.",
                           x = "Month",
                           y = "Year",
                           fill = "Count") +
                      theme_minimal() +
                      scale_x_continuous(breaks = 1:12, labels = month.abb) +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1)))
           },
           "Trend of Monthly Revenue Over Time" = {
             agg_data_revenue <- data_filtered %>%
               group_by(Join.Date) %>%
               summarise(total_revenue = sum(Monthly.Revenue), .groups = 'drop')
             return(ggplot(agg_data_revenue, aes(x = Join.Date, y = total_revenue, group = 1)) +
                      geom_line(color = "steelblue", size = input$plotSize) +
                      labs(title = "Trend of Monthly Revenue Over Time",
                           x = "Join Date",
                           y = "Total Monthly Revenue") +
                      theme_minimal())
           }
    )
    
  })
}

# Run the Shiny app
shinyApp(ui, server)

