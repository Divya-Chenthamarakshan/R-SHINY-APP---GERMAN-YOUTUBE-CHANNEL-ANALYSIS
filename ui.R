
#R SHINY APP FOR -Top 1000 most subscribed YouTube channels in Germany as of 13th Feb 2025

#install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "plotly", "DT", "scales"))

# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(scales)


# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "German YouTube Channel Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Growth Rate Analysis", tabName = "growth", icon = icon("chart-line")),
      menuItem("Content Efficiency", tabName = "efficiency", icon = icon("video")),
      menuItem("Category Distribution", tabName = "categories", icon = icon("tags")),
      menuItem("Publication Strategy", tabName = "publishing", icon = icon("calendar")),
      menuItem("Audience Loyalty", tabName = "loyalty", icon = icon("users")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    )
  ),

   
  dashboardBody(
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "About This Dashboard", status = "primary", solidHeader = TRUE, width = 12,
                  "This dashboard analyzes the top 1000 German YouTube channels as of February 13, 2025. Explore different tabs to find insights about growth rates, content efficiency, category distribution, and more."
                )
              ),
              fluidRow(
                valueBoxOutput("total_subs_box", width = 6),
                valueBoxOutput("total_views_box", width = 6),
                valueBoxOutput("total_videos_box", width = 6),
                valueBoxOutput("avg_subs_box", width = 6 )
              ),
              fluidRow(
                box(
                  title = "Top Categories", status = "info", solidHeader = TRUE,
                  plotlyOutput("top_categories_plot", height = 300)
                ),
                
                box(
                  title = "Subscribers Distribution", status = "info", solidHeader = TRUE,
                  plotlyOutput("subs_histogram", height = 300)
                )
              )
      ),
      
      # 1. Growth Rate Analysis tab
      tabItem(tabName = "growth",
              fluidRow(
                box(
                  title = "Filters", width = 3, status = "primary",
                  selectInput("growth_category", "Select Category:",
                              choices = c("All", "Music", "Entertainment", "Gaming", "Education", "Comedy"),
                              selected = "All"),
                  sliderInput("growth_year_range", "Year Range:",
                              min = 2005, max = 2024, value = c(2005, 2024))
                ),
                box(
                  title = "Channel Age vs Subscribers", status = "info", solidHeader = TRUE, width = 9,
                  plotlyOutput("growth_plot", height = 400)
                )
              ),
              fluidRow(
                box(
                  title = "Growth Rate by Starting Year", status = "warning", solidHeader = TRUE, width = 12,
                  plotlyOutput("yearly_growth_plot", height = 300)
                )
              )
      ),
      
      # 2. Content Efficiency tab
      tabItem(tabName = "efficiency",
              fluidRow(
                box(
                  title = "Filters", width = 3, status = "primary",
                  selectInput("efficiency_category", "Select Category:",
                              choices = c("All", "Music", "Entertainment", "Gaming", "Education", "Comedy"),
                              selected = "All"),
                  sliderInput("efficiency_views", "Views Range (in millions):",
                              min = 0, max = 100000, value = c(0, 100000), step = 1000)
                ),
                box(
                  title = "Views per Video by Category", status = "info", solidHeader = TRUE, width = 9,
                  plotlyOutput("efficiency_plot", height = 350)
                )
              ),
              fluidRow(
                box(
                  title = "Top 20 Most Efficient Channels", status = "success", solidHeader = TRUE, width = 12,
                  plotlyOutput("top_efficiency_table", height = 300)
                )
              )
      ),
      
      # Category Distribution tab
      tabItem(tabName = "categories",
              fluidRow(
                box(
                  title = "Category Market Share (Subscribers)", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("category_pie", height = 350)
                ),
                box(
                  title = "Category Market Share (Views)", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("category_views_pie", height = 350)
                )
              ),
              fluidRow(
                box(
                  title = "Category Growth Over Time", status = "warning", solidHeader = TRUE, width = 12,
                  plotlyOutput("category_growth_plot", height = 400)
                )
              )
      ),
      
      # Publication Strategy tab
      tabItem(tabName = "publishing",
              fluidRow(
                box(
                  title = "Filters", width = 3, status = "primary",
                  selectInput("publishing_category", "Select Category:",
                              choices = c("All", "Music", "Entertainment", "Gaming", "Education", "Comedy"),
                              selected = "All"),
                  sliderInput("publishing_age", "Channel Age Range:",
                              min = 1, max = 20, value = c(1, 20))
                ),
                box(
                  title = "Publishing Frequency vs Subscribers", status = "info", solidHeader = TRUE, width = 9,
                  plotlyOutput("publishing_plot", height = 400)
                )
              ),
              fluidRow(
                box(
                  title = "Optimal Publishing Frequency by Category", status = "success", solidHeader = TRUE, width = 12,
                  plotlyOutput("optimal_publishing_plot", height = 400)
                )
              )
      ),
      
      # Audience Loyalty tab
      tabItem(tabName = "loyalty",
              fluidRow(
                box(
                  title = "Filters", width = 3, status = "primary",
                  selectInput("loyalty_category", "Select Category:",
                              choices = c("All", "Music", "Entertainment", "Gaming", "Education", "Comedy"),
                              selected = "All"),
                  sliderInput("loyalty_subs", "Subscriber Range (in millions):",
                              min = 0, max = 100, value = c(0, 100), step = 1)
                ),
                box(
                  title = "Views per Subscriber by Category", status = "info", solidHeader = TRUE, width = 9,
                  plotlyOutput("loyalty_plot", height = 350)
                )
              ),
              fluidRow(
                box(
                  title = "Top 20 Channels by Audience Loyalty", status = "success", solidHeader = TRUE, width = 12,
                  plotlyOutput("top_loyalty_table", height = 500)
                )
              )
      ),
      
      # Data Table tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Raw Data", width = 12, status = "primary",
                  DTOutput("data_table")
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Load the dataset
  # Note: Replace with your actual data loading mechanism
  df <- reactive({
    # For demonstration purposes, creating a sample dataset
    # In a real application, you would use read.csv() or similar
    set.seed(123)
    
    # Define categories
    categories <- c("Music", "Entertainment", "Gaming", "Education", "Comedy", 
                    "How-to & Style", "Science & Technology", "Sports", "News & Politics", "Film & Animation")
    
    # Create random data for 1000 channels
    data.frame(
      Rank = 1:1000,
      Youtuber = paste0("Channel_", 1:1000),
      Subscribers = round(rlnorm(1000, meanlog = 15, sdlog = 1.5)),
      Video_Views = round(rlnorm(1000, meanlog = 20, sdlog = 2)),
      Video_Count = round(runif(1000, min = 10, max = 2000)),
      Category = sample(categories, 1000, replace = TRUE),
      Started = sample(2005:2024, 1000, replace = TRUE)
    )
  })
  
  # Calculate derived metrics
  derived_metrics <- reactive({
    data <- df()
    data %>%
      mutate(
        Channel_Age = 2025 - Started,
        Views_Per_Video = Video_Views / Video_Count,
        Videos_Per_Year = Video_Count / Channel_Age,
        Views_Per_Subscriber = Video_Views / Subscribers,
        Growth_Rate = Subscribers / Channel_Age
      )
  })
  
  # Summary metrics for overview
  output$total_subs_box <- renderValueBox({
    total_subs <- sum(df()$Subscribers)
    valueBox(
      value = format(total_subs, big.mark = ",", scientific = FALSE),
      subtitle = "Total Subscribers",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$total_views_box <- renderValueBox({
    total_views <- sum(df()$Video_Views)
    valueBox(
      value = format(total_views, big.mark = ",", scientific = FALSE),
      subtitle = "Total Views",
      icon = icon("eye"),
      color = "green"
    )
  })
  
  output$total_videos_box <- renderValueBox({
    total_videos <- sum(df()$Video_Count)
    valueBox(
      value = format(total_videos, big.mark = ",", scientific = FALSE),
      subtitle = "Total Videos",
      icon = icon("video"),
      color = "purple"
    )
  })
  
  output$avg_subs_box <- renderValueBox({
    avg_subs <- mean(df()$Subscribers)
    valueBox(
      value = format(round(avg_subs), big.mark = ",", scientific = FALSE),
      subtitle = "Average Subscribers",
      icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  # Overview plots

  output$top_categories_plot <- renderPlotly({
    category_counts <- df() %>%
      group_by(Category) %>%
      summarize(Count = n()) %>%
      arrange(desc(Count)) %>%
      head(10)
    
    # Assign blue color to the highest count category and grey to the rest
    colors <- ifelse(category_counts$Count == max(category_counts$Count), "#3366CC", "grey")
    
    p <- ggplot(category_counts, aes(x = reorder(Category, Count), y = Count)) +  # Remove fill = Category
      geom_bar(stat = "identity", fill = colors) +  # Directly use the colors vector
      coord_flip() +
      labs(x = "", y = "Number of Channels") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  
  output$subs_histogram <- renderPlotly({
    p <- ggplot(df(), aes(x = Subscribers / 1e6)) + # Divide by 1 million
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      scale_x_log10(labels = scales::comma) +
      labs(x = "Subscribers (Millions, log scale)", y = "Number of Channels") + # Update x-axis label
      theme_minimal()
    ggplotly(p)
  })
  
  # Growth Rate Analysis
  filtered_growth_data <- reactive({
    data <- derived_metrics()
    
    if (input$growth_category != "All") {
      data <- data %>% filter(Category == input$growth_category)
    }
    
    data %>% 
      filter(Started >= input$growth_year_range[1] & Started <= input$growth_year_range[2])
  })
  
  output$growth_plot <- renderPlotly({
    p <- ggplot(filtered_growth_data(), aes(x = Channel_Age, y = Subscribers / 1e6, color = Category)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      scale_y_log10(labels = scales::comma) +
      labs(x = "Channel Age (Years)", y = "Subscribers (Millions)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$yearly_growth_plot <- renderPlotly({
    yearly_growth <- filtered_growth_data() %>%
      group_by(Started) %>%
      summarize(Avg_Growth_Rate = mean(Growth_Rate))
    
    p <- ggplot(yearly_growth, aes(x = Started, y = Avg_Growth_Rate)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(x = "Starting Year", y = "Average-Growth-Rate (Subscribers/Year)") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Content Efficiency
  filtered_efficiency_data <- reactive({
    data <- derived_metrics()
    
    if (input$efficiency_category != "All") {
      data <- data %>% filter(Category == input$efficiency_category)
    }
    
    data %>% 
      filter(Video_Views >= input$efficiency_views[1] * 1e6 & 
               Video_Views <= input$efficiency_views[2] * 1e6)
  })
  
  output$efficiency_plot <- renderPlotly({
    p <- ggplot(filtered_efficiency_data(), aes(x = Category, y = Views_Per_Video / 1e6, fill = Category)) +
      geom_boxplot() +
      scale_y_log10(labels = scales::comma) +
      labs(x = "", y = "Views Per Video (MILLION)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
    
    ggplotly(p)
  })
  
  output$top_efficiency_table <- renderPlotly({
    top_efficiency <- derived_metrics() %>%
      arrange(desc(Views_Per_Video)) %>%
      head(20) %>%
      select(Rank, Youtuber, Category, Views_Per_Video, Video_Count, Subscribers)
    
    p <- plot_ly(
      type = 'table',
      header = list(
        values = c("Rank", "Channel", "Category", "Views/Video", "Videos", "Subscribers"),
        align = c('left', 'left', 'left', 'right', 'right', 'right'),
        line = list(width = 1, color = 'black'),
        fill = list(color = c('#3366CC')),
        font = list(family = "Arial", size = 14, color = "white")
      ),
      cells = list(
        values = list(
          top_efficiency$Rank,
          top_efficiency$Youtuber,
          top_efficiency$Category,
          format(round(top_efficiency$Views_Per_Video), big.mark = ","),
          format(top_efficiency$Video_Count, big.mark = ","),
          format(top_efficiency$Subscribers, big.mark = ",")
        ),
        align = c('left', 'left', 'left', 'right', 'right', 'right'),
        line = list(color = "black", width = 1),
        font = list(family = "Arial", size = 12, color = c('#333333'))
      )
    )
    
    p
  })
  
  # Category Distribution
  
  output$category_pie <- renderPlotly({
    category_subs <- df() %>%
      group_by(Category) %>%
      summarize(Total_Subscribers = sum(Subscribers) / 1e9) %>%
      arrange(desc(Total_Subscribers))
    
    colors <- ifelse(category_subs$Total_Subscribers == max(category_subs$Total_Subscribers), "red", "grey") # Assign colors
    
    plot_ly(category_subs, y = ~Category, x = ~Total_Subscribers, type = 'bar', orientation = 'h',
            hoverinfo = 'text',
            text = ~paste(format(Total_Subscribers, digits = 2), "B subscribers"),
            marker = list(color = colors)) %>% # Apply colors
      layout(xaxis = list(title = "Total Subscribers (Billions)"),
             yaxis = list(title = "Category", categoryorder = "total ascending"))
  })
  
  output$category_views_pie <- renderPlotly({
    category_views <- df() %>%
      group_by(Category) %>%
      summarize(Total_Views = sum(Video_Views) / 1e9) %>%
      arrange(desc(Total_Views))
    
    colors <- ifelse(category_views$Total_Views == max(category_views$Total_Views), "red", "grey") # Assign colors
    
    plot_ly(category_views, y = ~Category, x = ~Total_Views, type = 'bar', orientation = 'h',
            hoverinfo = 'text',
            text = ~paste(format(Total_Views, digits = 2), "B views"),
            marker = list(color = colors)) %>% # Apply colors
      layout(xaxis = list(title = "Total Views (Billions)"),
             yaxis = list(title = "Category", categoryorder = "total ascending"))
  })
  
  
  
  output$category_growth_plot <- renderPlotly({
    category_year_growth <- derived_metrics() %>%
      group_by(Category, Started) %>%
      summarize(Avg_Growth = mean(Growth_Rate)) %>%
      filter(n() >= 5) # Only include years with at least 5 channels
    
    p <- ggplot(category_year_growth, aes(x = Started, y = Avg_Growth, color = Category, group = Category)) +
      geom_line() +
      geom_point() +
      labs(x = "Starting Year", y = "Average Growth Rate") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Publication Strategy
  filtered_publishing_data <- reactive({
    data <- derived_metrics()
    
    if (input$publishing_category != "All") {
      data <- data %>% filter(Category == input$publishing_category)
    }
    
    data %>% 
      filter(Channel_Age >= input$publishing_age[1] & Channel_Age <= input$publishing_age[2])
  })
  
  output$publishing_plot <- renderPlotly({
    p <- ggplot(filtered_publishing_data(), aes(x = Videos_Per_Year, y = Subscribers / 1e6, color = Category)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "loess", se = FALSE, color = "black") +
      scale_x_log10(labels = scales::comma) +
      scale_y_log10(labels = scales::comma) +
      labs(x = "Videos Published Per Year (log scale)", y = "Subscribers (MILLION)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$optimal_publishing_plot <- renderPlotly({
    # First, convert Video_Bin to a numeric representation
    # Extract the midpoint of each bin for better visualization
    optimal_publishing <- derived_metrics() %>%
      mutate(Video_Rate = Videos_Per_Year) %>%  # Use actual values instead of bins
      group_by(Category, Video_Rate = round(Video_Rate/25)*25) %>%  # Round to nearest 25
      summarize(
        Avg_Subscribers = mean(Subscribers),
        Count = n()
      ) %>%
      filter(Count >= 3) # Only include points with at least 3 channels
    
    p <- ggplot(optimal_publishing, aes(x = Video_Rate, y = Avg_Subscribers, color = Category)) +
      geom_line(size = 1) +
      geom_point(size = 0) +
      labs(x = "Videos Published Per Year", y = "Average Subscribers (Millions)") +
      scale_y_continuous(labels = function(x) paste0(round(x/1000000, 1), "M")) +
      scale_x_continuous(breaks = seq(0, 500, by = 50)) +
      theme_minimal() +
      theme(legend.position = "right",
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Audience Loyalty
  filtered_loyalty_data <- reactive({
    data <- derived_metrics()
    
    if (input$loyalty_category != "All") {
      data <- data %>% filter(Category == input$loyalty_category)
    }
    
    data %>% 
      filter(Subscribers >= input$loyalty_subs[1] * 1e6 & 
               Subscribers <= input$loyalty_subs[2] * 1e6)
  })
  
  output$loyalty_plot <- renderPlotly({
    p <- ggplot(filtered_loyalty_data(), aes(x = Category, y = Views_Per_Subscriber, fill = Category)) +
      geom_boxplot() +
      labs(x = "", y = "Views Per Subscriber") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
    
    ggplotly(p)
  })
  
  output$top_loyalty_table <- renderPlotly({
    top_loyalty <- derived_metrics() %>%
      arrange(desc(Views_Per_Subscriber)) %>%
      head(20) %>%
      select(Rank, Youtuber, Category, Views_Per_Subscriber, Subscribers, Video_Count)
    
    p <- plot_ly(
      type = 'table',
      header = list(
        values = c("Rank", "Channel", "Category", "Views/Subscriber", "Subscribers", "Videos"),
        align = c('left', 'left', 'left', 'right', 'right', 'right'),
        line = list(width = 1, color = 'black'),
        fill = list(color = c('#3366CC')),
        font = list(family = "Arial", size = 14, color = "white")
      ),
      cells = list(
        values = list(
          top_loyalty$Rank,
          top_loyalty$Youtuber,
          top_loyalty$Category,
          format(round(top_loyalty$Views_Per_Subscriber, 1), big.mark = ","),
          format(top_loyalty$Subscribers, big.mark = ","),
          format(top_loyalty$Video_Count, big.mark = ",")
        ),
        align = c('left', 'left', 'left', 'right', 'right', 'right'),
        line = list(color = "black", width = 1),
        font = list(family = "Arial", size = 12, color = c('#333333'))
      )
    )
    
    p
  })
  
  # Data Table
  output$data_table <- renderDT({
    datatable(df(), 
              options = list(
                pageLength = 10,
                autoWidth = TRUE,
                scrollX = TRUE
              )
    )
  })
}

# Run the app
#shinyApp(ui, server)
shinyApp(ui, server, options = list(height = 1200, width = "100%"))

