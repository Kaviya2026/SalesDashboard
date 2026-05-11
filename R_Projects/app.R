install.packages(c(
  "shiny",
  "dplyr",
  "ggplot2",
  "plotly",
  "readr",
  "lubridate",
  "lifecycle",
  "DT"
))

install.packages("httpuv")
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(lubridate)
library(DT)

# Load Dataset
df <- read.csv("Superstore.csv", stringsAsFactors = FALSE)
head(df)

# Clean Column Names
colnames(df) <- trimws(colnames(df))

# Convert Date
df$`Order Date` <- as.Date(df$`Order Date`, format = "%d-%m-%Y")

# Extract Month-Year
df$Month_Year <- format(df$`Order Date`, "%Y-%m")

# UI
ui <- fluidPage(
  
  titlePanel("Sales Analytics Dashboard"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "region",
        "Select Region:",
        choices = c("All", unique(df$Region)),
        selected = "All"
      ),
      
      selectInput(
        "category",
        "Select Category:",
        choices = c("All", unique(df$Category)),
        selected = "All"
      )
      
    ),
    
    mainPanel(
      
      fluidRow(
        
        column(
          4,
          wellPanel(
            h3("Total Sales"),
            textOutput("total_sales")
          )
        ),
        
        column(
          4,
          wellPanel(
            h3("Total Profit"),
            textOutput("total_profit")
          )
        ),
        
        column(
          4,
          wellPanel(
            h3("Total Orders"),
            textOutput("total_orders")
          )
        )
      ),
      
      br(),
      
      fluidRow(
        
        column(
          6,
          plotlyOutput("monthly_sales")
        ),
        
        column(
          6,
          plotlyOutput("region_sales")
        )
      ),
      
      br(),
      
      fluidRow(
        
        column(
          6,
          plotlyOutput("category_perf")
        ),
        
        column(
          6,
          plotlyOutput("city_sales")
        )
      ),
      
      br(),
      
      fluidRow(
        
        column(
          12,
          plotlyOutput("top_products")
        )
      ),
      
      br(),
      
      fluidRow(
        
        column(
          12,
          dataTableOutput("sales_table")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    
    data <- df
    
    if(input$region != "All") {
      data <- data %>%
        filter(Region == input$region)
    }
    
    if(input$category != "All") {
      data <- data %>%
        filter(Category == input$category)
    }
    
    data
  })
  
  # KPI Cards
  output$total_sales <- renderText({
    
    paste0("$", round(sum(filtered_data()$Sales), 2))
  })
  
  output$total_profit <- renderText({
    
    paste0("$", round(sum(filtered_data()$Profit), 2))
  })
  
  output$total_orders <- renderText({
    
    nrow(filtered_data())
  })
  
  # Monthly Sales Trend
  output$monthly_sales <- renderPlotly({
    
    monthly <- filtered_data() %>%
      group_by(Month_Year) %>%
      summarise(Sales = sum(Sales))
    
    p <- ggplot(monthly,
                aes(x = Month_Year,
                    y = Sales,
                    group = 1)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Monthly Sales Trend",
        x = "Month",
        y = "Sales"
      ) +
      theme_minimal() +
      theme(axis.text.x =
              element_text(angle = 45,
                           hjust = 1))
    
    ggplotly(p)
  })
  
  # Region Sales
  output$region_sales <- renderPlotly({
    
    reg <- filtered_data() %>%
      group_by(Region) %>%
      summarise(Sales = sum(Sales))
    
    p <- ggplot(reg,
                aes(x = Region,
                    y = Sales,
                    fill = Region)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Region-wise Sales"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Category Performance
  output$category_perf <- renderPlotly({
    
    cat <- filtered_data() %>%
      group_by(Category) %>%
      summarise(Sales = sum(Sales))
    
    p <- ggplot(cat,
                aes(x = Category,
                    y = Sales,
                    fill = Category)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Category Performance"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # City Sales
  output$city_sales <- renderPlotly({
    
    city <- filtered_data() %>%
      group_by(City) %>%
      summarise(Sales = sum(Sales)) %>%
      arrange(desc(Sales)) %>%
      head(10)
    
    p <- ggplot(city,
                aes(x = reorder(City, Sales),
                    y = Sales,
                    fill = City)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = "Top 10 Cities by Sales",
        x = "City",
        y = "Sales"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Top Products
  output$top_products <- renderPlotly({
    
    prod <- filtered_data() %>%
      group_by(`Product Name`) %>%
      summarise(Sales = sum(Sales)) %>%
      arrange(desc(Sales)) %>%
      head(10)
    
    p <- ggplot(prod,
                aes(x = reorder(`Product Name`, Sales),
                    y = Sales,
                    fill = Sales)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = "Top Performing Products",
        x = "Product",
        y = "Sales"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Data Table
  output$sales_table <- renderDataTable({
    
    filtered_data() %>%
      select(
        `Order ID`,
        City,
        Region,
        Category,
        `Product Name`,
        Sales,
        Profit
      )
  })
}

# Run App
shinyApp(ui = ui, server = server)