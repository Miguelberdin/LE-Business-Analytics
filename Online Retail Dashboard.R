library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(maps)
library(scales)
library(lubridate)

# Load data
data <- read_excel("Online Retail.xlsx")

# Data preprocessing
data <- data %>%
  mutate(InvoiceDate = as.Date(InvoiceDate, format = "%Y-%m-%d %H:%M:%S")) %>%
  filter(!is.na(CustomerID))

# Preprocess revenue by country data
revenue_country <- data %>%
  group_by(Country) %>%
  summarize(TotalRevenue = sum(Quantity * UnitPrice)) %>%
  arrange(desc(TotalRevenue))

# Load country codes for plotting
country_codes <- map_data("world")

# Join revenue data with country codes
revenue_map_data <- left_join(country_codes, revenue_country, by = c("region" = "Country"))

# Get unique months, days, and years for the dropdowns
unique_years <- sort(unique(year(data$InvoiceDate)))
unique_months <- month.name
unique_days <- 1:31

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = tagList(
    tags$span(class = "logo-mini", "ORD"),
    tags$span(class = "logo-lg", "Online Retail Dashboard")
  ), titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #f4f6f9;
      }
      .main-header .logo {
        font-family: "Arial Black", sans-serif;
        font-size: 18px;
      }
    '))),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                infoBoxOutput("total_sales", width = 4),
                infoBoxOutput("total_customers", width = 4),
                infoBoxOutput("total_invoices", width = 4)
              ),
              fluidRow(
                box(title = "Sales Over Time", status = "success", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(6, 
                             selectInput("start_year", "Select Start Year:", choices = unique_years, selected = min(unique_years)),
                             selectInput("start_month", "Select Start Month:", choices = unique_months, selected = "January"),
                             selectInput("start_day", "Select Start Day:", choices = unique_days, selected = 1)
                      ),
                      column(6,
                             selectInput("end_year", "Select End Year:", choices = unique_years, selected = max(unique_years)),
                             selectInput("end_month", "Select End Month:", choices = unique_months, selected = "December"),
                             selectInput("end_day", "Select End Day:", choices = unique_days, selected = 31)
                      )
                    ),
                    plotlyOutput("sales_over_time")
                )
              ),
              fluidRow(
                box(title = "Top Selling Products", status = "danger", solidHeader = TRUE, width = 6,
                    selectInput("product_number", "Number of Products:", choices = c(5, 10, 15), selected = 10),
                    DTOutput("top_selling_products_table")
                ),
                box(title = "Top Customers", status = "warning", solidHeader = TRUE, width = 6,
                    selectInput("customer_number", "Number of Customers:", choices = c(5, 10, 15), selected = 10),
                    DTOutput("top_customers_table")
                )
              )
      ),
      tabItem(tabName = "data_table",
              fluidRow(
                box(title = "Data Table", status = "info", solidHeader = TRUE, width = 12,
                    DTOutput("data_table")
                )
              )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "Customer Retention Rate", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("customer_retention_plot"),
                    div("This plot shows the retention rates of the top 10 customers over time.", style = "color: #0073b7;")
  )
),
fluidRow(
  box(title = "Monthly Sales Trend", status = "success", solidHeader = TRUE, width = 6,
      plotlyOutput("monthly_sales_trend_plot"),
      div("This plot displays the trend of sales over each month.", style = "color: #00a65a;")
  ),
  box(title = "Average Order Value by Country", status = "danger", solidHeader = TRUE, width = 6,
      plotlyOutput("average_order_value_country_plot"),
      div("This plot shows the average order value per country.", style = "color: #f39c12;")
  )
),
fluidRow(
  box(title = "Revenue by Country", status = "info", solidHeader = TRUE, width = 6,
      plotlyOutput("revenue_by_country_plot"),
      div("This plot illustrates the total revenue generated by each country.", style = "color: #00c0ef;")
  ),
  box(title = "Sales Distribution by Customer", status = "warning", solidHeader = TRUE, width = 6,
      plotlyOutput("sales_distribution_customer_plot"),
      div("This plot highlights the distribution of total sales among the top customers.", style = "color: #f39c12;")
  )
)
),
tabItem(tabName = "about",
        fluidRow(
          box(title = "About This Dashboard", status = "danger", solidHeader = TRUE, width = 12,
              HTML("
        <p style='font-size: 35px; text-align: center; margin: 20px; line-height: 1.6;'><b>Welcome to the Online Retail Dashboard!</b></p>
        <p style='font-size: 16px; text-align: center; margin: 20px; line-height: 1.6;'> <b>Link to the dataset:</b> https://archive.ics.uci.edu/dataset/352/online+retail</p>
        <p style='font-size: 16px; text-align: justify; margin: 20px; line-height: 1.6;'>This interactive dashboard is designed to provide comprehensive insights into the <b>Online Retail dataset</b>, obtained from the UCI Machine Learning Repository. The dataset includes transactions from 01/12/2010 to 09/12/2011 for a UK-based and registered non-store online retail company.</p>
        <p style='font-size: 20px; text-align: justify; margin: 20px; line-height: 1.6;'><b>How is the Online Retail Dashboard structured and what are its key features?</b></p>
        <p style='font-size: 16px; text-align: justify; margin: 20px; line-height: 1.6;'>The dashboard is structured into several tabs, each serving a unique purpose:<br> The <b>Dashboard</b> tab presents key performance indicators (KPIs) such as Total Sales, Total Customers, and Total Invoices, and includes visualizations for sales trends over time, top-selling products, and top customers. These metrics provide an at-a-glance view of the business's performance.<br> The <b>Data Table</b> tab offers a detailed view of the raw data used for analysis, allowing users to explore the data, filter specific records, and gain deeper insights into individual transactions.<br> The <b>Analysis</b> tab includes advanced analysis and visualizations, enabling users to explore customer retention rate, monthly sales trends, revenue distribution by country, average order value by country, and sales distribution by customer. These analyses help identify patterns, trends, and opportunities for business growth. <br> The <b>About</b> tab provides information about the dashboard's purpose, data sources, and usage instructions.</p>
        <p style='font-size: 20px; text-align: justify; margin: 20px; line-height: 1.6;'><b>What types of analysis does the dashboard utilize and what insights do they provide?</b></p>
        <p style='font-size: 16px; text-align: justify; margin: 20px; line-height: 1.6;'>This dashboard utilizes descriptive and diagnostic analysis to provide valuable insights.<br> <b>Descriptive analysis</b> summarizes and describes the features of the data, with visualizations such as sales over time, top-selling products, top customers, revenue by country, average order value by country, and sales distribution by customer.<br> <b>Diagnostic analysis</b> helps understand the causes of specific outcomes by exploring data in depth, such as analyzing customer retention rates over time to understand customer loyalty and examining monthly sales trends to identify patterns and seasonality in sales performance.</p>
        <p style='font-size: 20px; text-align: justify; margin: 20px; line-height: 1.6;'><b>Who is the target audience for the Online Retail Dashboard and how can they benefit from it?</b></p>
        <p style='font-size: 16px; text-align: justify; margin: 20px; line-height: 1.6;'>The target audience for the Online Retail Dashboard includes stakeholders, analysts, and decision-makers in the e-commerce industry. By using this dashboard, they can benefit in several ways: it helps them make data-driven decisions by providing a clear picture of the current state of the business. The comprehensive analysis and interactive visualizations enable users to drill down into specific metrics, identify trends, and uncover insights that can drive business growth. Additionally, it helps optimize marketing strategies and enhance customer experiences by identifying patterns, trends, and opportunities for improvement.</p>
        <p style='font-size: 20px; text-align: justify; margin: 20px; line-height: 1.6;'><b>Who delevop this Online Retail Dashboard?</b></p>
        <p style='font-size: 16px; text-align: justify; margin: 20px; line-height: 1.6;'> This online retail dashboard was developed by the Accountabilitech team, which consists of Zcel M. Aca, Miguel Angelo O. Berdin, and Miguel Angelo O. Berdin.</p>
      ")
          )
        )
)

)
),
skin = "purple"  # Apply a purple skin to the dashboard
)

# Define server logic
server <- function(input, output) {
  output$total_sales <- renderInfoBox({
    total_sales <- sum(data$Quantity * data$UnitPrice, na.rm = TRUE)
    infoBox(
      title = "Total Sales",
      value = paste("£", format(round(total_sales, 2), big.mark = ",")),
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$total_customers <- renderInfoBox({
    total_customers <- n_distinct(data$CustomerID)
    infoBox(
      title = "Total Customers",
      value = total_customers,
      icon = icon("user"),
      color = "yellow"
    )
  })
  
  output$total_invoices <- renderInfoBox({
    total_invoices <- n_distinct(data$InvoiceNo)
    infoBox(
      title = "Total Invoices",
      value = total_invoices,
      icon = icon("file-text"),
      color = "red"
    )
  })
  
  output$sales_over_time <- renderPlotly({
    req(input$start_year, input$start_month, input$start_day, input$end_year, input$end_month, input$end_day)
    
    start_date <- as.Date(paste(input$start_year, match(input$start_month, month.name), input$start_day, sep = "-"))
    end_date <- as.Date(paste(input$end_year, match(input$end_month, month.name), input$end_day, sep = "-"))
    
    filtered_data <- data %>%
      filter(InvoiceDate >= start_date & InvoiceDate <= end_date)
    
    sales_data <- filtered_data %>%
      group_by(InvoiceDate) %>%
      summarize(DailySales = sum(Quantity * UnitPrice, na.rm = TRUE))
    
    p <- ggplot(sales_data, aes(x = InvoiceDate, y = DailySales)) +
      geom_line(color = "darkgreen") +
      labs(x = "Date", y = "Sales (£)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$data_table <- renderDT({
    datatable(data)
  })
  
  output$top_selling_products_table <- renderDT({
    req(input$product_number)
    top_products <- data %>%
      group_by(Description) %>%
      summarize(QuantitySold = sum(Quantity)) %>%
      arrange(desc(QuantitySold)) %>%
      head(as.numeric(input$product_number))
    
    datatable(top_products)
  })
  
  output$top_customers_table <- renderDT({
    req(input$customer_number)
    top_customers <- data %>%
      group_by(CustomerID) %>%
      summarize(TotalSales = sum(Quantity * UnitPrice)) %>%
      arrange(desc(TotalSales)) %>%
      head(as.numeric(input$customer_number))
    
    datatable(top_customers)
  })
  
  output$monthly_sales_trend_plot <- renderPlotly({
    monthly_sales <- data %>%
      mutate(Month = floor_date(InvoiceDate, "month")) %>%
      group_by(Month) %>%
      summarize(MonthlySales = sum(Quantity * UnitPrice, na.rm = TRUE))
    
    p <- ggplot(monthly_sales, aes(x = Month, y = MonthlySales)) +
      geom_line(color = "darkgreen") +
      labs(x = "Month", y = "Sales (£)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$average_order_value_country_plot <- renderPlotly({
    avg_order_value <- data %>%
      group_by(Country) %>%
      summarize(AverageOrderValue = mean(Quantity * UnitPrice, na.rm = TRUE)) %>%
      arrange(desc(AverageOrderValue))
    
    p <- ggplot(avg_order_value, aes(x = reorder(Country, -AverageOrderValue), y = AverageOrderValue, fill = Country)) +
      geom_bar(stat = "identity") +
      labs(x = "Country", y = "Average Order Value (£)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$revenue_by_country_plot <- renderPlotly({
    p <- ggplot(revenue_map_data, aes(x = long, y = lat, group = group, fill = TotalRevenue, text = paste("Country:", region, "<br>Revenue: £", scales::comma(TotalRevenue)))) +
      geom_polygon(color = "white") +
      scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "grey50", labels = scales::comma, guide = "colorbar") +
      labs(fill = "Revenue (£)") +
      theme_void() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$sales_distribution_customer_plot <- renderPlotly({
    sales_distribution <- data %>%
      group_by(CustomerID) %>%
      summarize(TotalSales = sum(Quantity * UnitPrice, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(TotalSales)) %>%
      head(100)
    
    p <- plot_ly(
      sales_distribution, 
      x = ~CustomerID, 
      y = ~TotalSales, 
      type = 'scatter', 
      mode = 'markers',
      marker = list(
        size = 10,
        color = ~TotalSales,
        colorscale = 'Viridis',
        showscale = TRUE
      ),
      text = ~paste("Customer ID:", CustomerID, "<br>Total Sales: £", scales::comma(TotalSales))
    ) %>%
      layout(
        xaxis = list(title = "Customer ID", tickangle = 45),
        yaxis = list(title = "Total Sales (£)", tickformat = ","),
        hovermode = 'closest'
      )
    
    p
  })
  
  output$customer_retention_plot <- renderPlotly({
    # Calculate the top 10 customers
    top_customers <- data %>%
      group_by(CustomerID) %>%
      summarize(TotalSales = sum(Quantity * UnitPrice)) %>%
      arrange(desc(TotalSales)) %>%
      slice(1:10) %>%
      pull(CustomerID)
    
    # Calculate the retention rate for each of the top 10 customers
    retention_data <- data %>%
      filter(CustomerID %in% top_customers) %>%
      mutate(Month = floor_date(InvoiceDate, "month")) %>%
      group_by(CustomerID, Month) %>%
      summarize(TotalPurchases = n(), .groups = "drop") %>%
      arrange(CustomerID, Month) %>%
      group_by(CustomerID) %>%
      mutate(RetentionRate = (TotalPurchases - lag(TotalPurchases)) / lag(TotalPurchases) * 100) %>%
      filter(!is.na(RetentionRate)) %>%
      ungroup()
    
    # Create the plot with multiple lines for each customer
    p <- ggplot(retention_data, aes(x = Month, y = RetentionRate, color = factor(CustomerID))) +
      geom_line() +
      labs(x = "Month", y = "Retention Rate (%)", color = "Customer ID") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)