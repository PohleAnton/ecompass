library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  titlePanel("ECOmpass Revenue Calculator"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "yearInputTabs",
        tabPanel("Year 1", 
                 sliderInput("percentageOfUsersY1", "Percentage of Total Visitors Year 1:", min = 0, max = 5, value = 0.4, step = 0.1),
                 sliderInput("conversionRateY1", "Conversion Rate for Explorers Year 1 (%):", min = 0, max = 100, value = 2),
                 sliderInput("bUsersY1", "Number of Businesses Year 1:", min = 0, max = 50, value = 15),
                 numericInput("bFeeY1", "Monthly Fee for Businesses Year 1 (€):", value = 50),
                 numericInput("addRevenueY1", "Monthly Advertisement Revenue Year 1 (€):", value = 2000),
                 numericInput("numCitiesY1", "Number of Cities Year 1:", value = 1),
                 numericInput("totalCostY1", "Total Cost Year 1 (€):", value = 10000)
        ),
        tabPanel("Year 2", 
                 sliderInput("percentageOfUsersY2", "Percentage of Total Visitors Year 2:", min = 0, max = 5, value = 0.4, step = 0.1),
                 sliderInput("conversionRateY2", "Conversion Rate for Explorers Year 2 (%):", min = 0, max = 100, value = 2),
                 sliderInput("bUsersY2", "Number of Businesses Year 2:", min = 0, max = 50, value = 15),
                 numericInput("bFeeY2", "Monthly Fee for Businesses Year 2 (€):", value = 50),
                 numericInput("addRevenueY2", "Monthly Advertisement Revenue Year 2 (€):", value = 2000),
                 numericInput("numCitiesY2", "Number of Cities Year 2:", value = 0),
                 numericInput("totalCostY2", "Total Cost Year 2 (€):", value = 10000)
        ),
        tabPanel("Year 3", 
                 sliderInput("percentageOfUsersY3", "Percentage of Total Visitors Year 3", min = 0, max = 5, value = 0.4, step = 0.1),
                 sliderInput("conversionRateY3", "Conversion Rate for Explorers Year 3 (%):", min = 0, max = 100, value = 2),
                 sliderInput("bUsersY3", "Number of Businesses Year 3:", min = 0, max = 50, value = 15),
                 numericInput("bFeeY3", "Monthly Fee for Businesses Year 3 (€):", value = 50),
                 numericInput("addRevenueY3", "Monthly Advertisement Revenue Year 3 (€):", value = 2000),
                 numericInput("numCitiesY3", "Number of Cities Year 3:", value = 0),
                 numericInput("totalCostY3", "Total Cost Year 3 (€):", value = 10000)
        )
     
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "yearOutputTabs",
        tabPanel("Year 1 Revenue", plotOutput("revenuePlotY1")),
        tabPanel("Year 2 Revenue", plotOutput("revenuePlotY2")),
        tabPanel("Year 3 Revenue", plotOutput("revenuePlotY3"))
      )
    )
  )
)

server <- function(input, output) {
  
  renderRevenuePlotForYear <- function(year) {
    renderPlot({
      percentageOfUsers <- input[[paste0("percentageOfUsersY", year)]]
      conversionRate <- input[[paste0("conversionRateY", year)]]
      bUsers <- input[[paste0("bUsersY", year)]]
      bFee <- input[[paste0("bFeeY", year)]]
      addRevenue <- input[[paste0("addRevenueY", year)]]
      numCities <- input[[paste0("numCitiesY", year)]]
      totalCost <- input[[paste0("totalCostY", year)]]
      
      aUsers <- 2700000 * (percentageOfUsers / 100) * (conversionRate / 100)
      aRevenue <- round(aUsers * 3.99 * 12) * numCities
      bRevenue <- round(bUsers * bFee * 12) * numCities
      adRevenue <- round(addRevenue * 12) * numCities
      totalRevenue <- aRevenue + bRevenue + adRevenue
      
      data <- data.frame(
        Category = c("Explorers", "Partner Businesses", "Ad Revenue", "Total Revenue"),
        Revenue = c(aRevenue, bRevenue, adRevenue, totalRevenue),
        Type = c("Explorers", "Partner Businesses", "Ad Revenue", "Total")
      )
      
    
      ggplot(data, aes(x = Category, y = Revenue, fill = Type)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set3") +
        labs(title = paste("Annual Revenue Breakdown per City - Year", year), y = "Revenue (€)", x = NULL) +
        geom_text(aes(label = format(round(Revenue), big.mark = ",")), position = position_stack(vjust = 0.5)) +
        guides(fill=guide_legend(title="Revenue Source"))
    })
  }
  
  output$revenuePlotY1 <- renderRevenuePlotForYear(1)
  output$revenuePlotY2 <- renderRevenuePlotForYear(2)
  output$revenuePlotY3 <- renderRevenuePlotForYear(3)
}

# Run the application
shinyApp(ui = ui, server = server)