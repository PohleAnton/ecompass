
library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("ECOmpass Revenue Calculator Final Update"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("percentageOfUsers",
                  "Percentage of Total Visitors:",
                  min = 0,
                  max = 5,
                  value = 0.4,
                  step = 0.1),
      sliderInput("conversionRate",
                  "Conversion Rate for Explorers (%):",
                  min = 0,
                  max = 100,
                  value = 2),
      sliderInput("bUsers",
                  "Number of Businesses:",
                  min = 0,
                  max = 50,
                  value = 15),
      numericInput("bFee",
                   "Monthly Fee for Businesses (€):",
                   value = 50),
      numericInput("addRevenue",
                   "Monthly Advertisement Revenue (€):",
                   value = 2000),
      numericInput("numCities",
                   "Number of Cities:",
                   value = 1),
      numericInput("totalCost",
                   "Total Cost (€):",
                   value = 10000)
      
    ),
    mainPanel(
      plotOutput("revenuePlot"),
      plotOutput("profitPlot")
      
    )
  )
)


server <- function(input, output) {
  output$revenuePlot <- renderPlot({
    #note: i am using 2.7 million here to roughly account for the local population as well
    aUsers <- 2700000 * (input$percentageOfUsers / 100) * (input$conversionRate / 100)
    aRevenue <- round(aUsers * 3.99 * 12) * input$numCities
    bRevenue <- round(input$bUsers * input$bFee * 12) * input$numCities
    adRevenue <- round(input$addRevenue * 12) * input$numCities
    totalRevenue <- aRevenue + bRevenue + adRevenue
    
    data <- data.frame(Category = c("Explorers", "Partner Businesses", "Ad Revenue", "Total Revenue"),
                       Revenue = c(aRevenue, bRevenue, adRevenue, totalRevenue),
                       Type = c("Explorers", "Partner Businesses", "Ad Revenue", "Total"))
    
    ggplot(data, aes(x = Category, y = Revenue, fill = Type)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Annual Revenue Breakdown per City", y = "Revenue (€)", x = NULL) +
      geom_text(aes(label = format(round(Revenue), big.mark = ",")), 
                position = position_stack(vjust = 0.5)) +
      guides(fill=guide_legend(title="Revenue Source"))
  })
  output$profitPlot <- renderPlot({
    # Recalculate revenues and profits with the number of cities
    aUsers <- 2700000 * (input$percentageOfUsers / 100) * (input$conversionRate / 100)
    aRevenue <- round(aUsers * 3.99 * 12) * input$numCities
    bRevenue <- round(input$bUsers * input$bFee * 12) * input$numCities
    adRevenue <- round(input$addRevenue * 12) * input$numCities
    totalRevenue <- aRevenue + bRevenue + adRevenue
    netProfit <- totalRevenue - input$totalCost
    
    # Adjust the order by setting a factor with levels in the desired order
    financialData <- data.frame(
      Category = factor(c("Total Cost", "Total Revenue", "Net Profit"), 
                        levels = c("Total Cost", "Total Revenue", "Net Profit")),
      Amount = c(-input$totalCost, totalRevenue, netProfit),
      Type = c("Cost", "Revenue", "Profit")
    )
    
    # Determine color for Net Profit based on value
    financialData$Color <- ifelse(financialData$Category == "Net Profit" & financialData$Amount >= 0, "black", "red")
    financialData$Color[financialData$Type == "Revenue"] <- "deepskyblue"
    financialData$Color[financialData$Type == "Cost"] <- "orange2"
    
    ggplot(financialData, aes(x = Category, y = Amount, fill = Color)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      scale_fill_identity() +
      labs(title = "Financial Overview per City", y = "Amount (€)", x = NULL) +
      geom_text(aes(label = format(round(Amount), big.mark = ",")),
                position = position_dodge(width = 0.9), vjust = -0.25)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


