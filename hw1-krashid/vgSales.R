library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
#install.packages("shinythemes")
library(shinythemes)

sales <- read.csv("vgsales.csv")
sales <- data.frame(sales)
typeof(sales)

# Define UI for application that plots features of movies -----------
ui <- fluidPage(theme = shinytheme("united"),
  
  # Application title -----------------------------------------------
  titlePanel("Video Game Sales across the world"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Sales in North America (in millions)" = "NA_Sales", 
                              "Sales in Europe (in millions)" = "EU_Sales", 
                              "Sales in Japan (in millions)" = "JP_Sales", 
                              "Sales in the rest of the world (in millions)" = "Other_Sales", 
                              "Total worldwide sales (in millions)" = "Global_Sales"), 
                  selected = "Total worldwide sales (in millions)"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Year", 
                              "Platform"), #take platform out possibly?
                  selected = "Year"),
      
      # Select variable for color -----------------------------------
      selectInput(inputId = "z",
                  label = "Color by:",
                  choices = c("Genre",
                              "Publisher",
                              "Platform"),
                  selected = "Genre"),
      
      # Set alpha level ---------------------------------------------
      sliderInput(inputId = "alpha",
                  label = "Alpha:",
                  min = 0, max = 1,
                  value = 0.5),
      
      # Set point size ----------------------------------------------
      sliderInput(inputId = "size", 
                  label = "Size:", 
                  min = 0, max = 5, 
                  value = 2),
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      # Enter text for scatterplot title ---------------------------------------------
      textInput(inputId = "plot_title", 
                label = "Scatterplot title", 
                placeholder = "Title for scatterplot"),
      
      # Horizontal line for visual separation -----------------------
      hr(),
      
      # Select which types of movies to plot ------------------------
      checkboxGroupInput(inputId = "selected_genre",
                         label = "Select video game genre:",
                         choices = c("Action", 
                                     "Adventure", 
                                     "Fighting", 
                                     "Misc", 
                                     "Platform", 
                                     "Puzzle", 
                                     "Racing", 
                                     "Role-Playing", 
                                     "Shooter", 
                                     "Simulation", 
                                     "Sports", 
                                     "Strategy"),
                         selected = "Role-Playing"),
    
      
      #download button
      downloadButton(outputId = "sales_download", 
                     label = "Download",
                     class = "butt1"),
      tags$head(tags$style(".butt1{background-color:pink;} .butt1{color: gray;}")), 
    ),
    
    # Output: -------------------------------------------------------
    mainPanel(
      
      # Show scatterplot --------------------------------------------
      plotOutput(outputId = "scatterplot"),
      br(),        # a little bit of visual separation
      
      # # Show barplot --------------------------------------------
      plotOutput(outputId = "barplot"),
      br(), br(),        # a little bit of visual separation

      # # Show summary table ---------------------------------------------
      tableOutput(outputId = "summtable"),
      br(), br(),
      
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "salestable")
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected genre types ------
  # Selected type is the checkbox
  sales_subset <- reactive({
    req(input$selected_genre) # ensure availablity of value before proceeding
    filter(sales, Genre %in% input$selected_genre)
  })

  # Convert plot_title toTitleCase ----------------------------------
  pretty_plot_title1 <- reactive({ toTitleCase(input$plot_title) })
  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = sales_subset(), aes_string(x = input$x, y = input$y,
                                              color = input$z)) +
      geom_point(size = input$size, alpha = input$alpha) +
      theme(plot.title = element_text(face = "plain", size = 18)) +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           y = toTitleCase(str_replace_all(input$y, "_", " ")),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = pretty_plot_title1()
      )
  })
  
  # Create barplot object  --
  output$barplot <- renderPlot({
      ggplot(data = sales, aes_string(x = input$x, y = "Global_Sales")) +
      stat_summary(fun = sum, geom = "bar") +
      geom_bar(fill = "pink", alpha = 1, stat = "identity") +
      theme(plot.title = element_text(face = "plain", size = 18)) +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           y = "Total Worldwide Sales (in millions)",
           title = "Global Sales across Years/Platforms"
      )
  })
  
  #data for the summary table
  genre_table <- reactive({
    req(input$selected_genre)
    sales %>%
        group_by(Genre) %>%
        summarise('Total Sales (in millions)' = sum(Global_Sales)) 
  })
  
  # Print summary table  -------------------------------------
  output$summtable <- renderTable({
    t(genre_table())
  },
  align = "c",
  width = "1200px",
  rownames = TRUE, colnames = FALSE)
  
  # Print data table if checked -------------------------------------
  output$salestable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = sales[, 1:7], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  #Download button
  output$sales_download <- downloadHandler('sales.csv', content = function(file){
    sales_subset
    write.table(sales, file, sep = ",", row.names = FALSE)
  })

}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)