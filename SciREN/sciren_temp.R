####
#creating a ui and server for bird diversity/primary productivity relationship in place of plant type and CO2 uptake
#template for sciren




ui <- fluidPage(  ## creates display that auto adjusts to user's device dimensions
  # Main title
  titlePanel("SciREN Example 1"),
  # Establish sidebar layout:
  sidebarLayout(
    # Create sidebar (which we'll fill with input controls)
    sidebarPanel(
      # Create a check box input control allowing multiple items to be checked (like bird fam or spp)
      checkboxGroupInput(inputId = "AOU", label = "Bird Species", choices = levels(bbs_sci$AOU), 
                         selected = levels(bbs_sci$AOU))  
      ## Note: levels() shows all unique "names" of a class = factor object
    ),
    # Spot for the plot
    mainPanel(
      plotOutput(outputId = "scatter.plot")  ## "scatter.plot" output object is created in server.R
    )
  ))

server <- function(input, output) {
  
  # renderPlot indicates that the function is "reactive" - it will automatically re-execute when input changes
  output$scatter.plot <- renderPlot({
    
    # Render the plot:    
    ## We'll create a stable plot size based on all data before adding [changing] subsets using points().
    plot(aveN ~ ndvi, data = bbs_sci, type = "n")  ## type = "n" causes no points to be drawn.
    points(aveN ~ ndvi, data = bbs_sci[bbs_sci$AOU  %in% c(input$type),])
    title(main = "Bird diversity Trends")
  })
}

shinyApp(ui = ui, server = server) #should make app 
