####
#creating a ui and server for bird diversity/primary productivity relationship in place of plant type and CO2 uptake
#template for sciren




ui <- fluidPage(  ## creates display that auto adjusts to user's device dimensions
  # Main title
  titlePanel("Shiny Workshop Example - CO2 Uptake in Plants"),
  # Establish sidebar layout:
  sidebarLayout(
    # Create sidebar (which we'll fill with input controls)
    sidebarPanel(
      # Create a check box input control allowing multiple items to be checked
      checkboxGroupInput(inputId = "type", label = "Plant Type", choices = levels(dat$Type), 
                         selected = levels(dat$Type))  
      ## Note: levels() shows all unique "names" of a class = factor object
    ),
    # Spot for the plot
    mainPanel(
      plotOutput(outputId = "scatter.plot")  ## "scatter.plot" output object is created in server.R
    )
  ))


