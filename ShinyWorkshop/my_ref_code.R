#Set up your computer to easily interact during "Intro to Shiny" Workshop (May 3, 2017).
#Create "ShinyWorkshop" folder in working directory AND install missing R packages needed.
#Also opens HTML page to allow you to follow along w/ code walkthrough during workshop.

#Create new folder "ShinyWorkshop" to put files into:
mainDir <- getwd()
subDir <- 'ShinyWorkshop'
dir.create(file.path(mainDir, subDir))

#Download zipped files to subdirectory "ShinyWorkshop":
## The files are currently saved as a .zip file on GitHub, user: theforestecologist
u1 <- "https://raw.githubusercontent.com/theforestecologist/ShinyWorkshop/master/IntroToShinyWorkshop.zip"
download.file(u1, file.path(subDir, basename(u1)))  

#Change Working directory to newly created Workshop subdirectory:
setwd(file.path(mainDir, subDir))

#Unzip files:
utils::unzip("IntroToShinyWorkshop.zip")

#Download/install necessary packages and open Read-along document
source("runShinyWorkshop.R")

#######
install.packages("shiny", repos="http://cran.rstudio.com/")
library(shiny)
#####
ui <- fluidPage()
server <- function(input, output){}
shinyApp(ui = ui, server = server)
#####
x <- faithful$waiting ## waiting times using internal dataset "faithful"
bins <- seq(min(x), max(x), length.out=20+1)
hist(x, breaks=bins, col="darkgray", border="white", 
     main="Old faithful waiting time to next eruption", xlab="Minutes")
####
library(shiny)
runExample("01_hello")
####
str(CO2)
dat <- data.frame(ID = as.numeric(CO2$Plant),CO2)
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
####
server <- function(input, output) {
  
  # renderPlot indicates that the function is "reactive" - it will automatically re-execute when input changes
  output$scatter.plot <- renderPlot({
    
    # Render the plot:    
    ## We'll create a stable plot size based on all data before adding [changing] subsets using points().
    plot(uptake ~ conc, data = dat, type = "n")  ## type = "n" causes no points to be drawn.
    points(uptake ~ conc, data = dat[dat$Type  %in% c(input$type),])
    title(main = "Plant Trends")
  })
}
####
shinyApp(ui = ui, server = server)
####
ui <- fluidPage(  ## creates display that auto adjusts to user's device dimensions
  # Main title
  titlePanel(
    ## paragraph of size-20 text: 
    p("Shiny Workshop Example - CO2 Uptake in Plants", style = "font-size: 20px")), 
  # Establish Sidebar layout:
  sidebarLayout(
    # Create sidebar (which we'll fill with input controls)
    sidebarPanel(width = 3,
                 div(style = "color:green; padding:0px 0px 150px 0px;", #padding is top, right, bottom, left
                     checkboxGroupInput(inputId = "type", label = "Plant Type", choices = levels(dat$Type), 
                                        selected = levels(dat$Type))
                 ) ## Everything held within this div() tag helper function will take on the styles assigned to it 
                 ## (Unless overridden internally) -- see next code example
    ),
    # Spot for the plot
    mainPanel(width = 9,
              plotOutput(outputId = "scatter.plot")
    )
  ))

server <- function(input, output) {
  
  # renderPlot indicates that the function is "reactive" - it will automatically re-execute when input changes
  output$scatter.plot <- renderPlot({
    
    # Render the plot:    
    ## We'll create a stable plot size based on all data before adding [changing] subsets using points().
    plot(uptake ~ conc, data = dat, type = "n")  ##type = "n" causes no points to be drawn.
    points(uptake ~ conc, data = dat[dat$Type  %in% c(input$type),])
    title(main = "Plant Trends")
  })
}

shinyApp(ui = ui, server = server)
####
ui <- fluidPage(
  
  # Title Panel: ####
  titlePanel(
    p("Shiny Workshop Example - CO2 Uptake in Plants", style = "font-size: 20px")
  ),
  
  
  sidebarPanel(width = 3,
               div(style = "color:green; padding:0px 0px 150px 0px;", #padding is top, right, bottom, left
                   p("Data Controls", style = "font-size: 15px; color:blue;"),           
                   
                   # Check box to give us the option whether to show all plants... 
                   # ...or just the plant selected in the slider input below
                   checkboxInput(inputId = "all.plants", label = "Show All Plants", value = T),
                   
                   # Slider bar allows us to select which plant ID to plot.
                   # This slider only works if the above checkboxInput is unchecked (see server.R code).
                   sliderInput(inputId = "plant", label = "Plant ID", min = min(dat$ID), max = max(dat$ID),
                               value = 1),  ## sets starting value at 1
                   
                   # This group check box let's us narrow which plant Types are plotted.
                   # checkboxGroupInput differs from checkboxInput by allowing more than one to be checked.
                   checkboxGroupInput(inputId = "type", label = "Plant Type", choices = levels(dat$Type), 
                                      selected = levels(dat$Type)), ## sets starting checks to all levels 
                   
                   # Radio buttons work like checkboxes, but result in strings/#'s instead of boolean T/F. 
                   # They allow multiple options like checkboxGroupInputs, but only one selection. 
                   radioButtons(inputId = "treat", label = "Treatment", 
                                choices = c(levels(dat$Treatment), "Both"), selected = "Both"),
                   
                   fluidRow(
                     div(style = "color:red;",
                         ## since both columns below are assigned a width of 6 (with 0 offset), 
                         ## they'll fill the tabpanel width equally.
                         column(width = 6, offset = 0,   
                                selectInput(inputId = "colorQ", label = "Quebec Color", choices = palette(), 
                                            selected = "black")
                         ),
                         column(width = 6, offset = 0,
                                selectInput(inputId = "colorM", label = "Miss. Color", choices = palette(), 
                                            selected = "black") 
                         )
                     )
                     
                   )
               ) ## Everything held within this div tag helper function takes on the styles assigned in div()
               ## (Unless overridden internally) -- see the div() function assigned in fluidRow above. 
  ),
  
  #Spot for the plot  
  mainPanel(width = 9,
            plotOutput(outputId = "scatter.plot")
  )
)


server <- function(input, output) {
  
  # We need to Set-up various input values:
  
  ## Allows us to narrow data by one plant or to include all plants. 
  ## This is necessary b/c checkboxInput simply generates T/F, and we must translate that to plant ID #'s. 
  plants.to.plot <- reactive({   ## Note: Reactive objects take on () when used... (see below)
    if(input$all.plants == T) {
      sort(unique(dat$ID))
    } else { input$plant
    }
  })
  
  ## Allows us to narrow data by one treatment level or to include both. 
  ## This is necessary because "both" is not actually a Treatment type, ... 
  ## ...and therefore needs to be assigned something meaningful (i.e., relevant to the data)
  treats.to.plot <- reactive({ 
    if(input$treat == "Both") {
      levels(dat$Treatment)
    } else {input$treat
    }
  }) 
  
  ## provides vector of color options (corresponding to plant$Type) to apply to coloring points in plot
  colors <- reactive({
    Colors <- rep(NA,length(dat2()$Type))
    Colors[which(dat2()$Type == "Quebec")] <- input$colorQ
    Colors[which(dat2()$Type == "Mississippi")] <- input$colorM
    Colors
  })   
  
  # Create modified data set based on our numerous input selections:
  ## This will keep our plotting code neater...
  
  dat2 <- reactive({  dat[dat$ID %in% c(plants.to.plot()) &  ## Note that reactive objects use ()
                            dat$Type  %in% c(input$type) &
                            dat$Treatment %in% c(treats.to.plot()), ] 
  })
  
  
  # Create plot to be rendered in mainpanel. Notice we now use dat2 as the data source. 
  
  output$scatter.plot <- renderPlot({
    plot(uptake ~ conc, data = dat, type = "n")
    points(uptake ~ conc, data = dat2(),col = colors())
    title(main = paste0("Plant(s): ", paste(levels(dat$Plant)[plants.to.plot()],collapse =", ")))
    ## title combines "Plant(s)" with the plant names of whichever plants are included by the inputs. 
  })
}

## note that we can save a shinyApp as an object that, when called, runs the app automatically 
app <- shinyApp(ui = ui, server = server) 
app
####
library(shiny)

ui <- fluidPage(
  
  # Title Panel: ####
  titlePanel(
    p("Shiny Workshop Example - CO2 Uptake in Plants", style = "font-size: 20px")
  ),
  
  sidebarPanel(width = 3,
               div(style = "color:green; padding:0px 0px 150px 0px;", #padding is top, right, bottom, left
                   p("Data Controls", style = "font-size: 15px; color:blue;"),           
                   
                   # Check box to give us the option whether to show all plants 
                   # or just the plant selected in the slider input below
                   checkboxInput(inputId = "all.plants", label = "Show All Plants", value = T),
                   
                   # Slider bar allows us to select which plant ID to plot
                   # This slider only works if the above checkboxInput is unchecked (see server.R code)
                   sliderInput(inputId = "plant", label = "Plant ID", min = min(dat$ID), max = max(dat$ID),
                               value = 1),  ## sets starting value at 1
                   
                   # This group check box let's us narrow which plant Types are plotted.
                   # checkboxGroupInput differs from checkboxInput by allowing more than one to be checked
                   checkboxGroupInput(inputId = "type", label = "Plant Type", choices = levels(dat$Type), 
                                      selected = levels(dat$Type)), ## sets starting checks to all levels 
                   
                   # Radio buttons work like checkboxes, but result in strings/#'s instead of boolean T/F. 
                   # They allow multiple options like checkboxGroupInputs. 
                   radioButtons(inputId = "treat", label = "Treatment", 
                                choices = c(levels(dat$Treatment), "Both"), selected = "Both"),
                   
                   fluidRow(
                     div(style = "color:red;",
                         column(width = 6, offset = 0,
                                selectInput(inputId = "colorQ", label = "Quebec Color", choices = palette(), 
                                            selected = "black")
                         ),
                         column(width = 6, offset = 0,
                                selectInput(inputId = "colorM", label = "Miss. Color", choices = palette(), 
                                            selected = "black") 
                         )
                     )
                   ),
                   
                   ## We'll insert printed R output generated from clicking the plot
                   ## Note: this text is created using nearPoints() in server.R
                   verbatimTextOutput(outputId = "plot.info")
                   
               )
  ),
  
  mainPanel(width = 9,
            
            ## Create tab layout for our app
            ## Note b/c of placement, we will keep side bar consistent, but have changing mainpanel content
            tabsetPanel( 
              
              ## Everything held within this argument will be placed in a tab with title "Scatter Plot"
              tabPanel(title = "Scatter Plot", 
                       
                       plotOutput(outputId = "scatter.plot",click = "plot.click"),
                       ## Note: we've assigned a new input object called "plot.click"
                       
                       ##Let's add a data table to our main panel below the graph. 
                       ## (Note: don't forget to add a comma after `plotOutput()`)
                       dataTableOutput(outputId = "data") 
                       
              ),
              
              tabPanel(title = "Tab 2",
                       p("We can add another graph here!!",style = "font-size:38px;" )
                       
              )
            )
  )
)


server <- function(input, output) {
  
  # We need to Set-up various input values:
  
  ## Allows us to narrow data by one plant or to include all plants. 
  ## This is necessary b/c checkboxInput simply generates T/F, and we must translate that to plant ID #'s. 
  plants.to.plot <- reactive({   ## Note: Reactive objects take on () when used... (see below)
    if(input$all.plants == T) {
      sort(unique(dat$ID))
    } else { input$plant
    }
  })
  
  ## Allows us to narrow data by one treatment level or to include both. 
  ## This is necessary because "both" is not actually a Treatment type, ... 
  ## ...and therefore needs to be assigned something meaningful (i.e., relevant to the data)
  treats.to.plot <- reactive({ 
    if(input$treat == "Both") {
      levels(dat$Treatment)
    } else {input$treat
    }
  }) 
  
  ## provides vector of color options (corresponding to plant$Type) to apply to coloring points in plot
  colors <- reactive({
    Colors <- rep(NA,length(dat2()$Type))
    Colors[which(dat2()$Type == "Quebec")] <- input$colorQ
    Colors[which(dat2()$Type == "Mississippi")] <- input$colorM
    Colors
  })   
  
  # Create modified data set based on our numerous input selections:
  ## This will keep our plotting code neater...
  
  dat2 <- reactive({  dat[dat$ID %in% c(plants.to.plot()) & ## Note that reactive objects use ()
                            dat$Type  %in% c(input$type) &
                            dat$Treatment %in% c(treats.to.plot()), ] 
  })
  
  
  # Create plot to be rendered in mainpanel. Notice we now use dat2 as the data source. 
  
  output$scatter.plot <- renderPlot({
    plot(uptake ~ conc, data = dat, type = "n")
    points(uptake ~ conc, data = dat2(),col = colors())
    title(main = paste0("Plant(s): ", paste(levels(dat$Plant)[plants.to.plot()],collapse =", ")))
    ## title combines "Plant(s)" with the plant names of whichever plants are included by the inputs. 
  })
  
  
  # Create data.frame showing data of only the plants (i.e. the rows) selected to be plotted
  
  output$data <- renderDataTable({ dat2()},
                                 options = list(lengthMenu = list(c(5, 10,-1), list("5", "10", "All")), 
                                                pageLength = 5, ordering = T))
  ## These options allow for modification of the data table presented.
  ## This is a rare case in which UI formatting is assigned in server.R code
  ## lengthMenu creates drop down menu giving you choice in # of rows to show.
  ## (-1 = show all rows)
  ## pageLength assigns how many rows are shown when table is 1st rendered
  ## ordering allows you to click on columns to change ordering of the data.
  
  
  # Create data to be generated when the scatter plot is clicked 
  
  ## Note: input$plot.click is generated by clicking on the scatter plot. 
  ## This plot-click input object provides x & y coordinates of where you click 
  
  ## We could use the nearPoints function to do this, but I don't like that it has a "messy" NULL output
  
  #output$plot.info <- renderPrint({
  #nearPoints(dat2()[,2:6], input$plot.click, xvar = "conc", yvar = "uptake",threshold=3)})
  
  ## Instead, I'll make my own function that provides a custom string of text when nothing is clicked. 
  
  #Update to make plot clicker info look better:
  nearPoints2 <- function(coordinfo,blank.text = "Click on Plot Points for Details", cols, ...) {
    if (is.null(coordinfo)) {
      cat(blank.text,"\n")   #"Click on Plot Points for Details"
    } else { 
      points.info.table <- nearPoints(coordinfo = coordinfo,...)
      print(points.info.table[,cols], row.names = F)
    }
  }
  
  #Create table containing columns 2:6 of dat2 for the point clicked and assign to renderPrint output:
  output$plot.info <- renderPrint({
    nearPoints2(coordinfo = input$plot.click, cols = c(2:6), df = dat2(), xvar = "conc", 
                yvar = "uptake",threshold=3)
  })  ## Note: nearPoints (default or nP2) actually reports all points (i.e., rows in your data.frame)  
  ## that are within the threshold value distance from your click. 
  ## So you might want to shrink `threshold` if your clicks result in too many data.
}

app <- shinyApp(ui = ui, server = server)
app
