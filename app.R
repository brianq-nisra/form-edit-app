
library("pacman")

p_load("shiny", "DT", "dplyr")

mtcars2 <- mtcars %>%
  mutate(model = rownames(.),
         rec_no = 1:nrow(.),
         colour = rep_len(c("Red", "Blue", "Green", "Black"), length.out = nrow(.))) %>%
  select(rec_no, model, colour, everything())

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("App for editing records in a form"),
    
    mainPanel(width = 12,
              tabsetPanel(type = "tabs",
                          
                          tabPanel("Form",
                                   
                                   p("Select record number to edit or select new to create new one"),
                                   div(class = "row", style = "display: flex;",
                                       div(style = "padding:6px 0 0 15px;", actionLink("prev", "<<")),
                                       div(style = "padding:0 15px 0 15px;", selectInput("recNum", label = NULL, choices = c("(new)", mtcars2$rec_no), selected = "(new)", width = "80px")),
                                       div(style = "padding-top: 6px;", actionLink("next1", ">>")),
                                       div(style = "padding-left: 15px;", textInput("search", label = NULL, placeholder = "Search"))),
                                   textInput("model", "Make and model"),
                                   textInput("colour", "Colour"),
                                   numericInput("mpg", "Miles per gallon", value = 0),
                                   numericInput("cyl", "Number of cylinders", value = 0),
                                   numericInput("disp", "Displacement (cu.in)", value = 0, step = 0.1),
                                   numericInput("hp", "Horsepower", value = 0),
                                   numericInput("drat", "Rear axle ratio", value = 0, step = 0.01),
                                   numericInput("wt", "Weight (1000 lbs)", value = 0, step = 0.001),
                                   numericInput("qsec", "1/4 mile time", value = 0, step = 0.01),
                                   numericInput("vs", "Engine (0 = V-shaped, 1 = straight)", value = 0, min = 0, max = 1),
                                   numericInput("am", "Transmission (0 = automatic, 1 = manual)", value = 0, min = 0, max = 1),
                                   numericInput("gear", "Number of forward gears", value = 0),
                                   numericInput("carb", "Number of carburetors", value = 0),
                                   div(class = "row", style = "display: flex;",
                                       actionButton("add", "Add new"),
                                       div(style = "padding-left: 15px;", actionButton("edit", "Confirm Edit")),
                                       div(style = "padding-left: 15px;", actionButton("delete", "Delete Record")))
                                   ),
                          
                          tabPanel("Data",
                                   dataTableOutput("carsData"))))
    
)

# Important to add session to server function for updateXInput to work ####
server <- function(input, output, session) {
  
  carsDT <- function() {
    datatable(mtcars2,
              rownames = FALSE,
              escape = FALSE,
              options = list(
                paging = FALSE,
                searching = TRUE,
                scrollX = TRUE,
                scrollY = "800px",
                autoWidth = FALSE,
                ordering = TRUE,
                dom = "Bfrtip"),
              class = "display")
  }
  
  # Output data table onto second tab
  output$carsData <- renderDT(server = FALSE, {
   
    carsDT()
    
  })
  
  # What happens when you click button to add new record ####
  observeEvent(input$add, {
    
    # Single row data frame
    newRecord <- data.frame(rec_no = max(mtcars2$rec_no) + 1,
                            model = input$model,
                            colour = input$colour,
                            mpg = input$mpg,
                            cyl = input$cyl,
                            disp = input$disp,
                            hp = input$hp,
                            drat = input$drat,
                            wt = input$wt,
                            qsec = input$qsec,
                            vs = input$vs,
                            am = input$am,
                            gear = input$gear,
                            carb = input$carb)
    
    # Bind to new data set and re-render datatable 
    mtcars2 <<- rbind(mtcars2, newRecord)
    
    output$carsData <- renderDT(server = FALSE, {
      carsDT()
    })
    
    # Add new item to drop down list
    updateSelectInput(session, "recNum", choices = c("(new)", mtcars2$rec_no), selected = max(mtcars2$rec_no))
    
  })
  
  
  
  # When you click the next arrow it increments the selectInput by 1 ####
  observeEvent(input$next1, {
    
    if (input$recNum != max(mtcars2$rec_no)) {
    
      updateSelectInput(session,
                        "recNum",
                        selected = c("(new)", mtcars2$rec_no)[which(c("(new)", mtcars2$rec_no) == input$recNum) + 1])
      
    } else {
      
      updateSelectInput(session, "recNum", selected = "(new)")
      
    }
    
  })
  
  # When you click the back arrow it decreases the selectInput by 1 ####
  observeEvent(input$prev, {
    
    if (input$recNum != "(new)") {
    
      updateSelectInput(session,
                        "recNum",
                        selected = c("(new)", mtcars2$rec_no)[which(c("(new)", mtcars2$rec_no) == input$recNum) - 1])
      
    } else {
      
      updateSelectInput(session, "recNum", selected = max(mtcars2$rec_no))
      
    }
    
  })
  
  # Any time the selectInput changes the rest of the form will update ####
  observeEvent(input$recNum, {
    
    for (var in names(mtcars2)) {

      if (var %in% c("model", "colour")) {
        updateTextInput(session, var, value = if (input$recNum == "(new)") {""} else {mtcars2[[var]][mtcars2$rec_no == as.integer(input$recNum)]})
      } else {
        updateNumericInput(session, var, value = if(input$recNum == "(new)") {0} else {mtcars2[[var]][mtcars2$rec_no == as.integer(input$recNum)]})
      }

    }
    
  })
  
  
  # When you save an edit to a form ####
  observeEvent(input$edit, {
    
    if (input$recNum != "(new)") {
      
      editRecord <- data.frame(rec_no = as.integer(input$recNum),
                               model = input$model,
                               colour = input$colour,
                               mpg = input$mpg,
                               cyl = input$cyl,
                               disp = input$disp,
                               hp = input$hp,
                               drat = input$drat,
                               wt = input$wt,
                               qsec = input$qsec,
                               vs = input$vs,
                               am = input$am,
                               gear = input$gear,
                               carb = input$carb)

      
      mtcars2 <<- mtcars2[mtcars2$rec_no != editRecord$rec_no, ] %>%
        rbind(editRecord) %>%
        arrange(rec_no)
      
      output$carsData <- renderDT(server = FALSE, {
        carsDT()
      })
      
      updateSelectInput(session, "recNum", label = NULL, choices = c("(new)", mtcars2$rec_no))
      
    }
    
  })
  
  # When you save an edit to a form ####
  observeEvent(input$delete, {
    
    if (input$recNum != "(new)") {
      
      mtcars2 <<- mtcars2[mtcars2$rec_no != as.integer(input$recNum), ] 
      
      output$carsData <- renderDT(server = FALSE, {
        carsDT()
      })
      
      updateSelectInput(session, "recNum", label = NULL, choices = c("(new)", mtcars2$rec_no))
      
    }
    
  })
  
  # When you type something in search ####
  observeEvent(input$search, {
    
    # When search box isn't blank perform a while loop to return the first row of the data
    # where the search condition is met
    if (input$search != "") {
      
      # initially set looping variables
      foundMatch <- FALSE
      i <- 1
      
      # Loop while foundMatch is false and i is less than number of rows
      while (foundMatch == FALSE & i <= nrow(mtcars2)) {
      
        # Transform row of data into single character string
         r <- tolower(paste(as.character(mtcars2[i, ]), collapse = " "))
         
         # Use grepl to search for input in character string
         if (grepl(tolower(input$search), r)) {
           foundMatch <- TRUE
         } else {
          i <- i + 1
         }
         
      }
      
      # If match is found update the numeric input to 
      if (foundMatch == TRUE) {
        
        updateNumericInput(session, "recNum", value = mtcars2$rec_no[i])
        
      }
        
    } else {
      
      # If search term is deleted go back to blank form
      updateNumericInput(session, "recNum", value = "(new)")
      
    }
    
  }) 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
