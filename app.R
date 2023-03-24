
library("pacman")

p_load("shiny", "DT", "dplyr")

mtcars2 <- mtcars %>%
  mutate(model = rownames(.),
         rec_no = 1:nrow(.)) %>%
  select(rec_no, model, everything())

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("App for editing records in a form"),
    
    mainPanel(width = 12,
              tabsetPanel(type = "tabs",
                          
                          tabPanel("Form",
                                   
                                   p("Select record number to edit or select new to create new one"),
                                   div(class = "row", style = "display: flex;",
                                       div(style = "padding-left: 15px;", actionLink("prev", "<<")),
                                       div(style = "padding:0 15px 0 15px;", selectInput("recNum", label = NULL, choices = c("(new)", mtcars2$rec_no), selected = "(new)", width = "80px")),
                                       div(actionLink("next1", ">>"))),
                                   textInput("model", "Make and model"),
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
                                   numericInput("carb", "Carbon emission score", value = 0),
                                   div(class = "row", style = "display: flex;",
                                       actionButton("add", "Add new"),
                                       div(style = "padding-left: 15px;", actionButton("edit", "Confirm Edit")))
                                   ),
                          
                          tabPanel("Data",
                                   dataTableOutput("carsData"))))
    
)


server <- function(input, output, session) {
  
  carsDT <- function() {
    datatable(mtcars2,
              rownames = FALSE,
              escape = FALSE,
              extensions = c("FixedColumns"),
              options = list(
                paging = FALSE,
                searching = TRUE,
                scrollX = TRUE,
                scrollY = "800px",
                fixedColumns = list(leftColumns = 1), 
                autoWidth = TRUE,
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
    updateSelectInput(session, "recNum", choices = c("(new)", mtcars2$rec_no))
    
  })
  
  
  
  # When you click the next arrow it increments the selectInput by 1 ####
  observeEvent(input$next1, {
    
    if (input$recNum != max(mtcars2$rec_no)) {
    
      updateSelectInput(session,
                        "recNum",
                        selected = c("(new)", 1:nrow(mtcars2))[which(c("(new)", 1:nrow(mtcars2)) == input$recNum) + 1])
      
    } else {
      
      updateSelectInput(session, "recNum", selected = "(new)")
      
    }
    
  })
  
  # When you click the back arrow it decreases the selectInput by 1 ####
  observeEvent(input$prev, {
    
    if (input$recNum != "(new)") {
    
      updateSelectInput(session,
                        "recNum",
                        selected = c("(new)", 1:nrow(mtcars2))[which(c("(new)", 1:nrow(mtcars2)) == input$recNum) - 1])
      
    } else {
      
      updateSelectInput(session, "recNum", selected = max(mtcars2$rec_no))
      
    }
    
  })
  
  # Any time the selectInput changes the rest of the form will update ####
  observeEvent(input$recNum, {
    
    updateTextInput(session, "model", value = if (input$recNum == "(new)") {""} else {mtcars2$model[mtcars2$rec_no == as.integer(input$recNum)]})
    
    updateNumericInput(session, "mpg", value = if (input$recNum == "(new)") {0} else { mtcars2$mpg[mtcars2$rec_no == as.integer(input$recNum)]})
    
    updateNumericInput(session, "cyl", value = if (input$recNum == "(new)") {0} else { mtcars2$cyl[mtcars2$rec_no == as.integer(input$recNum)]})
    
    updateNumericInput(session, "disp", value = if (input$recNum == "(new)") {0} else { mtcars2$cyl[mtcars2$rec_no == as.integer(input$recNum)]})
    
    updateNumericInput(session, "hp", value = if (input$recNum == "(new)") {0} else { mtcars2$hp[mtcars2$rec_no == as.integer(input$recNum)]})
    
    updateNumericInput(session, "drat", value = if (input$recNum == "(new)") {0} else { mtcars2$drat[mtcars2$rec_no == as.integer(input$recNum)]})
    
    updateNumericInput(session, "wt", value = if (input$recNum == "(new)") {0} else { mtcars2$wt[mtcars2$rec_no == as.integer(input$recNum)]})
    
    updateNumericInput(session, "qsec", value = if (input$recNum == "(new)") {0} else { mtcars2$qsec[mtcars2$rec_no == as.integer(input$recNum)]})
    
    updateNumericInput(session, "vs", value = if (input$recNum == "(new)") {0} else { mtcars2$vs[mtcars2$rec_no == as.integer(input$recNum)]})
    
    updateNumericInput(session, "am", value = if (input$recNum == "(new)") {0} else { mtcars2$am[mtcars2$rec_no == as.integer(input$recNum)]})
    
    updateNumericInput(session, "gear", value = if (input$recNum == "(new)") {0} else { mtcars2$gear[mtcars2$rec_no == as.integer(input$recNum)]})
    
    updateNumericInput(session, "carb", value = if (input$recNum == "(new)") {0} else { mtcars2$carb[mtcars2$rec_no == as.integer(input$recNum)]})
    
  })
  
  
  # When you save an edit to a form ####
  observeEvent(input$edit, {
    
    if (input$recNum != "(new)") {
      
      editRecord <- data.frame(rec_no = as.integer(input$recNum),
                               model = input$model,
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)