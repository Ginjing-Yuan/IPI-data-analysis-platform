library(shiny)
shinyUI(fluidPage(
  titlePanel("IPI data analysis platform"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file", multiple = TRUE), # fileinput() function is used to get the file upload contorl option
      helpText("Default max. file size is 5MB"),
      helpText("Select the read.table parameters below"),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
      numericInput(inputId = 'row',label = 'The number of row you need to skip',1),
      uiOutput("selectfile"),
      uiOutput("selectelement"),
      uiOutput("selectrange")
    ),
    mainPanel(
      uiOutput("tb")
      
    )
    
  )
))
