
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)
library(ggplot2)
library(baseline)

shinyServer(function(input,output) {
  
  ## input$file is a data frame and contains the details around the name, size and temp location of the files uploaded
  # this reactive output display the content of the input$file dataframe
  
  
  datareading<-reactive({
    file1=input$file
    if(is.null(file1)){return()}
    read.table(file=input$file$datapath[input$file$name==input$Select],
               sep = input$sep, 
               header=input$header, 
               stringsAsFactors = input$stringAsFactors,
               fill = T,
               skip = input$row)
  })
  
  trapezoid<-reactive({
    file1=input$file
    if(is.null(file1)){return()}
    ti=function(Fi){
      a=Fi[,2][input$range[1]:input$range[2]]
      h=Fi[,1][input$range[1]:input$range[2]]
      S=numeric(length(h)-1)
      for(t in 1 : length(h)) {
        S[t]=(a[t]+a[t+1]-2*(mean(sort(na.omit(a))[1:10])+3*sd(sort(na.omit(a))[1:10])))*(h[t+1]-h[t])/2
      }
      return(sum(na.omit(S[-which(S<0)])))
    }
    return(ti(datareading()))
  })
  
  
  output$filedf <- renderTable({
    if(is.null(input$file)){return ()}
    input$file # the file input data frame object that contains the file attributes
  })
  
  # Extract the file path for file
  output$filedf2 <- renderTable({
    if(is.null(input$file)){return ()}
    input$file$datapath # the file input data frame object that contains the file attributes
  })
  
  ## Below code to display the structure of the input file object
  output$fileob <- renderPrint({
    if(is.null(input$file)){return ()}
    str(input$file)
  })
  
  # Following code displays the select input widget with the list of file loaded by the user
  output$selectfile <- renderUI({
    if(is.null(input$file)) {return()}
    list(hr(), 
         helpText("Select the files for which you need to see data and summary stats"),
         selectInput("Select", "Select", choices=input$file$name)
    )
    
  })
  
  output$selectelement<-renderUI({
    if(is.null(input$file)) {return()}
    list(hr(), 
         helpText("Select the element for which you need to see scatter diagram"),
         selectInput("Substances", "Substances", choices=names(datareading()))
    )
  })
  
  output$selectrange<-renderUI({
    if(is.null(input$file)) {return()}
    list(hr(), 
         helpText("Select the the range you need to integral"),
         sliderInput("range","Range of integral",1,length(datareading()[,1]),
                     c(length(datareading()[,1])/length(datareading()[,1]),length(datareading()[,1])/2),step = 1)
    )
  })
  
  
  ## Summary Stats code ##
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$summ <- renderPrint({
    if(is.null(input$file)){return()}
    summary(datareading())
  })
  
  ## Dataset code ##
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({ 
    if(is.null(input$file)){return()}
    datareading()
    
  })
  
  output$scatter<-renderPlot({
    if(is.null(input$file)){return()}
    ggplot(datareading(),aes(datareading()[,1],y= get(input$Substances)))+geom_point()+
      geom_line(aes(datareading()[,1],y=getBaseline(baseline(matrix(get(input$Substances), nrow = 1)))))+
      xlab("Time (s)")+
      theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20),
            axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))
    
  })
  
  output$inte<-renderPlot({
    if(is.null(input$file)){return()}
    ggplot(datareading()[input$range[1]:input$range[2],],aes(datareading()[input$range[1]:input$range[2],][,1],y= get(input$Substances)))+
      geom_point()+
      xlab("Time (s)")+
      geom_area(aes(),fill="red")+
      scale_x_continuous(limits=c(0, max(datareading()[,1])))+
      theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20),
            axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))
    
  })
  
  output$selectedarea<-renderText({
    if(is.null(input$file)){return()}
    paste("Integral area of range you select is:",trapezoid())
  })
  
  
  ## MainPanel tabset renderUI code ##
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. 
  # Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(
        tabPanel("Input File Object DF ", tableOutput("filedf"), tableOutput("filedf2")),
        tabPanel("Input File Object Structure", verbatimTextOutput("fileob")),
        tabPanel("Dataset", tableOutput("table")),
        tabPanel("Summary Stats", verbatimTextOutput("summ")),
        tabPanel("Scatter diagram of the selected element", 
                 verticalLayout(mainPanel(plotOutput("scatter")),
                                mainPanel(plotOutput("inte")),
                                mainPanel(textOutput("selectedarea"))
                 ))
      )
  })
})

