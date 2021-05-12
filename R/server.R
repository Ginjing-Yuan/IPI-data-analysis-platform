
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
      a=Fi[,input$Substances][input$range[1]:input$range[2]]
      h=Fi[,1][input$range[1]:input$range[2]]
      S=numeric(length(h)-1)
      for(t in 1 : length(h)) {
        S[t]=(a[t]+a[t+1])*(h[t+1]-h[t])/2
      }
      return(sum(na.omit(S)))
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
         sliderInput("range","Range of baseline integral",1,length(datareading()[,1]),
                     c(length(datareading()[,1])/length(datareading()[,1]),length(datareading()[,1])/2),step = 1)
    )
  })
  
  output$manual_integral<-renderUI({
    if(is.null(input$file)) {return()}
    list(hr(), 
         helpText("Do you want to integrate it manually?"),
         selectInput("manual", "manual integration", choices=c("No"="N","Yes"="Y"))
    )
  })
  
  output$manual_integral_slider_tailing_problems<-renderUI({
    if(is.null(input$file)){return()}
    if(input$manual == "N") {return()}
    list(hr(), 
         helpText("Select the the range you need to integral manually"),
         selectInput("integration_methods","Which problem does this figure have?",choices=c("Tailing"="T","Double peak"="D")),
         sliderInput("manual_range","Range of manual integral for tailing problem",1,length(datareading()[,1]),
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
            axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+
      ylab(input$Substances)
    
  })
  
  output$inte<-renderPlot({
    if(is.null(input$file)){return()}
    if(input$manual == "N") {
      ggplot(datareading()[input$range[1]:input$range[2],],aes(datareading()[input$range[1]:input$range[2],][,1],y= get(input$Substances)))+
        geom_point(aes(),size=0.5)+
        xlab("Time (s)")+
        geom_area(aes(),fill="red",alpha=0.5)+
        scale_x_continuous(limits=c(0, max(datareading()[,1])))+
        theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20),
              axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+
        ylab(input$Substances)
    }else{
      if(input$integration_methods=="D"){
        ggplot(datareading()[input$range[1]:input$range[2],],aes(datareading()[input$range[1]:input$range[2],][,1],y= get(input$Substances)))+
          geom_point(aes(),size=0.5)+
          xlab("Time (s)")+
          geom_area(aes(),fill="red",alpha=0.5)+
          scale_x_continuous(limits=c(0, max(datareading()[,1])))+
          theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20),
                axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+
          geom_segment(aes(x=datareading()[,1][input$manual_range[1]],xend=datareading()[,1][input$manual_range[2]],
                           y=datareading()[,input$Substances][input$manual_range[1]],yend=datareading()[,input$Substances][input$manual_range[1]]))+
          geom_segment(aes(x=datareading()[,1][input$manual_range[2]],xend=datareading()[,1][input$manual_range[2]],
                           y=datareading()[,input$Substances][input$manual_range[1]],yend=datareading()[,input$Substances][input$manual_range[2]]))+
          ylab(input$Substances)
      }else{
      #df=data.frame(t=c(datareading()[,1][input$manual_range[1]],datareading()[,1][input$manual_range[2]]),
      #element=c(datareading()[,input$Substances][input$manual_range[1]],datareading()[,input$Substances][input$manual_range[2]]))
      #lm=lm(element~t,df)
      ggplot(datareading()[input$range[1]:input$range[2],],aes(datareading()[input$range[1]:input$range[2],][,1],y= get(input$Substances)))+
        geom_point(aes(),size=0.5)+
        xlab("Time (s)")+
        geom_area(aes(),fill="red",alpha=0.5)+
        scale_x_continuous(limits=c(0, max(datareading()[,1])))+
        theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20),
              axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))+
        geom_segment(aes(x=datareading()[,1][input$manual_range[1]],xend=datareading()[,1][input$manual_range[2]],
                         y=datareading()[,input$Substances][input$manual_range[1]],yend=datareading()[,input$Substances][input$manual_range[2]]))+
        ylab(input$Substances)
      #geom_abline(aes(intercept = lm$coefficients[1],slope = lm$coefficients[2]), size=1, alpha=0.5)
      }
    }
    
  })
  
  
  output$selectedarea<-renderText({
    if(is.null(input$file)){return()}
    paste("Integral area of range you select is:",trapezoid())
  })
  
  manual_tropezoid_tailing<-reactive({
    file1=input$file
    if(is.null(file1)){return()}
    ti=function(Fi){
      a=c(Fi[,input$Substances][input$manual_range[1]],Fi[,input$Substances][input$manual_range[2]])
      h=c(Fi[,1][input$manual_range[1]],Fi[,1][input$manual_range[2]])
      S=numeric(1)
      S=(a[1]+a[2])*(h[2]-h[1])/2
      
      return(sum(na.omit(S)))
    }
    return(ti(datareading()))
  })
  
  manual_tropezoid_doublepeak<-reactive({
    file1=input$file
    if(is.null(file1)){return()}
    ti=function(Fi){
      a=c(Fi[,input$Substances][input$manual_range[1]],Fi[,input$Substances][input$manual_range[2]])
      h=c(Fi[,1][input$manual_range[1]],Fi[,1][input$manual_range[1]])
      S=numeric(1)
      S=(a[1]+a[2])*(h[2]-h[1])/2
      
      return(sum(na.omit(S)))
    }
    return(ti(datareading()))
  })
  
  output$manual_selectedarea<-renderText({
    if(is.null(input$file)){return()}
    if(input$manual == "N"){return()}
    else{
      if(input$integration_methods=="D"){
        paste("Manual integral area of range you select is:",trapezoid()-manual_tropezoid_doublepeak())
      }
      else{
        paste("Manual integral area of range you select is:",trapezoid()-manual_tropezoid_tailing())
      }
    }
    
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
                                mainPanel(h3(textOutput("selectedarea"))),
                                mainPanel(h3(textOutput("manual_selectedarea")))
                 ))
      )
  })
})

