library(shiny)
ui<- fluidPage(
  headerPanel(title = "Neuron Simulation"),
  sidebarPanel(
  actionButton(inputId = "simB2",label = "Simulate Neurons"),
  textInput(inputId = "dir",label = "",placeholder = "Select directory for generated neurons"),
  numericInput(inputId = "NN",label = "Number of simulations",value = 1,min = 1),
  actionButton(inputId = "GenS",label = "Generate model performance statistics"),
  textInput(inputId = "dir2",label = "",placeholder = "Select directory of current data"),
  textInput(inputId = "dir3",label = "",placeholder = "Select directory of simulated data"),
  actionButton(inputId = "RetM",label = "Retrain Model"),
  textInput(inputId = "dir4",label = "",placeholder = "select directory of new data"),
  numericInput(inputId = "eps",label = "eps parameter",value = 60,min = 0),
  #actionButton(inputId = "GenS",label = "Model Queries"),
  actionButton(inputId = "simB3",label = "Default models"),
  textOutput(outputId = "Reporter"),
  textOutput(outputId = "Helper")
  ),
  mainPanel(

  #imageOutput(outputId = "neurV")
  htmlOutput(outputId = "neurV")
)

)

server <- function(input,output){
  getPage<-function() {
    return(tags$iframe(src = "https://lrodriguezlujan.github.io/neuroviewer/"
                       , style="width:100%;",  frameborder="0"
                       ,id="iframe"
                       , height = "1000px"))
  }

  output$neurV<-renderUI({
    #x <- input$test
    getPage()
  })
   rv <- reactiveValues(pressed = "")
   neur <- reactiveValues(pressed = "")
   models_retrained <<-0


  observeEvent(input$simB,{

    neur$pressed <- simulate_neuron_2()
    dirsn <- input$dir
    strdir <- paste(dirsn,"/",sep = "")
    strdir <- paste(strdir,"Simulated_Neuron_",sep="")
    strdir <- paste(strdir,as.character(input$simB),sep = "")
    strdir <- paste(strdir,".json",sep = "")

    write_2_JSON(neuron, path = strdir)

    str <- paste("Simulated neuron saved in",sep="")
    str <- paste(str,dirsn)
    rv$pressed <- str
  })


  observeEvent(input$simB2,{

    dirsn <- input$dir
    for(i in 1:(input$NN)){
      neuron <- simulate_neuron_2()
      strdir <- paste(dirsn,"/",sep = "")
      strdir <- paste(strdir,"Simulated_Neuron_",sep="")
      strdir <- paste(strdir,as.character(i),sep = "")
      strdir <- paste(strdir,".json",sep = "")
      write_2_JSON(neuron,path = strdir)
    }
    str <- paste(as.character(input$NN)," simulated neurons saved in")
    str <- paste(str,dirsn)
    rv$pressed <- str

  })


  observeEvent(input$GenS,{

    file_path_real <- input$dir2
    file_path_synthetic <- input$dir3
    eps <- input$eps
    strdir <- paste(file_path_synthetic,"/",sep = "")
    strdir <- paste(strdir,"Model performance statistics",sep = "")
    rv$pressed <- "Running tests... this may take some minutes."
    test_results <- Testing(file_path_real,file_path_synthetic,eps)
    rv$pressed <- paste ("Statistics file generated in",strdir)
  })

    observeEvent(input$RetM,{
      file_path_data <- input$dir4
      eps <- input$eps
      models<-mega_retrain(file_path_data,eps)
      rv$pressed <- "Models retrained and selected"
      models_retrained <<- 1
      })

    observeEvent(input$simB3,{
      models_retrained <<- 0
      rv$pressed <- "Default models selected"
    })

    output$Reporter<- renderText({
     rv$pressed
  })

}

shinyApp(ui=ui,server=server)


