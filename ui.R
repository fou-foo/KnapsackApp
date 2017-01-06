# ui.R
library(ggvis)
shinyUI(fluidPage(
	titlePanel("UFO Reports (ggvis demo)"),
	
	sidebarLayout(
		sidebarPanel(
			helpText("See the number of UFO reports in relation to 
					 the number of military bases by state (USA)."),
      br(),
      helpText("Hover over the data point to see the state and values."),
      
      sliderInput("size", label = "Point Size",min = 10, max = 100, value = 20),
      selectInput("model", label = "Model type", 
                     choices = c("Linear" = "lm", "LOESS" = "loess"),
                     selected = "lm"),
            
      uiOutput("ggvis_ui")
          
    ),
    
    mainPanel(
      
      ggvisOutput('ggvis'))
    )
))