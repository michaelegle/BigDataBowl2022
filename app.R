if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")

library(dplyr)
library(ggplot2)
library(shiny)

#read in the plot data from punt_angles.R
plot_data <- read.csv('plot_data.csv') %>% select(-1)

#we're making this its own function for convenience
get_plot <- function(yl){
  
  #This just helps put the title in proper football terms
  yardl <- case_when(
    yl > 50 ~ yl-100,
    T ~ yl
  )
  
  #Just makes the plot
  g <- ggplot(plot_data %>% filter(yardline == yl))+
    geom_tile(mapping = aes(x = phi, y = max_s, fill = res))+
    scale_fill_manual(values = c('Fair Catch'='forestgreen','Downed'='violet','Return'='red',
                                  'Touchback'='darkblue'))+
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )+
    labs(
      x = 'Phi (deg)',
      y = 'Punt Speed (yds/sec)',
      title = paste0('Likeliest Punt Outcomes at ',yardl,' Yardline'),
      caption = 'By Drop The Net | Data from Big Data Bowl 2022',
      fill = 'Punt Outcome'
    )
  
  return(g)
}

#This is the most basic of basic UI. 
#Since this app is supposed to be a glorified calculator, I don't see the need to make it more fancy
ui <- fluidPage(
  
  titlePanel("Drop the Net 2022 Big Data Bowl Submission"),
  
  sidebarLayout(
    
    #this is the input section
    sidebarPanel(
      
      sliderInput(inputId = "yl",
                  label = "Distance from the Endzone:",
                  min = 40,
                  max = 99,
                  value = 70)
      
    ),
    
    #this is where the plot pops out
    mainPanel(
      
      plotOutput(outputId = "distPlot")
      
    )
  )
)

#this just actually runs the function we defined and pumps out the plot
server <- function(input, output) {
  
 output$distPlot <- renderPlot({
    get_plot(as.double(input$yl)) #it gave me a weird error if i didn't do this
  })
  
}

#for personal running and checking
shinyApp(ui, server)
