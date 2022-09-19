library(shiny)



ui <- fluidPage( titlePanel("RS1 Color System Grade Calculator") ,
                 sidebarPanel(
                 helpText("Enter the total points for each color. If a color was not part of the quiz, enter 0."),
                 helpText("Total Points", style = "font-size:120%; font-weight:bold;"),  
                 numericInput("Green_Tot", "Total Green Points", value=0, min=0, max=100),
                 numericInput("Blue_Tot", "Total Blue Points", value=0, min=0, max=100),
                 numericInput("Red_Tot", "Total Red Points", value=0, min=0, max=100),   
                 br(),
                 helpText("Your Scores", style = "font-size:120%; font-weight:bold;"),                 
                 numericInput("Green_Score", "Your Green Score", value=0, min=0, max=100),
                 numericInput("Blue_Score", "Your Blue Score", value=0, min=0, max=100),
                 numericInput("Red_Score", "Your Red Score", value=0, min=0, max=100)),                 
                 tableOutput("table"),
                 textOutput("ScoreNoColors"),
                 br(),
                 span(textOutput("ScoreWithColors"), style="color:red"),
                 textOutput("weighted_total"),
                 br(),
                 br(),
                 helpText("Created by Mr. Ng, with supervision from Casper the cat."),
                 img(src='Casper.png', align='left')

)



server <- function(input, output, session) {

  
  # Total_Points<- input$Green_Tot + input$Blue_Tot + input$Red_Tot
  output$table <- renderTable({
    if(!is.null(input$Green_Tot | input$Blue_Tot | input$Red_Tot)){
      table_data <- data.frame(
        color= c('Green', 'Blue', 'Red'),
        scores= c(input$Green_Score, input$Blue_Score, input$Red_Score),
        percentages= paste(round(c(input$Green_Score, input$Blue_Score, input$Red_Score)/ c(input$Green_Tot, input$Blue_Tot, input$Red_Tot)*100, 2), "%"),
        color_weighting= c(0.52, 0.24, 0.2)
      )
      
      table_data$weighted_scores <- round(c(input$Green_Score, input$Blue_Score, input$Red_Score)/ c(input$Green_Tot, input$Blue_Tot, input$Red_Tot)*100, 2) * table_data$color_weighting
      
      colnames(table_data) <- c('Color', 'Your Score', 'Percentage Correct', 'Color System Weighting', 'Your Weighted Score')
      }
      
    table_data
  })
  
  
  output$ScoreNoColors <- renderText(
    {paste("Your Score Without the Color System (Raw Percentage):", round((input$Green_Score + input$Blue_Score + input$Red_Score)/(input$Green_Tot + input$Blue_Tot + input$Red_Tot ), 2)*100, "%")
    }
  )
  

  
  
  
  
  output$ScoreWithColors <- renderText(
    { weighted_tot <- sum(c(as.integer(as.logical(input$Green_Tot)), as.integer(as.logical(input$Blue_Tot)), as.integer(as.logical(input$Red_Tot))) * c(0.52, 0.24, 0.2))
      paste("Your Score With Colors:", 
           round(((sum(
             na.omit(c(input$Green_Score, input$Blue_Score, input$Red_Score)/ c(input$Green_Tot, input$Blue_Tot, input$Red_Tot) * c(0.52, 0.24, 0.2)))) / weighted_tot) * 100, 2), 
           "%"
           )}
  )

}




shinyApp(ui, server)