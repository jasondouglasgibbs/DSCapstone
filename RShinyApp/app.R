## This Shiny App is for the Capstone project for the Johns Hopkins Data Science Specialization on Coursera.##
## This Shiny App loads a trained corpus (corpus_trained.rda) from the working directory and uses the SBO package##
## to predict next words based on the data set. The only dependencies for this app are the app.R file, the##
## Instructions.Rmd file, and the corpus_trained.rda file. Everything else is included for completeness.##

library(shiny)
library(shinydashboard)
library(knitr)
library(rmarkdown)
library(sbo)

##Series of markdown rendering functions to properly render the instructions page.##
##Other methods of rendering produced errant HTML tags at the top of the instructions.##
rmarkdown::render(input="Instructions.Rmd", output_format = "html_document")
xml2::write_html(rvest::html_node(xml2::read_html("Instructions.html"), "body"), file = "Instructions2.html")

ui <- dashboardPage(skin="green",
                    
                    ##Application title.##
                    dashboardHeader(titleWidth=350, title = "Next Word Predictor"),
                    
                    ##Sidebar with text inputs.##
                    dashboardSidebar(width=350,
                                     sidebarMenu(
                                         textInput("Phrase", h5("Enter your phrase/word:"), "")
                                     )
                    )
                    ,
                    
                    # Provides brief instructions on the app from the Instructions.Rmd file.
                    dashboardBody(includeHTML("Instructions2.html"),
                                  verbatimTextOutput("PredictedPhrase"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$PredictedPhrase<- renderText({
        if(!isTruthy(input$Phrase)){
            "No input."
        }else{
            load("corpus_trained.rda")
            corpus_trained2<-sbo_predictor(corpus_trained)
            predictvector<-predict(corpus_trained2, input$Phrase)
            if (predictvector[1]=="<EOS>"&&predictvector[2]!="said"&&predictvector[3]!="will"){
                predictvector[1]<-predictvector[2]
                
            }
            if (predictvector[1]=="<EOS>"&&predictvector[2]!="said"&&predictvector[3]!="will"){
                predictvector[1]<-predictvector[3]
                
            }
            
            if (predictvector[1]=="<EOS>"&&predictvector[2]!="said"&&predictvector[3]!="will"){
                predictvector[1]<-"No prediction available."
                
            }
            
            if (predictvector[1]=="<EOS>"&&predictvector[2]=="said"&&predictvector[3]=="will"){
                predictvector[1]<-"said"
                
            }
            
            if(predictvector[1]=="<EOS>"){
                predictvector[1]<-"No prediction available."
            }
            
            predictvector[1]
        }
    }
    )
        
}
    
    


# Run the application 
shinyApp(ui = ui, server = server)
