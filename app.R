#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(psych)

pku <- read.csv("pku.csv",sep='\t',stringsAsFactors=FALSE,dec=",")
select <- pku[,c(5,1)]

res <- list()
for (i in seq(1,nrow(select)-1)) {
    key <- select[i,][[1]]
    value <- select[i,][[2]]
    res[[key]] <- c(value,res[[key]])
}

## Only run examples in interactive R sessions
# if (interactive()) {

# demoing optgroup support in the `choices` arg
shinyApp(

    ui = fluidPage(
       
        selectInput("foodstuff", "Velg en matvare:",
                                 res
                                 ),

        numericInput("gram", "gram:", 10, min = 1, max = 100),
        verbatimTextOutput("value"),
                     
        textOutput("result")
    
    ),

  server = function(input, output) {
      output$result <- renderText({

          phe <- 50/(pku[which(pku[c(1)]==input$foodstuff),
                                    c(2)])
          enheter <- suppressWarnings(as.numeric(phe))
          
          if ( is.numeric(enheter) && enheter > 0 ) {
              enheter <- input$gram / enheter
          
              paste("Phe:",phe, "Du valgte", input$gram, " gram av ",
                    input$foodstuff,
                    ". Det gir ",              
                    format(enheter, digits=2, nsmall=2)  ,
                    "enheter.")
          } else {
              paste ("Det er ikke protein i ", input$foodstuff)
         }
      })
  }
)
#}
