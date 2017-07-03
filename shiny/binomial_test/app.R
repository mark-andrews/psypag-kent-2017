library(shiny)
library(ggplot2)
library(latex2exp)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  withMathJax(),
   
   # Application title
   titlePanel("Sampling theory based inference of a coin's bias"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        helpText("Calculate the p-value for a hypothesis test on the true value
                 of a coin's bias after observing the outcomes of a series of coin flips. 
                 Use the controls to specify the total number of coin flips, 
                 and the total number of times a Heads is the observed outcome. 
                 The areas shaded in blue represent the probability of observing an outcome
                 as or more extreme than the one observed assuming the hypothesis being tested is true."),
         sliderInput("N",
                     "Number of coin flips:",
                     min = 1,
                     max = 250,
                     value = 150, 
                     step = 1),
         uiOutput("n.obs.slider"),
         sliderInput("theta",
                     "Hypothesized bias on the coin: ",
                     min = 0,
                     max = 1.0,
                     value = 0.5),
        htmlOutput("text")
      ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot", width="100%")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  

  
  
  output$n.obs.slider <- renderUI({
    sliderInput("n.obs", 
                "Observed number of Heads: ", 
                min=0, 
                max=input$N, 
                step=1,
                value=max(1, 0.4*input$N))
  })
  
  output$text <- renderUI({
    
    withMathJax()
    
    # Do I have to repeat this? Probably not, but anyway. 
    if (is.null(input$n.obs)){
      n.obs <- max(1, 0.4*input$N)
    } else {
      n.obs <- input$n.obs
    }
    
    results <- binom.test(n.obs, 
                          input$N, 
                          p=input$theta)
    
    stmt.1 <- sprintf("Having observed %d coin flips, of which %d are Heads and %d are tails, 
                       the probability of observing an outcome <em>as or more extreme</em> than this, 
                       assuming that the true value of the bias is %2.2f, is p=%2.3f. This is the area 
                       shaded in <font color='#56B4E9'>blue</font>.", 
                      input$N,
                      n.obs,
                      input$N - n.obs,
                      input$theta,
                      results$p.value)
    
    stmt.2 <- sprintf("The confidence interval on the coin's bias is from %2.3f to %2.3f.", 
                      results$conf.int[1],
                      results$conf.int[2])
    
    HTML(paste(stmt.1, stmt.2, sep = '<br/>'))
  })
  
   output$distPlot <- renderPlot({
     
     withMathJax()
     
     expected.value <- input$N*input$theta
     
     x <- seq(0, input$N)
     y <- dbinom(x, size=input$N, prob = input$theta)
 
     if (is.null(input$n.obs)){
       n.obs <- max(1, 0.4*input$N)
     } else {
       n.obs <- input$n.obs
     }
     
     z <- abs(expected.value-x) >= abs(expected.value-n.obs)
     
     Df <- data.frame(x = x,
                      y = y,
                      z = z)
     
     ggplot(Df,
             mapping=aes(x = x, y = y, fill=z)) + 
       geom_col(width = 0.65) +
       theme_classic() + 
       guides(fill=FALSE) +
       scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
       xlab('Observed number of Heads') +
       ylab('Probability') +
       ggtitle(sprintf('Binomial sampling distribution: Trials=%d, probability of \"success\" is %2.2f.', input$N, input$theta))

   }, height = 600, width = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)

