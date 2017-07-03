library(shiny)
library(ggplot2)
library(latex2exp)

# Load some functions
source('utils.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  withMathJax(),
  
   # Application title
   titlePanel("Bayesian inference of a coin's bias"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       helpText("Calculate the likelihood and posterior probability of a coin's bias given and observed sequence of coin tosses.
                Use the controls to specify the total number of coin flips, 
               and the total number of times a Heads is the observed outcome.
                Select a Beta distribution prior by choosing the (hyper)-parameters of the Beta
                distribution."),
       sliderInput("N",
                   "Number of coin flips:",
                   min = 1,
                   max = 250,
                   value = 125, 
                   step = 1),
       uiOutput("n.obs.slider"),
       sliderInput("alpha",
                   "\\(\\alpha\\) hyperparameter of prior (Beta) distribution:",
                   min = 0.01,
                   max = 50.0,
                   value = 3.0),
       sliderInput("beta",
                   "\\(\\beta\\) hyperparameter of prior (Beta) distribution:",
                   min = 0.01,
                   max = 50.0,
                   value = 3.0),
       sliderInput("hpd.mass",
                   "High posterior density interval:",
                   min = 0.01,
                   max = 0.99,
                   value = 0.9, 
                   step = 0.01),
       htmlOutput("text")
     ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$text <- renderUI({


    
    withMathJax()
    
    # Do I have to repeat this? Probably not, but anyway. 
    if (is.null(input$n.obs)){
      n.obs <- max(1, 0.4*input$N)
    } else {
      n.obs <- input$n.obs
    }
    
    this.hpd.interval <- beta.hpd.interval(input$alpha + n.obs, input$beta + input$N - n.obs, hpd.mass=input$hpd.mass)    
    posterior.summary <- beta.summary(input$alpha + n.obs, input$beta + input$N - n.obs)
    
    log.bayes.factor <- lbeta(input$alpha + n.obs, input$beta + input$N - n.obs) - lbeta(input$alpha, input$beta) - -input$N*log(2)
    
    stmt.1 <- sprintf("<li>Having observed %d coin flips, of which %d are Heads and %d are tails, the posterior 
                      distribution on the true value of the coin's bias, assuming a Beta(%2.2f, %2.2f) prior, 
                      has a mean of %2.3f, a mode of %2.3f, and 
                      a standard deviation of %2.3f.</li>", 
                      input$N,
                      n.obs,
                      input$N - n.obs,
                      input$alpha,
                      input$beta,
                      posterior.summary$mean,
                      posterior.summary$mode,
                      posterior.summary$sd)
    
    stmt.2 <- sprintf("<li>The %d%% high posterior density (HPD) interval is from %2.3f to %2.3f.</li>",
                      100*input$hpd.mass,
                      this.hpd.interval$hpd.interval[1],
                      this.hpd.interval$hpd.interval[2])
    
    stmt.3 <- sprintf("<li>The Bayes factor for this model against a point null model at 0.5 is %2.2f. </li>",
                      exp(log.bayes.factor))
    
    stmt.4 <- sprintf("<li>The posterior predictive probability that the next flip of the coin, i.e. coin flip %d, will be a Heads is %2.3f.</li>",
                      input$N + 1,
                      posterior.summary$mean)
                      
    
    HTML(paste('<ul>', 
               paste(stmt.1, stmt.2, stmt.4, stmt.3, sep = '<br/>'),
               '</ul>'))
    
  })
  

  output$n.obs.slider <- renderUI({
    sliderInput("n.obs", 
                "Observed number of Heads: ", 
                min=0, 
                max=input$N, 
                step=1,
                value=max(1, 0.4*input$N))
  })
  
  output$distPlot <- renderPlot({
    
    withMathJax()
    
    if (is.null(input$n.obs)){
      n.obs <- max(1, 0.4*input$N)
    } else {
      n.obs <- input$n.obs
    }
    
    theta <- seq(0, 1, by = 0.001)
    ll <- dbeta(theta, n.obs + 1.0, input$N - n.obs + 1.0) # normalized likelihood is Beta distribution with uniform prior
    prior <- dbeta(theta, input$alpha, input$beta)
    posterior <- dbeta(theta, input$alpha + n.obs, input$beta + (input$N - n.obs))
    
    HPD <- beta.hpd.segment(input$alpha + n.obs, 
                            input$beta + (input$N - n.obs), 
                            hpd.mass=input$hpd.mass)
    
    Df <- data.frame(theta,
                     ll,
                     prior,
                     posterior)
    
    ggplot(Df) + 
      geom_line(mapping=aes(x = theta, y = ll, col='likelihood')) +
      geom_line(mapping=aes(x = theta, y = prior, col='prior')) +
      geom_line(mapping=aes(x = theta, y = posterior, col='posterior')) +
      theme_classic() + 
      xlab('bias') +
      ylab('P(bias|data)') +
      labs(col=NULL) + 
      theme(text = element_text(size=20),
            axis.text.x = element_text(size=10)) + 
      geom_segment(aes(x = HPD[['x']],
                       y = HPD[['y']],
                       xend = HPD[['xend']],
                       yend = HPD[['yend']]),
                   col='black')
    
  }, height = 600, width = 800)

}

# Run the application 
shinyApp(ui = ui, server = server)