# Load packages -----------------------------------------------------
library(shiny)
library(openintro)
library(gridExtra)
library(BHH2)
library(ggplot2)
library(data.table)
library(DT)

# Define UI ---------------------------------------------------------
#ui <- pageWithSidebar(
ui <- fluidPage(
    
    #headerPanel("Central Limit Theorem for Means"),
    titlePanel("Central Limit Theorem"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
              
                selectInput(inputId = "dist",
                            label = "Population Distribution:",
                            choices = c(
                                "Normal" = "rnorm", 
                                "Exponential" = "rexp",
                                "Uniform" = "runif", 
                                "Poisson" = "rpois", 
                                "Binomial" = "rbinom",
                                "Log Normal" = "rlnorm",
                                "Beta" = "rbeta"),
                            selected = "rnorm"),  
              
            
            uiOutput("mu"), #rnorm
            uiOutput("sd"), #rnorm
            uiOutput("min"), #runif
            uiOutput("max"), #runif
            uiOutput("skew"), # for Log Normal and Beta
            uiOutput("rate"), # for rexp
            uiOutput("size"), # rbinom
            uiOutput("probability"), # for rbinom
            uiOutput("lambda"), # for rpois
            
            
            sliderInput("n", 
                        "Sample size:", 
                        value = 30,
                        min = 2, 
                        max = 300)
            
            ),
# Create Tabbed Panel ---------------------------------------------------------
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Distribution of Sample Means", plotOutput("sampling.dist"),
                                 div(textOutput("sampling.descr"), align = "center")),
                        tabPanel("Population Distribution", plotOutput("population.dist"),
                                div(textOutput("populationDistributionText"), align = "center")),
                        tabPanel("Distributions of Samples", plotOutput("sample.dist"),
                                 div(h3(textOutput("num.samples")), align = "center"))
            )
        )    
    )
)

# Define server function --------------------------------------------

seed = as.numeric(Sys.time())

server <- function(input, output) {
    
    output$mu = renderUI(
        {
        req(input$dist)
        if (input$dist == "rnorm")
        {
            numericInput("mu",
                        "Mean",
                        value = 0)
        }
    })
  
    output$sd = renderUI(
        {
        req(input$dist)
        if (input$dist == "rnorm")
        {
            numericInput("sd",
                        "Standard deviation",
                        value = 1,
                        min = .01)
        }
    })
  
    output$min = renderUI(
        {
        req(input$dist)
        #print("min")
        if (input$dist == "runif")
        {
            sliderInput("min",
                        "Lower Bound",
                        value = 0,
                        min = 0,
                        max = 20)
        }
    })
  
    output$max = renderUI(
        {
        req(input$dist)
        #print("max")
        if (input$dist == "runif")
        {
            sliderInput("max",
                        "Upper Bound",
                        value = 2,
                        min = 1,
                        max = 20)
        }
        })
  
    output$skew = renderUI(
        {
        req(input$dist)
        #print("skew options")
        if (input$dist == "rlnorm" | input$dist == "rbeta"){
            selectInput(inputId = "skew",
                        label = "Skew",
                        choices = c("Low skew" = "low",
                                    "Medium skew" = "med",
                                    "High skew" = "high"),
                        selected = "low")
        }
    })
  
    output$size = renderUI(
        {
          req(input$dist)
          if (input$dist == "rbinom")
          {
              numericInput("size", 
                           "Size",
                           value = 10)
          }
        })
  
    output$probability = renderUI(
        {
          req(input$dist)
          if (input$dist == "rbinom")
          {
              sliderInput("probability", 
                          label = "Probability",
                          min = 0,
                          max = 1,
                          value = 0.5)
          }
      })
  
    output$lambda = renderUI(
        {
          req(input$dist)
          if (input$dist == "rpois")
          {
              numericInput("lambda", 
                           "Lambda",
                           min = 0,
                           value = 2)
          }
      })
  
    output$rate = renderUI(
        {
        req(input$dist)
            if (input$dist == "rexp")
            {
                sliderInput("rate", 
                            "Rate",
                            min = .01,
                            max = 10,
                            value = 1)
          }
        })

#   "rate"  for rexp
#   "size" for rbinom
#   "probability" for rbinom
#   "lambda" for rpois

# rand_draw function -----------------------------------------------------------
    
    # rand_draw creates a population distribution 
    # Output: returns a population vector based on the user selected input distribution function
    
  rand_draw = function(dist, n, mu, sd, min, max, skew, rate, size, probability, lambda) 
  {
    vals = NULL
    if (dist == "rbeta") {
      if (skew == "low"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=2))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=1.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=1)) 
      }
    }     
    else if (dist == "rnorm"){
      mean = input$mu ; sd = input$sd 
      vals = do.call(dist, list(n=n, mean=mu, sd=sd))
    } 
    else if (dist == "rexp"){
        rate = input$rate
        vals = do.call(dist, list(n=n, rate=rate))
    }
    else if (dist == "rlnorm"){
      if (skew == "low"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=.25))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=1))
      }
    }
    else if (dist == "runif"){
      vals = do.call(dist, list(n=n, min=min, max=max))
    }    
    return(vals)
  }

# Create population and sample data ------------------------------------------   
    
  # the repeatable function always uses the same seed when called
  rep_rand_draw = repeatable(rand_draw)  
  
  # parent returns the same population distribution each time it is called based 
  # on the called functions and inputs.
  parent = reactive({
    req(input$dist, input$mu, input$sd)
    n = 1e5
    return(rep_rand_draw(input$dist, n, input$mu, input$sd, input$min, input$max, input$skew, input$rate, input$size, input$probability, input$lambda))
  })

  # samples from the population distribution 
  # returns a matrix of samples, each value is a sample, each column is a sample set
  samples = reactive({
    req(parent())
    population = parent()
    n = input$n
    k = 10000

    return(replicate(k, sample(population, n, replace=TRUE)))
  })
  
# Plot: Distribution of Sample Means -------------------------------------------------
  
    output$sampling.dist = renderPlot({
      
        L = NULL ; U = NULL ; error = FALSE
      
        if (input$dist == "runif"){
            L = input$min ; U = input$max
            if (L > U){
                error = TRUE
            }
        }
      
        if (error)
            return
          
        else{
              
            distname = switch(input$dist,
                                rnorm = "normal population",
                                rlnorm  = "right skewed population",
                                rbeta = "left skewed population",
                                runif = "uniform population",
                                rexp = "exponential population")   
              
              
            n = input$n
            k = 10000
            
            population = parent()
              
            m_population =  round(mean(population),2)
            sd_population = round(sd(population),2)
              
            ndist = colMeans(samples())
            ndistDataTable = data.table(sampleMeans = colMeans(samples()))
           
              
            m_samp =  round(mean(ndist),2)
            sd_samp = round(sd(ndist),2)
              
            ndens=density(ndist)
            nhist=hist(ndist, plot=FALSE)
              
            #Old code
            
              # if (input$dist == "rnorm"){
              #   hist(ndist, main = paste("Sampling distribution:\nDistribution of means of ", k, 
              #                            " random samples, each\nconsisting of ", n, 
              #                            " observations from a ", distname, sep=""),              
              #        xlab="Sample means", freq=FALSE,
              #        xlim=c(min(-100,population),max(100,population)),
              #        ylim=c(0, max(ndens$y, nhist$density)),
              #        col=COL[2,2], border = "white", 
              #        cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
              #   legend_pos = ifelse(m_samp > 40, "topleft", "topright")
              #   legend(legend_pos, inset = 0.025, 
              #          legend=bquote(atop("mean of " ~ bar(x)==.(m_samp),"sd of " ~ bar(x) ~ "(SE)" ==.(sd_samp))), 
              #          bty = "n", cex = 1.5, text.col = COL[2,2], text.font = 2)
              # }
              
              
              if (input$dist == "rnorm"){
                  ggplot(ndistDataTable, aes(sampleMeans, stat(density))) + geom_histogram()
              }
              else{
                  hist(ndist, main=paste("Distribution of means of ", k, 
                                         " random samples, each\nconsisting of ", n, 
                                         " observations from a ", distname, sep=""), 
                       xlab="Sample means", freq=FALSE, ylim=c(0, max(ndens$y, nhist$density)),
                       col=COL[2,3], border = "white", 
                       cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
                  legend_pos = ifelse(m_samp > 40, "topleft", "topright")
                  legend(legend_pos, inset = 0.025, 
                         legend=bquote(atop("mean of " ~ bar(x)==.(m_samp),"sd of " ~ bar(x) ~ "(SE)" ==.(sd_samp))), 
                         bty = "n", cex = 1.5, text.col = COL[2], text.font = 2)
              }
          }
      })
  
  # Sampling Distribution Text 
  
  output$sampling.descr = renderText({
      
      distname = switch(input$dist,
                        rnorm = "normal population.",
                        rlnorm  = "right skewed population.",
                        rbeta = "left skewed population.",
                        runif = "uniform population.",
                        rexp = "exponential population.")  
      
      L = NULL ; U = NULL ; error = FALSE
      
      if (input$dist == "runif"){
          L = input$min ; U = input$max
          if (L > U){
              error = TRUE
          }
      }
      
      if (error)
          paste0("Error!")
      
      else{
          
          k = 10000
          n = input$n
          paste("Distribution of ", k, "meaned samples,\n
                each meaned sample consists of", n, " observations
                from a", distname)
      }
      })  
  
# Plot: Population Distribution ------------------------------------------------

output$population.dist = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "Population distribution: Normal",
                      rlnorm = "Population distribution: Right skewed",
                      rbeta = "Population distribution: Left skewed",
                      runif = "Population distribution: Uniform",
                      rexp = "Population Distribution: Exponential")

    population = parent()
    m_population =  round(mean(population),2)
    sd_population = round(sd(population),2)
    mu = input$mu

    populationDataTable = data.table(parent())
    
    
    L = NULL
    U = NULL

    error = FALSE

    if (input$dist == "runif"){
        L = input$min
        U = input$max
        if (L > U){
            error = TRUE
        }
    }

    if (error){
        plot(0,0,type='n',axes=FALSE,xlab="",ylab="",mar=c(1,1,1,1))
        text(0,0,"Error: Lower bound greater than upper bound.",col="red",cex=2)
    }
    else{
        
        
        ggplot(populationDataTable, aes(V1, fill=COL[2,3])) + geom_histogram(bins = 200)
        
        # if (input$dist == "rnorm"){
        #     ggplot(populationDataTable, aes(V1)) + geom_histogram()
        # }
        # 
        # else if (input$dist == "runif"){
        #     ggplot(populationDataTable, aes(V1)) + geom_histogram()
        # }
        # 
        # else if (input$dist == "rlnorm"){
        #     ggplot(populationDataTable, aes(V1)) + geom_histogram() 
        # }
        #  
        # else if (input$dist == "rbeta"){
        #     ggplot(populationDataTable, aes(V1)) + geom_histogram()
        # }
        # 
        # else if (input$dist == "rexp"){
        #     ggplot(populationDataTable, aes(V1)) + geom_histogram()
        # }
    }
})
  

    
# Render Population Distribution Text
#populationDistributionText
  
  output$populationDistributionText <- renderText({
  
  population = parent()
  m_population =  round(mean(population),2)
  sd_population = round(sd(population),2)
  mu = input$mu
  
  populationDataTable = data.table(parent())
  
          
        paste0("Data: ", populationDataTable[1:10])   
    })
  
  
# Plot: Sample Distribution --------------------------------------------------
  
  output$sample.dist = renderPlot({
    
    L = NULL ; U = NULL ; error = FALSE
    
    if (input$dist == "runif"){
      L = input$min
      U = input$max
      if (L > U){
        error = TRUE
      }
    }
    
    if (error)
      return
    
    else{
      
      par(mfrow=c(3,3))
      x = samples()
      
      par(mfrow=c(2,4))
      for(i in 1:8){
        BHH2::dotPlot(x[,i], col = COL[2,3],
                      main = paste("Sample",i),
                      xlab = "", pch=19,
                      #ylim = c(0,2), xlim = c(min(-100,x),max(100,x))
                      ylim = c(0,2), xlim = c(min(x), max(x)),
                      cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
        box()
        mean_samp = round(mean(x[,i]),2)
        sd_samp = round(sd(x[,i]),2)
        legend("topright",
               legend=bquote(atop(bar(x)[.(i)]==.(mean_samp),
                                  s[.(i)]==.(sd_samp))),
               bty = "n", cex = 1.5, text.font = 2)
        abline(v=mean_samp, col=COL[2],lwd=2)
      }
    }
  })
  
# Render "Distribution of Samples" Text ------------------------------------------------------------------
  
  output$num.samples = renderText({
    L = NULL ; U = NULL ; error = FALSE
    
    if (input$dist == "runif"){
      L = input$min ; U = input$max
      if (L > U){
        error = TRUE
      }
    }
    
    if (error)
      paste0()
    
    else{
      
      k = 10000
      paste0("... continuing to Sample ",k,".")
    }
  })
  

  
# text
  output$CLT.descr = renderText({
    
    L = NULL ; U = NULL ; error = FALSE
    
    if (input$dist == "runif"){
      L = input$min ; U = input$max
      if (L > U){
        error = TRUE
      }
    }
    
    if (error)
      paste0()
    
    else{
      population = parent()
      m_population =  round(mean(population),2)
      s_population = round(sd(population),2)
      
      n = input$n
      se=round(s_population/sqrt(n),2)
      paste("According to the Central Limit Theorem (CLT), the distribution of sample means 
            (the sampling distribution) should be nearly normal. The mean of 
            the sampling distribution should be approximately equal to the population mean (", m_population, ") 
            and the standard error (the standard deviation of
            sample means) should be approximately equal to the SD of the population divided by square root of
            sample size (", s_population,
            "/sqrt(",n, ") =", se,").")
    }
  })
}
# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
