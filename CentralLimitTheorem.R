

# Load packages -----------------------------------------------------
library(shiny)
library(openintro)
library(gridExtra)
library(BHH2)
library(ggplot2)
library(data.table)
library(DT)

# Define UI ---------------------------------------------------------

ui <- fluidPage(
    
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
                                "Beta" = "rbeta",
                                "Student T" = "rt",
                                "Chi Squared" = "rchisq"),
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
            uiOutput("df"), #rchisq, rt
            uiOutput("ncp"), #rchisq, rt, rbeta
            uiOutput("meanlog"), #rlnorm
            uiOutput("sdlog"), #rlnorm
            uiOutput("shape1"), #rlnorm
            uiOutput("shape2"), #rlnorm

            
            sliderInput("n", 
                        "Sample size:", 
                        value = 30,
                        min = 2, 
                        max = 500)
            
            ),
# Create Tabbed Panel ---------------------------------------------------------
        mainPanel(
            
            tabsetPanel(type = "tabs",
                tabPanel("Population Distribution", plotOutput("population.dist"),
                br()),
                tabPanel("Distributions of Samples", plotOutput("sample.dist"),
                    div(h3(textOutput("num.samples")), align = "center"),
                    br()),
                tabPanel("Distribution of Sample Means", plotOutput("sampling.dist"),
                    div(textOutput("sampling.descr"), align = "center"),
                    br()),
                tabPanel("Sample Means QQ Plot", plotOutput("QQPlot"))

            )
        )    
    ),
    div(tableOutput("summaryTableOutput"), align = "center")
    
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

    output$shape1 = renderUI(
        {
            req(input$dist)
            if (input$dist == "rbeta")
            {
                numericInput("shape1", 
                             "Shape 1",
                             min = 0,
                             value = 5)
            }
        })
    
    output$shape2 = renderUI(
        {
            req(input$dist)
            if (input$dist == "rbeta")
            {
                numericInput("shape2", 
                             "Shape 2",
                             min = 0,
                             value = 1)
            }
        })    
  
    output$meanlog = renderUI(
        {
            req(input$dist)
            if (input$dist == "rlnorm")
            {
                numericInput("meanlog", 
                             "Mean log",
                             value = 0)
            }
        })
    
    output$sdlog = renderUI(
        {
            req(input$dist)
            if (input$dist == "rlnorm")
            {
                numericInput("sdlog", 
                             "SD log",
                             value = 1,
                             min = 0.1)
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
    
    output$df = renderUI(
        {
            req(input$dist)
            if (input$dist == "rt" || input$dist == "rchisq")
            {
                numericInput("df", 
                             "Degrees of Freedom",
                             min = 0,
                             value = 10)
          }
      })
    
    output$ncp = renderUI(
        {
            req(input$dist)
            if (input$dist == "rt" || input$dist == "rchisq" || input$dist == 'rbeta')
            {
                numericInput("ncp", 
                             "Non-Centrality Parameter",
                             min = 0,
                             value = 0)
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


# rand_draw function -----------------------------------------------------------
    
    # rand_draw creates a population distribution 
    # Output: returns a population vector based on the user selected input distribution function
    
    rand_draw = function(dist, n, mu, sd, min, max, skew, rate, size, probability, lambda, df, ncp) 
    {
        vals = NULL
        if (dist == "rbeta"){
            shape1 = input$shape1 ; shape2 = input$shape2; ncp = input$ncp
            vals = do.call(dist, list(n=n, shape1=shape1, shape2=shape2, ncp=ncp))
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
            meanlog = input$meanlog; sdlog = input$sdlog
            vals = do.call(dist, list(n=n, meanlog=meanlog, sdlog=sdlog))
        }
        else if (dist == "runif"){
          vals = do.call(dist, list(n=n, min=min, max=max))
        }
        else if (dist == "rpois"){
            vals = do.call(dist, list(n=n, lambda = lambda))    
        }
        else if (dist == "rbinom"){
            vals = do.call(dist, list(n=n, size = size, prob = probability))    
        }
        else if (dist == "rt"){
            vals = do.call(dist, list(n=n, df=df, ncp=ncp))    
        }
        else if (dist == "rchisq"){
            vals = do.call(dist, list(n=n, df=df, ncp=ncp))    
        }
        return(vals)
    }

# Create population and sample data ------------------------------------------   
    
    # the repeatable function always uses the same seed when called
    rep_rand_draw = repeatable(rand_draw)  
  
    # parent returns the same population distribution each time 
    # it is called depending on the called functions and inputs.
    parent = reactive({
        req(input$dist, input$mu, input$sd)
        n = 1e5
        return(rep_rand_draw(
                        input$dist, 
                        n, 
                        input$mu, 
                        input$sd, 
                        input$min, 
                        input$max, 
                        input$skew, 
                        input$rate, 
                        input$size, 
                        input$probability, 
                        input$lambda, 
                        input$df, 
                        input$ncp
                        )
              )
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
    
# Create Table With Summary Data -------------------------------------------    
    summaryTable <- reactive({
        req(parent(), samples(), input$n)
        population = parent()
        m_population =  round(mean(population),3)
        sd_population = round(sd(population),3)
        ndist = colMeans(samples())
        sd_ndist = round(sd(ndist),3)
        m_sample = round(mean(ndist),3)

        return(
            data.frame(
                Mean = c(m_population, m_sample),
                SD = c(sd_population,sd_ndist), 
                row.names = c("Population", "Sample Means")

                
            )
        )
    })

# Plot: Population Distribution ------------------------------------------------

    output$population.dist = renderPlot({
    
        distname = switch(input$dist,
                        rnorm = "Population distribution: Normal",
                        rexp = "Population distribution: Exponential",
                        runif = "Population distribution: Uniform",
                        rpois = "Population distribution: Poisson",
                        rbinom = "Population distribution: Binomial",
                        rlnorm = "Population distribution: Log Normal",
                        rbeta = "Population distribution: Beta",
                        rt = "Population distribution: Student T",
                        rchisq = "Population distribution: Chi Square"
        )
    

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
        } else {
            p <- ggplot(populationDataTable, aes(V1, stat(density))) 
            p + geom_histogram(bins = 200) + geom_vline(xintercept = m_population) + ylab("Density") + xlab("")
        }
    })
  
  
# Plot: Distributions of Samples-------------------------------------------
  
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
  
# Render Text "Distribution of Samples"-------------------------------------
  
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
                            rexp = "exponential population",
                            runif = "uniform population",
                            rpois = "poisson population",
                            rbinom = "binomial population",
                            rlnorm  = "log normal population",
                            rbeta = "beta population",
                            rt = "student's t population",
                            rchisq = "chi-squared population")   
          
          
            n = input$n
            k = 10000
          
            population = parent()
          
            m_population =  mean(population)
            sd_population = round(sd(population),2)
          
            ndist = colMeans(samples())
            ndistDataTable = data.table(sampleMeans = colMeans(samples()))
          
          
            m_samp =  round(mean(ndist),2)
            sd_samp = round(sd(ndist),2)
          
            ndens=density(ndist)
            nhist=hist(ndist, plot=FALSE)
          

            p <- ggplot(ndistDataTable, aes(sampleMeans, stat(density))) 
            p + geom_histogram(bins = 100) + geom_vline(xintercept = m_population) + ylab("Density") + xlab("")
          
        }
    })
  
# Distribution of Sample Means Text -----------------------------------------------
    output$sampling.descr = renderText({
      
        distname = switch(input$dist,
                        rnorm = "normal population",
                        rexp = "exponential population",
                        runif = "uniform population",
                        rpois = "poisson population",
                        rbinom = "binomial population",
                        rlnorm  = "log normal population",
                        rbeta = "beta population",
                        rt = "student's t population",
                        rchisq = "chi-squared population")  
      
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

# Render QQPlot
    
    output$QQPlot <- renderPlot({
        
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
                              rexp = "exponential population",
                              runif = "uniform population",
                              rpois = "poisson population",
                              rbinom = "binomial population",
                              rlnorm  = "log normal population",
                              rbeta = "beta population",
                              rt = "student's t population",
                              rchisq = "chi-squared population")   
            
            
            n = input$n
            k = 10000
            
            ndist = colMeans(samples())
            ndist <- data.frame(y=ndist)
        
            p <- ggplot(ndist, aes(sample = y))
            p + stat_qq() + stat_qq_line()
        
        }
    })
    
    
# Render Table With Summary Data ----------------------------------------
    
    output$summaryTableOutput <- renderTable(
        {
            summaryTable()
        },
        digits = 3,
        align = 'c',
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        rownames = TRUE
    )
}
# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
