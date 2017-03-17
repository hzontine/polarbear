library(shiny)
library(igraph)

source("../opinionDynamics.R")

shinyServer(function(input, output, session) {
  
  sim.started <- FALSE
  progress <- NULL

  get.graphs <- reactive({

    isolate({
      progress <<- Progress$new(session,min=0,max=input$numEncounters+1)

      if(input$seedType=="specific") {
        set.seed(input$seed)
      } else {
        the.seed <- sample(1:10000,1)
        set.seed(the.seed)
        updateNumericInput(session, "seed", value=the.seed)
      }

      init <<- get.expressed.latent.graph(num.agents=input$numAgents, prob.connected=input$probConnected, dir=FALSE)
      return(sim.opinion.dynamics(init, num.encounters=input$numEncounters,
                              encounter.func=list(
                                  get.mean.field.encounter.func(1),
                                  get.graph.neighbors.encounter.func(1)),
                              victim.update.function=list(
                                  get.automatically.update.victim.function(A.is.victim=input$victim,
                                      prob.update=input$probUpdate, opinion.type="hidden"),
                                  get.peer.pressure.update.function(A.is.victim=input$victim,
                                      prob.knuckle.under.pressure=input$probPressure,
                                      prob.internalize.expressed.opinion=input$probInternalize, trumpEffect=input$hte)),
                                  generate.graph.per.encounter=input$generate,
                                  termination.function=get.unanimity.termination.function(input$terminate),
                                  choose.randomly.each.encounter=input$chooseRandomly, progress=progress))
    
      #plot.animation(graphs,attribute.name="hidden", second.attribute="expressed", delay.between.frames=.5)
      #plot.polarization(graphs)
    })
  })

  output$binaryPlot <- renderPlot({
    if (input$runsim < 1) return(NULL)
    isolate({
      plot.binary.opinions(get.graphs(), attribute1 = "hidden", attribute2 = "expressed")
    })
  })
  
  output$polarPlot <- renderPlot({
    if (input$runsim < 1) return(NULL)
    cat("Drawing polarization\n")
    isolate({
      plot.polarization(get.graphs())
    })
  })
  
  
  output$biasPlot <- renderPlot({
    if (input$runsim < 1) return(NULL)
    cat("Drawing bias plot\n")
    isolate({
      plot.bias(get.graphs())
    })
  })
  
  output$genuinePlot <- renderPlot({
    if (input$runsim < 1) return(NULL)
    cat("Drawing % genuineness\n")
    isolate({
      plot.genuine(get.graphs())
    })
  })
  
  
  output$hiddenPlot <- renderPlot({
    if (input$runsim < 1) return(NULL)
    cat("Drawing hidden true believers\n")
    isolate({
      plot.hidden(get.graphs())
    })
  })
  
  output$expPlot <- renderPlot({
    if (input$runsim < 1) return(NULL)
    cat("Drawing expressed true believers\n")
    isolate({
      plot.expressed(get.graphs())
    })
  })
  
  output$effectiveHiddenPlot <- renderPlot({
    if (input$runsim < 1) return(NULL)
    cat("Drawing hidden effective encounters plot\n")
    isolate({
      plot.hidden.effective.encounters(get.graphs())
    })
  })
  
  
  output$effectiveExpPlot <- renderPlot({
    if (input$runsim < 1) return(NULL)
    cat("Drawing expressed effective encounters plot\n")
    isolate({
      plot.exp.effective.encounters(get.graphs())
    })
  })
  
  
})
