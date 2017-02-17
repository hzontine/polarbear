library(shiny)
library(igraph)

source("opinionDynamics.R")

shinyServer(function(input, output, session) {
  
  sim.started <- FALSE
  progress <- NULL

  output$binaryPlot <- renderPlot({
    if (input$runsim < 1) return(NULL)

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
      graphs <<- sim.opinion.dynamics(init, num.encounters=input$numEncounters,
                              encounter.func=list(
                                  get.mean.field.encounter.func(1),
                                  get.graph.neighbors.encounter.func(1)),
                              victim.update.function=list(
                                  get.automatically.update.victim.function(A.is.victim=input$victim,
                                      prob.update=input$probUpdate, opinion.type="hidden"),
                                  get.peer.pressure.update.function(A.is.victim=input$victim,
                                      prob.knuckle.under.pressure=input$probPressure,
                                      prob.internalize.expressed.opinion=input$probInternalize, trumpEffect=input$hte)),
                                  generate.graph.per.encounter=TRUE,
                                  termination.function=get.unanimity.termination.function(input$terminate),
                                  choose.randomly.each.encounter=input$chooseRandomly, progress=progress)
    
      #plot.animation(graphs,attribute.name="hidden", second.attribute="expressed", delay.between.frames=.5)
      #plot.polarization(graphs)
      plot.binary.opinions(graphs, attribute1 = "hidden", attribute2 = "expressed")
    })
  })
  
  output$polarPlot <- renderPlot({
    if (input$runsim < 1) return(NULL)
    cat("Redrawing polarization\n")
    isolate({
      plot.polarization(graphs)
    })
  })
  
  
  output$biasPlot <- renderPlot({
    if (input$runsim < 1) return(NULL)
    cat("Redrawing bias\n")
    isolate({
      plot.bias(graphs)
    })
  })
  
  
})
