library(shiny)
library(igraph)

source("opinionDynamics.R")

shinyServer(function(input, output) {
  
  output$binaryPlot <- renderPlot({
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
                                      prob.internalize.expressed.opinion=input$probInternalize)),
                                  generate.graph.per.encounter=TRUE,
                                  termination.function=get.unanimity.termination.function(input$terminate),
                                  choose.randomly.each.encounter=input$chooseRandomly)
    
      #plot.animation(graphs,attribute.name="hidden", second.attribute="expressed", delay.between.frames=.5)
      #plot.polarization(graphs)
      plot.binary.opinions(graphs, attribute1 = "hidden", attribute2 = "expressed")
  })
  
  output$polarPlot <- renderPlot({
      plot.polarization(graphs)
  })
  
})