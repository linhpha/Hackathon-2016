library(shiny)
require(visNetwork)
require(igraph)
library(reshape2)
library(plotly)

### Prepare Data Input
# data: binding2, attributes (prop.attributes), categoryProp
attributes <- read.csv("data/attributes.csv", stringsAsFactors = F)
attributes <- attributes[-1]
binding2 <- read.csv("data/binding2.csv", stringsAsFactors = F)
binding2 <- binding2[-1]
country <- colnames(attributes)[3:length(attributes)]
category.Prop <- read.csv("data/category.Prop.csv", stringsAsFactors = F)
category.Prop <- category.Prop[-1]
Num <- read.csv("data/Number-of-recipes.csv", stringsAsFactors=FALSE)
Num <- Num[, 2:3]

shinyServer(function(input, output) {
  
    #Subseting data in response to input
    dataInput <- reactive({
      subset(binding2, is.na(binding2[,c(input$cuisine)]) == 0)[,c("ingredient.x","ingredient.y",input$cuisine)]
    })
    output$network <- renderVisNetwork({
    #Create nodes with id, label, and group
      nodes <- data.frame(id=attributes$Ingredients,
                        label=attributes$Ingredients,
                        group = attributes$Category,
                        size = attributes[,c(input$cuisine)]*250,
                        stringsAsFactors = F)
      nodes <- nodes[nodes$size >= quantile(nodes$size,.5),]
    #Create links with from, to, foo
      links <- dataInput() 
      colnames(links) <- c("from","to","foo")
      links <- links[links$foo >= quantile(links$foo,.90),] #only use 75% of the edges
      links$value <- links$foo*100
      nodes$label.cex <- (nodes$size >= quantile(nodes$size,.70))*(nodes$size)^2
      visNetwork(nodes, links, directed = F) %>%
        visInteraction(dragNodes = TRUE,
                       dragView = TRUE,
                       zoomView = TRUE,
                       hideEdgesOnDrag = F) %>% 
        visPhysics(solver = "repulsion", 
                   repulsion = list(centralGravity = 0.1, springConstant =.01),
                                           
                   stabilization = list(enabled = T, 
                                        iterations = 50)) %>%
        visOptions(
          highlightNearest = list(enabled = T,
                                  degree = 2),
          selectedBy = "group"
        ) %>%
        visLayout(randomSeed = 123,
                  improvedLayout = T) %>%
        visNodes(shape = "dot", 
                 color = list(hover = "red"),
                 scaling = list(label = list(enabled = T)),
                 font = list(size = 30)) %>%
        visEdges(
          color = list(highlight = "red"),
          smooth = list(type = "curvedCCW")) %>%
        
        visGroups(groupname = unique(category.Prop$Ingredients)[1], color = "6A3D9A") %>%
        visGroups(groupname = unique(category.Prop$Ingredients)[2], color = "CAB2D6") %>%
        visGroups(groupname = unique(category.Prop$Ingredients)[3], color = "A6CEE3") %>%
        visGroups(groupname = unique(category.Prop$Ingredients)[4], color =  "FF7F00") %>%
        visGroups(groupname = unique(category.Prop$Ingredients)[5], color = "FDBF6F") %>%
        visGroups(groupname = unique(category.Prop$Ingredients)[6], color = "E31A1C") %>%
        visGroups(groupname = unique(category.Prop$Ingredients)[7], color = "FB9A99") %>%
        visGroups(groupname = unique(category.Prop$Ingredients)[8], color = "33A02C") %>%
        visGroups(groupname = unique(category.Prop$Ingredients)[9], color = "B2DF8A") %>%
        
        visEvents(
            type = "once", 
            startStabilizing = "function() {
                this.moveTo({scale:0.2})}")
      })
    
    # output$category <- renderPlotly({
    #   plot_ly(x = category.Prop[,c(input$cuisine)], y = category.Prop[,1], yaxis = order(category.Prop[,1]),
    #           xaxis= c(0,.5,1,1.5,2,2.5,3,3.5,4,4.5,5), type = 'bar', orientation = 'h',
    #           marker = list(color = c("6A3D9A", "CAB2D6","A6CEE3","FF7F00","FDBF6F","E31A1C","FB9A99","33A02C","B2DF8A"),
    #                         width = 0.5)) %>%
    #     layout(margin = list(l = 80, r = 10, t = 50, b = 50)) %>%
    #     add_annotations(xref = 'x1', yref = 'y',
    #                     x = category.Prop[,c(input$cuisine)],  y = category.Prop[,1], 
    #                     text = paste("       " ,mapply(round, category.Prop[,c(input$cuisine)],2)),
    #                     font = list(family = 'Cambria', size = 12, color = 'rgb(50, 171, 96)'),
    #                     
    #                     showarrow = FALSE)
    # })
    {
      
      output$text1 <- renderText({ 
          paste(
                input$cuisine, " had", Num[which(Num[,1] == input$cuisine), 2], " recipes" )
      })
      
    }
      
}
)
