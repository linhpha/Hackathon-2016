library(shiny)
require(visNetwork)
require(igraph)
library(reshape2)
library(plotly)

### Prepare Data Input
# data: binding2, attributes (prop.attributes), categoryProp
attributes1 <- read.csv("data/attributes.csv", stringsAsFactors = F)
attributes <- attributes[-1]
binding2 <- read.csv("data/binding2.csv", stringsAsFactors = F)
binding2 <- binding2[-1]
country <- colnames(attributes)[3:length(attributes)]
category.Prop <- read.csv("data/category.Prop.csv", stringsAsFactors = F)
category.Prop <- category.Prop[-1]
Num <- read.csv("data/Number-of-recipes.csv", stringsAsFactors=FALSE)
Num <- Num[, 2:3]

shinyUI(
  fluidPage(

  # Application title
  titlePanel("Food Corner"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("This app visualizes how each cuisine combines ingredients."),
      helpText("Our dataset includes 50K recipes all over the world "),
      selectizeInput('cuisine', 
                     label = "Choose one cuisine of your preference:",
                     choices = country,
                     multiple = FALSE)
    )
  ,

    # Show a plot of the generated distribution
    mainPanel(
      fluidPage(
        visNetworkOutput("network", height = "400px"),
        helpText("Average Distribution of Food Categories in 1 Recipe of This Cuisine"),
        textOutput("text1"),
        plotlyOutput("category", height = "300px")
    )
  )
)))
