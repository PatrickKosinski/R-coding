library(shiny)
library(shinycssloaders)
library(dplyr)


shinyUI(fluidPage(
        titlePanel("Patrick Kosinski: Katz Back-Off Prediction Model"),

        sidebarLayout(
                sidebarPanel(
                        textInput("box1", "Please type in a sentence consisting of exactly two words. More than two words can be typed in but only the last 
                                  two words are considered into account:", value = "a beautiful"),
                        submitButton("Run"),
                        h3("Internally used string:"),
                        textOutput("pred2"),
                        # Include a slider "how many predictions to you want to see?"
                        h3("How many predictions do you want to see?"),
                        sliderInput("slider1", "", 1, 10, 3)
                ),
                mainPanel(
                        tabsetPanel(type = "tabs", 
                                    tabPanel("How to use it", br(), textOutput("out1")),
                                    tabPanel("Further details", br(), textOutput("out2")),
                                    tags$a(href="https://en.wikipedia.org/wiki/Katz%27s_back-off_model", 
                                           "For more details on the Katz Back-off algorithm, please click here.")
                        ),
                       
                )
        ),
        plotOutput("bar", height = 500) %>% withSpinner(color="#0dc5c1")
))