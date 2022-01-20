library(shiny)

shinyUI(fluidPage(
  titlePanel(""),
  sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput(
                    "leto1",
                    label = "Leto:",
                    choices = c(2003, 2005, 2007, 2010, 2013, 2016),
                    selected = 2010
                  ),
                  selectInput(
                    "vrsta.zemljisca1",
                    label = "Vrsta zemljišča:",
                    choices = c("gozd", "nerodovitno", "njive", "travniki in pašniki", "žita"),
                    selected = "gozd"
                  ))
                ,
                mainPanel(plotOutput("graf"))),
  uiOutput("izborTabPanel")))
