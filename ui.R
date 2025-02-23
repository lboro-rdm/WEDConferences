library(shiny)
library(DT)
library(lubridate)
library(shinycssloaders)
library(httr)
library(jsonlite)
library(dplyr)

ui <- tags$html(
  lang = "en",
  fluidPage(
    titlePanel(
      HTML('<span style="color: #002c3d;"><strong>WEDC, Loughborough University:</strong></span>
          <span style="color: #009BC9;">Conferences</span><br><br>')
    ),
    tags$head(
      tags$style(HTML("
        body {
          background-color: #FFFFFF;
          font-size: 16px;
        }
        h2, a {
          color: #6F3092;
        }
        a.hover-underline:hover {
          text-decoration: underline;
        }
      "))
    ),
    sidebarLayout(
      sidebarPanel(
        style = "margin-top: 20px;",
        selectInput(
          inputId = "collectionSelect",
          label = "Select a Conference:",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        textInput(
          inputId = "authorSearch",
          label = "Search by Author:",
          placeholder = "Enter author's name"
        ),
        textInput(
          inputId = "titleSearch",
          label = "Search by Title:",
          placeholder = "Enter book or manual title"
        )
      ),
      mainPanel(
        fluidRow(
          style = "margin-left: 20px; margin-right: 20px;",
          withSpinner(
            uiOutput("bookDetails"),
            type = 3,
            color = "#009BC9",
            color.background = "#FFFFFF"
          )
        )
      )
    )
  )
)