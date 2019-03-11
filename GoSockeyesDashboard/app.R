#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(geojsonio)

source("map.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "GoSockeyes Dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                infoBoxOutput("progressBox"),
                infoBox("App Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon")),
                infoBox("New Installs", "2,500", icon = icon("mobile")),
                box(
                  title = "Last 10 Minutes Logged-On", 
                  collapsible = TRUE,
                  solidHeader = TRUE, status = "info",
                  plotOutput("activityPlot", height = 250)
                  ),
                box(
                  title = "Player Suvey", 
                  collapsible = TRUE,
                  solidHeader = TRUE, status = "info",
                  plotOutput("surveyPlot", height = 250)
                ),
                box(
                  title = "Activity by Geo",
                  collapsible = TRUE,
                  solidHeader = TRUE, status = "info",
                  leafletOutput("mymap")
                ),
                box(
                  title = "Trending Topics",
                  collapsible = TRUE,
                  solidHeader = TRUE, status = "info",
                  tableOutput("trendingTopics")
                )
              )
      ),

      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )    
    
  ))
)

next_change <- function() {
  return(sample(-850:900, 1))
}
  
init_logged_users <- function() {
  logged_users_first <- c(1:60)
  logged_users_first[1] <- 50000
  for (i in 2:60) {
    logged_users_first[i] = logged_users_first[i-1] + next_change()
  }
  
  logged_users_history <- as.data.frame(cbind(1:60,logged_users_first))
  colnames(logged_users_history) <- c("x","y")
  
  return(logged_users_history)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  logged_users_history <- init_logged_users()
  
  data <- reactivePoll(1000, session,
       checkFunc = function() {
         Sys.time()
       },
       valueFunc = function() {
         last_row <- nrow(logged_users_history)
         next_val <<- logged_users_history[last_row,"y"] + next_change()
         logged_users_history$x = logged_users_history$x - 1 
         logged_users_history <<- rbind(logged_users_history[2:last_row,], c(last_row, next_val))
         logged_users_history
       }
  )
  
  
    output$active_users_placeholder <- renderText({
      df <- data()
      updateTextInput(session, "active_users", value = df[nrow(df), "y"])
      ""
    })
    
    output$surveyPlot <- renderPlot({
      df <- as.data.frame(
        rbind(
          cbind("Sammy Bonez", 30),
          cbind("Nacho Cheese", 60),
          cbind("Russel McRussel", -35),
          cbind("Chuck eCheese", -20),
          cbind("Vanilla Ice", 80)
        )
      )
      colnames(df) <- c("player", "vote")
      df$vote <- as.numeric(levels(df$vote))
      df$like <- ifelse(df$vote > 0, "Like", "Dislike")
      
      ggplot() + 
        geom_bar(data = df, width=.5, aes(x=player, y=vote, fill=like), stat = "identity") + 
        coord_flip() + guides(fill=FALSE) + xlab("")
    })
    
    output$activityPlot <- renderPlot({
    df <- data()
    ggplot(df, aes(x=x, y=y)) +
      geom_line(colour="#3c8dbc", size=1.75) + 
      ylim(40000, 70000) +
      xlab("") + ylab("") +
      ggtitle("Logged Users")
    })
    
    output$mymap <- renderLeaflet({
      make_map()
    })
    
    output$progressBox <- renderInfoBox({
      df <- data()
      infoBox(
        "Logged In:", prettyNum(df[nrow(df),"y"], big.mark = ","), icon = icon("users"),
        color = "purple"
      )
    })
    
    output$trendingTopics <- renderTable(
      c("Stop serving salmon in Seattle Resturant #fishismurder",
        "We are loving the new roster #gosockeyes",
        "This App Rocks!",
        "This App Sucks!",
        "Tired of making bogus topics #fakenews"), 
      colnames = FALSE, rownames = FALSE, bordered = TRUE
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

