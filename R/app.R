library(shiny)
library(shinydashboard)
library(fresh)
library(bs4Dash)
library(waiter)

greta_theme <- create_theme(
  bs4dash_status(
    primary = "#FED22B",
    secondary = "#F4F4F2",
    info = "#5E81AC",
    danger = "#BF616A",
    dark = "#F4F4F2"
  ),
  bs4dash_layout(main_bg = "#FDFDFD"),
  bs4dash_sidebar_light(bg = "#F4F4F2", color = "#000000"),
  bs4dash_color(
    white = "#FDFDFD",
    gray_800 = "#000000",
    gray_900 = "#000000"
  ),
  bs4dash_font(family_sans_serif = "Literata")
)
"#B3DDFE"

preloader <- list(html = tagList(spin_6(), "Loading ..."), color = "#B3DDFE")

ui <- dashboardPage(
  dashboardHeader(
    tags$div(
      a(
        href = "https://projectgreta.eu/",
        tags$img(src = "https://projectgreta.eu/wp-content/uploads/2021/09/GRETA_Logo_Tekstilogo_RGB-1.svg", height = "30px"),
        style = "padding-top:10px; padding-bottom:10px;"
      ), class = "dropdown"
    ),
    title = "GIS tool",
    status = "secondary",
    skin = "light"
  ),
  dashboardSidebar(
    sidebarMenu(
      sidebarHeader("Navigation"),
      sidebarSearchForm("textSearch", "buttonSearch", icon = icon("magnifying-glass")),
      menuItem(
        text = "Home",
        icon = icon("house", lib = "font-awesome")
      ),
      menuItem(
        text = "Hotspot analysis",
        icon = icon("map-pin", lib = "font-awesome")
      ),
      menuItem(
        text = "Lotka-Volterra analysis",
        icon = icon("users", lib = "font-awesome")
      ),
      menuItem(
        text = "Cluster analysis",
        icon = icon("arrows-to-circle", lib = "font-awesome")
      ),
      menuItem(
        text = "Regression analysis",
        icon = icon("chart-line", lib = "font-awesome")
      ),
      menuItem(
        text = "Network analysis",
        icon = icon("circle-nodes", lib = "font-awesome")
      ),
      flat = TRUE
    ),
    skin = "light",
    width = "2em"
  ),
  dashboardBody(
    includeCSS("www/font-family.css"),
    actionButton("reload", "Reload")
  ),
  freshTheme = greta_theme,
  dark = NULL,
  preloader = preloader
)

server = function(input, output, session) {
  observeEvent(input$reload, {
    session$reload()
  })
}

shinyApp(ui, server)
