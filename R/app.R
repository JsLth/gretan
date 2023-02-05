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
  bs4dash_layout(main_bg = "#FDFDFD", sidebar_width = "350px"),
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

home_tab <- tabItem("home",
  box("Test text")
)

hotspot_tab <- tabItem("hotspot",
  box(
    "Box content here", br(), "More box content",
    sliderInput("slider", "Slider input:", 1, 100, 50),
    textInput("text", "Text input:")
  )
)

vl_tab <- tabItem("lotkavolterra",
  box(
    "Box content here", br(), "More box content",
    sliderInput("slider", "Slider input:", 1, 100, 50),
    textInput("text", "Text input:")
  )
)

cluster_tab <- tabItem("cluster",
  box(
    "Box content here", br(), "More box content",
    sliderInput("slider", "Slider input:", 1, 100, 50),
    textInput("text", "Text input:")
  )
)

reg_tab <- tabItem("regression",
  box(
    "Box content here", br(), "More box content",
    sliderInput("slider", "Slider input:", 1, 100, 50),
    textInput("text", "Text input:")
  )
)

network_tab <- tabItem("network",
  box(
    "Box content here", br(), "More box content",
    sliderInput("slider", "Slider input:", 1, 100, 50),
    textInput("text", "Text input:")
  )
)

ui <- dashboardPage(
  dashboardHeader(
    tags$div(
      a(
        href = "https://projectgreta.eu/",
        tags$img(src = "https://projectgreta.eu/wp-content/uploads/2021/08/GRETA_Logo_Acronym_RGB-1-1.svg", height = "35px"),
        style = "padding-top:10px; padding-bottom:10px;"
      ), class = "dropdown"
    ),
    title = "GIS tool",
    status = "secondary",
    skin = "light",
    sidebarIcon = fontawesome::fa("bars", fill = "#000000")
  ),
  dashboardSidebar(
    sidebarMenu(
      sidebarHeader("Navigation"),
      sidebarSearchForm("textSearch", "buttonSearch", icon = icon("magnifying-glass")),
      menuItem(
        text = "Home",
        icon = icon("house", lib = "font-awesome"),
        tabName = "home"
      ),
      menuItem(
        text = "Hotspot analysis",
        icon = icon("map-pin", lib = "font-awesome"),
        tabName = "hotspot"
      ),
      menuItem(
        text = "Lotka-Volterra analysis",
        icon = icon("users", lib = "font-awesome"),
        tabName = "lotkavolterra"
      ),
      menuItem(
        text = "Cluster analysis",
        icon = icon("arrows-to-circle", lib = "font-awesome"),
        tabName = "cluster"
      ),
      menuItem(
        text = "Regression analysis",
        icon = icon("chart-line", lib = "font-awesome"),
        tabName = "regression"
      ),
      menuItem(
        text = "Network analysis",
        icon = icon("circle-nodes", lib = "font-awesome"),
        tabName = "network"
      ),
      flat = TRUE,
      id = "sidebar"
    ),
    skin = "light",
    minified = TRUE,
    collapsed = FALSE
  ),
  dashboardBody(
    tabItems(
      home_tab,
      hotspot_tab,
      vl_tab,
      cluster_tab,
      reg_tab,
      network_tab
    )
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
