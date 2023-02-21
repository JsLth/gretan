library(shiny)
library(shinydashboard)
library(fresh)
library(bs4Dash)
library(waiter)
library(leaflet)
library(dplyr)

srv <- survey_local

all_pals <- RColorBrewer::brewer.pal.info %>%
  filter(category == "seq") %>%
  row.names()
all_pals <- list(
  "Common palettes" = as.list(all_pals),
  "Colorblind palettes" = list(
    "Magma", "Inferno", "Plasma", "Viridis",
    "Cividis", "Rocket", "Mako", "Turbo"
  )
)

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

preloader <- list(html = tagList(spin_6(), "Loading ..."), color = "#B3DDFE")

home_tab <- tabItem("home",
  box("Test text")
)

explorer_tab <- tabItem("explorer",
  class = "outer",
  tags$head(includeCSS("../styles.css")),
  leafletOutput("explorer", width = "100%", height = "100%"),
  absolutePanel(
    id = "controls",
    draggable = TRUE, fixed = TRUE, class = "panel panel-default",
    top = 80, left = "auto", right = 40, bottom = "auto",
    width = 330, height = "auto",
    
    h2("Data explorer"),
    selectInput("survey_col", "Topic", setdiff(names(srv), "id")),
    htmlOutput("question"),
    selectInput("scale", "Geographic Scale", c("NUTS-0", "NUTS-1", "NUTS-2")),
    selectInput("pal", "Color palette", all_pals)
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

cs1_tab <- tabItem("cs1",
  box(
   "Box content here", br(), "More box content",
   sliderInput("slider", "Slider input:", 1, 100, 50),
   textInput("text", "Text input:")
  )
)

cs2_tab <- tabItem("cs2",
  box(
   "Box content here", br(), "More box content",
   sliderInput("slider", "Slider input:", 1, 100, 50),
   textInput("text", "Text input:")
  )
)

cs3_tab <- tabItem("cs3",
  box(
   "Box content here", br(), "More box content",
   sliderInput("slider", "Slider input:", 1, 100, 50),
   textInput("text", "Text input:")
  )
)

cs4_tab <- tabItem("cs4",
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
        text = "Data explorer",
        icon = icon("map", lib = "font-awesome"),
        tabName = "explorer"
      ),
      menuItem(
        text = "Case studies",
        icon = icon("map-pin", lib = "font-awesome"),
        menuSubItem(text = "Coopernico, Portugal", tabName = "cs1"),
        menuSubItem(text = "Pilastro-Roveri, Italy", tabName = "cs2"),
        menuSubItem(text = "UR Beroa, Spain", tabName = "cs3"),
        menuSubItem(text = "Gas-free neighborhoods, Netherlands", tabName = "cs4")
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
      explorer_tab,
      vl_tab,
      cluster_tab,
      reg_tab,
      network_tab,
      cs1_tab,
      cs2_tab,
      cs3_tab,
      cs4_tab
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
  
  output$question <- renderUI({
    if (!is.null(input$survey_col)) {
      HTML(paste0(
        "<b>Question</b> ",
        codebook[codebook$variable == input$survey_col, ]$variable,
        ": ", codebook[codebook$variable == input$survey_col, ]$label
      ))
    } else {
      ""
    }
  })
  
  output$explorer <- renderLeaflet({
    poly <- switch(input$scale,
                   "NUTS-0" = survey_nuts0,
                   "NUTS-1" = survey_nuts1,
                   "NUTS-2" = survey_nuts2
    )
    
    if (input$pal %in% all_pals[["Colorblind palettes"]]) {
      pal <- viridis::viridis_pal(option = tolower(input$pal))(5)
    } else {
      pal <- input$pal
    }
    pal <- leaflet::colorQuantile(pal, NULL, n = 5)
    print(pal)
    leaflet() %>%
      addTiles() %>%
      setView(lng = 6, lat = 52, zoom = 4) %>%
      addPolygons(
        data = sf::st_transform(poly[input$survey_col], 4326),
        fillColor = as.formula(paste0("~pal(", input$survey_col, ")")),
        fillOpacity = 0.7,
        weight = 1,
        color = "black",
        opacity = 0.5,
        popup = htmltools::htmlEscape(poly[[input$survey_col]])
      ) %>%
      addLegend(
        position = "bottomleft",
        na.label = "No data",
        pal = pal,
        values = poly[[input$survey_col]]
      )
  })
}

shinyApp(ui, server)
