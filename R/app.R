library(shiny)
library(shinydashboard)
library(fresh)
library(bs4Dash)
library(shinyjs)
library(waiter)
library(leaflet)
library(dplyr)
library(purrr)

srv <- survey_local[cb_ext[cb_ext$topic %in% "Demographics", ]$variable]

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

categories <- unique(na.omit(cb_ext$topic))
titles <- categories %>%
  map(~filter(cb_ext, topic == .x) %>% pull(title) %>% unique()) %>%
  set_names(categories)

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
  bs4dash_font(family_sans_serif = "Arial")
)

preloader <- list(html = tagList(spin_6(), "Loading ..."), color = "#B3DDFE")

home_tab <- tabItem("home",
  box("Test text")
)

explorer_tab <- tabItem("explorer",
  class = "outer",
  tags$head(includeCSS("../styles.css")),
  fluidRow(
    bs4Dash::column(
      width = 3,
      bs4Dash::box(
        title = "Data selection", 
        id = "exp_databox",
        width = 12,
        solidHeader = FALSE, 
        collapsible = TRUE,
        selectInput("title", "Topic", titles, "id"),
        htmlOutput("question"),
        tags$br(),
        shinyjs::hidden(
          div(id = "subitem_hide",
            selectInput("subitem", "Subitem", character())
          )
        ),
        shinyjs::hidden(
          div(id = "option_hide",
              selectInput("option", "Option", character())
          )
        ),
        selectInput("scale", "Geographic Scale", c("NUTS-0", "NUTS-1", "NUTS-2")),
        selectInput("pal", "Color palette", all_pals)
      )
    ),
    column(
      width = 9,
      box(
        id = "exp_mapbox",
        width = 12,
        collapsible = FALSE,
        solidHeader = FALSE,
        headerBorder = FALSE,
        leafletOutput("explorer", width = "100%", height = 800)
      )
    )
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

cs5_tab <- tabItem("cs4",
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
        menuSubItem(text = "Pilastro-Roveri, Italy", tabName = "cs1"),
        menuSubItem(text = "Coopernico, Portugal", tabName = "cs2"),
        menuSubItem(text = "The Earnest App, Germany", tabName = "cs3"),
        menuSubItem(text = "Gas-free neighborhoods, Netherlands", tabName = "cs4"),
        menuSubItem(text = "UR Beroa, Spain", tabName = "cs5")
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
    useShinyjs(),
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
      cs4_tab,
      cs5_tab
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
    if (!is.null(input$title)) {
      indat <- cb_ext[cb_ext$title %in% input$title, ]

      if (!all(is.na(indat$subitem))) {
        indat <- indat[indat$subitem %in% input$subitem, ]
      }

      if (!all(is.na(indat$option))) {
        indat <- indat[indat$option %in% input$option, ]
      }

      HTML(paste0(
        "<b>Question ", toupper(indat$og_var), ":</b><br>", indat$label
      ))
    } else {
      ""
    }
  })
  
  observeEvent(input$title, {
    invar <- cb_ext[cb_ext$title %in% input$title, ]$variable
    items <- cb_ext[cb_ext$variable %in% invar, ]$subitem
    options <- cb_ext[cb_ext$variable %in% invar, ]$option
    show_subitems <- length(invar) > 1 & !all(is.na(items))
    show_options <- length(invar) > 1 & !all(is.na(options))
    
    if (show_subitems) {
      shinyjs::show("subitem_hide", anim = TRUE)
      updateSelectInput(inputId = "subitem", choices = items)
    } else if (show_options) {
      shinyjs::show("option_hide", anim = TRUE)
      updateSelectInput(inputId = "option", choices = options)
    } else {
      shinyjs::hide("subitem_hide", anim = TRUE)
      shinyjs::hide("option_hide", anim = TRUE)
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
    pal <- leaflet::colorNumeric(pal, NULL, n = 5)

    has_title <- cb_ext$title %in% input$title
    invar <- cb_ext[has_title, ]$variable
    
    if (length(invar) > 1) {
      has_subitem <- cb_ext$subitem %in% input$subitem
      invar <- cb_ext[has_title & has_subitem, ]$variable
    }
    
    if (length(invar) > 1 || !length(invar)) {
      has_option <- cb_ext$option %in% input$option
      invar <- cb_ext[has_title & has_option, ]$variable
    }
    
    tryCatch({
      leaflet(sf::st_transform(poly[invar], 4326)) %>%
        addTiles() %>%
        setView(lng = 9, lat = 55, zoom = 4) %>%
        addPolygons(
          fillColor = as.formula(paste0("~pal(", invar, ")")),
          fillOpacity = 0.7,
          weight = 1,
          color = "black",
          opacity = 0.5,
          popup = htmltools::htmlEscape(poly[[invar]])
        ) %>%
        addLegend(
          position = "bottomright",
          na.label = "No data",
          pal = pal,
          values = as.formula(paste0("~", invar)),
          opacity = 0.9,
          title = "Mean age"#,
          #labFormat = labelFormat(suffix = " years")
        )
    }, error = \(e) NULL)
  })
}

shinyApp(ui, server)
