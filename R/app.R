library(shiny)
library(shinydashboard)
library(fresh)
library(bs4Dash)
library(shinyjs)
library(waiter)
library(leaflet)
library(leaflegend)
library(dplyr)
library(purrr)
library(htmltools)

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

cs_coords <- st_sf(
  cs = c("Italy", "Portugal", "Germany", "The Netherlands", "Spain"),
  geometry = st_sfc(
    st_point(c(11.399926, 44.507145)),
    st_point(c(-9.136693, 38.710479)),
    st_point(c(8.651177, 49.872775)),
    st_point(c(5.6343227, 52.2434979)),
    st_point(c(-1.994286, 43.300075)),
    crs = 4326
  )
)

nuts0 <- readRDS("../data/bounds/nuts0.rds")
bgn_1 <- readRDS("../data/bounds/bgn_1.rds")
bgn_2 <- readRDS("../data/bounds/bgn_2.rds")
bgn_3 <- readRDS("../data/bounds/bgn_3.rds")
don_1 <- readRDS("../data/bounds/don_1.rds")
don_2 <- readRDS("../data/bounds/don_2.rds")
don_3 <- readRDS("../data/bounds/don_3.rds")

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
  tags$head(includeCSS("../styles.css")),
  fluidRow(
    column(
      width = 6,
      box(
        title = "Welcome to the GRETA GIS tool",
        width = 12,
        status = "primary",
        p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
        p("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat."),
        p("Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi."),
        h2("Subtitle"),
        p("Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat."),
        p("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis."),
        p("At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, At accusam aliquyam diam diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et et invidunt justo labore Stet clita ea et gubergren, kasd magna no rebum. sanctus sea sed takimata ut vero voluptua. est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur")
      )
    ),
    column(
      width = 6,
      bs4Dash::box(
        title = "Geographical overview",
        width = 12,
        status = "primary",
        leafletOutput("csmaps", width = "100%", height = 450)
      ),
      bs4Dash::box(
        title = "Case study descriptions",
        width = 12,
        status = "primary",
        div(style = "overflow-y: scroll;overflow-x: scroll;", uiOutput("csdesc"))
      )
    )
  )
)



explorer_tab <- tabItem("explorer",
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
        status = "primary",
        selectInput("exp_title", "Topic", titles, "id"),
        htmlOutput("question"),
        tags$br(),
        shinyjs::hidden(
          div(id = "subitem_hide",
            selectInput("exp_subitem", "Subitem", character())
          )
        ),
        shinyjs::hidden(
          div(id = "option_hide",
            selectInput("exp_option", "Option", character())
          )
        )
      ),
      bs4Dash::box(
        title = "Map configuration",
        id = "exp_config",
        width = 12,
        solidHeader = FALSE,
        collapsible = TRUE,
        status = "primary",
        selectInput("scale", "Aggregation level", c("NUTS-0", "NUTS-1", "NUTS-2")),
        selectInput("pal", "Color palette", all_pals)
      ),
      div(
        actionButton("exp_refresh",
          label = "Refresh",
          icon = icon("refresh", lib = "font-awesome")
        ),
        align = "center",
        style = "margin-bottom: 15px;"
      ),
      bs4Dash::box(
        title = "Download",
        id = "exp_download",
        width = 12,
        solidHeader = FALSE,
        collapsible = TRUE,
        status = "primary",
        actionButton("download_button",
          "Download data",
          icon = icon("download", lib = "font-awesome")
        )
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
        status = "primary",
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
  tags$head(includeCSS("../styles.css")),
  fluidRow(
   column(
     width = 6,
     box(
       title = "Renewable energy district",
       width = 12,
       status = "primary",
       p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
       p("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat."),
       p("Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi."),
       h2("Subtitle"),
       p("Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat."),
       p("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis."),
       p("At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, At accusam aliquyam diam diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et et invidunt justo labore Stet clita ea et gubergren, kasd magna no rebum. sanctus sea sed takimata ut vero voluptua. est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur")
     )
   ),
   column(
     width = 6,
     bs4Dash::box(
       title = "Case study map",
       width = 12,
       status = "primary",
       fluidRow(column(width = 4,
         selectInput("cs1bounds",
           label = "Boundaries",
           choices = c("Quarters", "Zones", "Statistical areas")
         )),
         column(width = 4,
         radioButtons("cs1bg",
           label = "Base map",
           choices = c("OpenStreetMap", "Satellite"),
           selected = "OpenStreetMap"
         )),
         column(width = 4,
         sliderInput("cs1opacity",
           label = "Opacity",
           min = 0,
           max = 1,
           step = 0.1,
           value = 0.2
         ))
       ),
       leafletOutput("cs1map", width = "100%", height = 450)
     ),
     bs4Dash::box(
       title = "",
       width = 12,
       status = "primary",
       uiOutput("cs1desc")
     )
   )
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
    title = HTML(paste(
      img(src = "https://projectgreta.eu/wp-content/uploads/2021/09/salama.svg", align = "center", width = 50, height = 50),
      span("GIS tool", class = "brand-text font-weight-light", style = "font-size: 130%; valign: bottom")
    ), sep = "\n"),
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
  # Home ----
  # Plot geographical overview map
  output$csmaps <- renderLeaflet({
    leaflet(cs_coords) %>%
      addTiles() %>%
      setView(lng = 9, lat = 55, zoom = 3) %>%
      addMarkers(icon = makeIcon("https://www.svgrepo.com/download/352253/map-pin.svg", iconWidth = 25, iconHeight = 25)) %>%
      addPolylines(
        data = st_transform(nuts0, 4326),
        fillOpacity = 0,
        weight = 1,
        color = "red"
      ) %>%
      addLegendImage(
        images = makeSymbol("line", width = 7, color = "red"),
        labels = "Surveyed countries",
        orientation = "vertical",
        position = "bottomleft",
        width = 10,
        height = 10,
        labelStyle = "font-size: 12px; vertical-align: middle;"
      )
  })
  
  # Show case study description based on map clicks
  output$csdesc <- renderUI({
    click <- input$csmaps_marker_click
    target <- NULL
    if (!is.null(click)) {
      marker <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)
      target <- cs_coords[st_is_within_distance(
        cs_coords$geometry,
        marker,
        dist = 1,
        sparse = FALSE
      ), ]$cs
    }
    
    if (identical(target, "Italy")) {
      HTML(paste(
        h2("Renewable energy district Pilastro-Roveri"),
        p(HTML(paste(
          "The case study takes place in the Pilastro-Roveri district in",
          "the northeast of Bologna, where a financed Green Energy",
          "Community project (GECO) has been running since 2019 to",
          "support communities in the process of designing and creating",
          "a green energy community. Pilastro-Roveri is a",
          "socio-economically stratified district composed of two",
          "areas:"
        )), style = "margin-bottom: 0.5cm;"),
        p(HTML(paste(
          "<b>Pilastro “Rione”</b> (neighbourhood): a residential neighborhood",
          "with a long history of activism but also of socio-economic issues."
        )), style = "margin-bottom: 0.5cm;"),
        p(HTML(paste(
          "<b>Roveri</b> area: distinctly separated from Pilastro by a former",
          "railway terminal, is an industrial and productive neighborhood",
          "hosting a variety of companies in sectors such as packaging,",
          "mechanics, and electric vehicles. The area also hosts the",
          "Agriculture and Food Center of Bologna’s (CAAB) food and",
          "agriculture theme park (FICO) with its industrial partners, which",
          "has the largest solar power plant on industrial roofs within the EU."
        )), style = "margin-bottom: 0.5cm;")
      ))
    } else if (identical(target, "Portugal")) {
      HTML(paste(
        h2("Coopérnico – renewable energy-driven cooperative"),
        p(paste(
          "The case study examines Coopérnico, Portugal’s first renewable",
          "energy cooperative, founded in 2013. Coopérnico has more than",
          "1,700 members, including citizens, small and medium-sized",
          "enterprises, and municipalities all over Portugal. Its mission is",
          "to involve its members in reshaping the energy sector to be more",
          "renewable, socially just and collaborative."
        ))
      ))
    } else if (identical(target, "Germany")) {
      HTML(paste(
        h2("The Earnest App – a virtual community for sustainable mobility in Darmstadt"),
        p(paste(
          "The case study explores how the regular use of a sustainability",
          "app can foster energy citizenship among members of a virtual",
          "community. The case study is conducted in Darmstadt – a city with",
          "160.000 inhabitants located in the state of Hesse in Germany. In",
          "cooperation with students from the University for Applied Science",
          "in Darmstadt (h_da), the case study explores how a virtual energy",
          "community – connected by the shared experience of using an app –",
          "affects citizens’ awareness and behaviour in regard to their",
          "mobility and energy consumption choices in everyday life."
        ))
      ))
    } else if (identical(target, "The Netherlands")) {
      HTML(paste(
        h2("Natural gas-free neighbourhoods"),
        p(paste(
          "Most Dutch households currently use natural gas to, for example,",
          "heat their homes. The gas has been mainly produced in the Groningen",
          "gas field, in the northeastern part of the Netherlands. However,",
          "the exploitation of Groningen has caused increasing earthquakes and",
          "damage to the city and nearby areas since the late 1980s. Because",
          "of this, the Netherlands has decided that all its neighbourhoods",
          "will become natural gas-free by 2050. The case study examines the",
          "transition towards natural gas-free homes in the Netherlands."
        ))
      ))
    } else if (identical(target, "Spain")) {
      HTML(paste(
        h2("UR BEROA – energy efficiency-driven cooperative"),
        p(paste(
          "The case study examines UR BEROA, a cooperative providing energy to",
          "the Bera Bera neighbourhood in San Sebastian, Spain. The",
          "cooperative was founded in 1985 to provide hot water and community",
          "heating to the residents and improve the energy efficiency of the",
          "neighbourhood. Since its establishment, the cooperative has",
          "successfully introduced cleaner energy sources and ways to measure",
          "the energy consumption of each household. Now, the cooperative is",
          "slowly making its way toward decarbonisation."
        ))
      ))
    } else {
      HTML(paste(
        p("Click on a map marker to learn more about the GRETA case studies.")
      ))
    }
  })
  
  
  
  # Data explorer ----
  # Show question
  output$question <- renderUI({
    if (!is.null(input$exp_title)) {
      indat <- cb_ext[cb_ext$title %in% input$exp_title, ]

      if (!all(is.na(indat$subitem))) {
        indat <- indat[indat$subitem %in% input$exp_subitem, ]
      }

      if (!all(is.na(indat$option))) {
        indat <- indat[indat$option %in% input$exp_option, ]
      }

      HTML(sprintf(
        "<b>Question %s:</b><br>%s",
        toupper(indat$og_var),
        indat$label
      ))
    } else {
      ""
    }
  })
  
  # Hide or show selectors for subitems or options depending on the question
  observeEvent(input$exp_title, {
    invar <- cb_ext[cb_ext$title %in% input$exp_title, ]$variable
    items <- cb_ext[cb_ext$variable %in% invar, ]$subitem
    options <- cb_ext[cb_ext$variable %in% invar, ]$option
    show_subitems <- length(invar) > 1 & !all(is.na(items))
    show_options <- length(invar) > 1 & !all(is.na(options))
    
    if (show_subitems) {
      updateSelectInput(inputId = "exp_subitem", choices = items)
      shinyjs::show("subitem_hide", anim = TRUE)
    } else {
      shinyjs::hide("subitem_hide", anim = TRUE)
    }
    
    if (show_options) {
      updateSelectInput(inputId = "exp_option", choices = options)
      shinyjs::show("option_hide", anim = TRUE)
    } else {
      shinyjs::hide("option_hide", anim = TRUE)
    }
  })
  
  # TODO: Remove reactivity and add a refresh button
  
  invar <- observeEvent(input$exp_refresh, {
    
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

    has_title <- cb_ext$title %in% input$exp_title
    invar <- cb_ext[has_title, ]

    # case: multiple items exist, look for subitems
    if (length(invar$variable) > 1) {
      has_subitem <- invar$subitem %in% input$exp_subitem
      
      # only select subitems if any exist
      if (any(has_subitem)) {
        invar <- invar[has_subitem, ]
      }
    }

    # case: there's still multiple items, look for options
    if (length(invar$variable) > 1 || !length(invar$variable)) {
      has_option <- invar$option %in% input$exp_option
      
      # if neither option nor subitem exist, select first row to prevent errors
      if (all(!has_option)) {
        has_option <- 1
      }
      invar <- invar[has_option, ]
    }

    invar <- invar$variable

    is_metric <- cb_ext[cb_ext$variable %in% invar, ]$is_metric
    is_dummy <- cb_ext[cb_ext$variable %in% invar, ]$is_dummy ||
      cb_ext[cb_ext$variable %in% invar, ]$is_pdummy

    if (identical(invar, "c1")) {
      lgd <- "Mean age"
      unit <- " years"
    } else if (is_metric) {
      lgd <- "Mean"
      unit <- ""
    } else {
      lgd <- "Share"
      unit <- " %"
      poly[[invar]] <- poly[[invar]] * 100
    }

    leaflet(sf::st_transform(poly[invar], 4326)) %>%
      addTiles() %>%
      setView(lng = 9, lat = 55, zoom = 4) %>%
      addPolygons(
        fillColor = as.formula(paste0("~pal(", invar, ")")),
        fillOpacity = 0.7,
        weight = 1,
        color = "black",
        opacity = 0.5,
        popup = htmltools::htmlEscape(paste0(
          lgd, ": ",
          round(poly[[invar]], 2), unit
        ))
      ) %>%
      addLegend(
        position = "bottomright",
        na.label = "No data",
        pal = pal,
        values = as.formula(paste0("~", invar)),
        opacity = 0.9,
        title = lgd,
        labFormat = labelFormat(suffix = unit)
      )
  })
  
  
  
  # Case studies ----
  output$cs1map <- renderLeaflet({
    cs_bounds <- switch(input$cs1bounds,
      "Quarters" = bgn_1,
      "Zones" = bgn_2,
      "Statistical areas" = bgn_3
    )
    
    cs_bounds <- st_geometry(cs_bounds)
    m <- leaflet(cs_bounds) %>%
      setView(lng = 11.399926, lat = 44.507145, zoom = 13) %>%
      addPolygons(
        color = "black",
        weight = 2,
        opacity = 1,
        fillOpacity = input$cs1opacity
      )
    
    if (identical(input$cs1bg, "Satellite")) {
      m <- addProviderTiles(m, "Esri.WorldImagery")
    } else {
      m <- addTiles(m)
    }
  })
}

shinyApp(ui, server)
