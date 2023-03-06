app_ui <- function() {
  use_css <- tags$style(includeCSS(app_sys("app/www/styles.css")))
  
  all_pals <- RColorBrewer::brewer.pal.info %>%
    dplyr::filter(category == "seq") %>%
    row.names()
  all_pals <- list(
    "Common palettes" = as.list(all_pals),
    "Colorblind palettes" = list(
      "Magma", "Inferno", "Plasma", "Viridis",
      "Cividis", "Rocket", "Mako", "Turbo"
    )
  )
  
  cb_ext <- readRDS("data/codebook.rds")
  
  categories <- unique(na.omit(cb_ext$topic))
  titles <- categories %>%
    purrr::map(~dplyr::filter(cb_ext, topic == .x) %>%
                 dplyr::pull(title) %>% unique() %>%
                 as.list()) %>%
    purrr::set_names(categories)
  
  greta_theme <- fresh::create_theme(
    fresh::bs4dash_status(
      primary = "#FED22B",
      secondary = "#F4F4F2",
      info = "#5E81AC",
      danger = "#BF616A",
      dark = "#F4F4F2"
    ),
    fresh::bs4dash_layout(main_bg = "#FDFDFD", sidebar_width = "350px"),
    fresh::bs4dash_sidebar_light(bg = "#F4F4F2", color = "#000000"),
    fresh::bs4dash_color(
      white = "#FDFDFD",
      gray_800 = "#000000",
      gray_900 = "#000000"
    ),
    fresh::bs4dash_font(family_sans_serif = "Arial")
  )
  
  preloader <- list(
    html = tagList(waiter::spin_6(), "Loading ..."),
    color = "#B3DDFE"
  )
  
  home_tab <- bs4Dash::tabItem(
    "home",
    tags$head(includeCSS("inst/app/www/styles.css")),
    fluidRow(
      bs4Dash::column(
        width = 6,
        bs4Dash::box(
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
      bs4Dash::column(
        width = 6,
        bs4Dash::box(
          title = "Geographical overview",
          width = 12,
          status = "primary",
          leaflet::leafletOutput("csmaps", width = "100%", height = 450)
        ),
        bs4Dash::box(
          title = "Case study descriptions",
          width = 12,
          status = "primary",
          uiOutput("csdesc")
        )
      )
    )
  )
  
  
  
  explorer_tab <- bs4Dash::tabItem(
    "explorer",
    tags$head(includeCSS("inst/app/www/styles.css")),
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
          shinyWidgets::pickerInput(
            "exp_title",
            "Topic",
            titles,
            options = shinyWidgets::pickerOptions(
              windowPadding = c(30, 0, 0, 0),
              liveSearch = TRUE
            )
          ),
          htmlOutput("question"),
          tags$br(),
          shinyjs::hidden(
            div(id = "subitem_hide",
                shinyWidgets::pickerInput("exp_subitem", "Subitem", character())
            )
          ),
          shinyjs::hidden(
            div(id = "option_hide",
                shinyWidgets::pickerInput("exp_option", "Option", character())
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
          shinyWidgets::pickerInput(
            "scale",
            "Aggregation level",
            c("NUTS-0", "NUTS-1", "NUTS-2")
          ),
          shinyWidgets::pickerInput("pal", "Color palette", all_pals)
        ),
        bs4Dash::box(
          title = "Download",
          id = "exp_download",
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = TRUE,
          status = "primary",
          bs4Dash::actionButton(
            "download_button",
            "Download data",
            icon = icon("download", lib = "font-awesome")
          )
        )
      ),
      bs4Dash::column(
        width = 9,
        bs4Dash::box(
          id = "exp_mapbox",
          width = 12,
          collapsible = FALSE,
          solidHeader = FALSE,
          headerBorder = FALSE,
          status = "primary",
          leaflet::leafletOutput("explorer", width = "100%", height = 800)
        )
      )
    )
  )
  
  simulation_tab <- bs4Dash::tabItem(
    "simulation",
    bs4Dash::box(
      "bs4Dash::box content here", br(), "More bs4Dash::box content",
      sliderInput("slider", "Slider input:", 1, 100, 50),
      textInput("text", "Text input:")
    )
  )
  
  spatial_tab <- bs4Dash::tabItem(
    "spatial",
    bs4Dash::box(
     "bs4Dash::box content here", br(), "More bs4Dash::box content",
     sliderInput("slider", "Slider input:", 1, 100, 50),
     textInput("text", "Text input:")
    )
  )
  
  document_tab <- bs4Dash::tabItem(
    "document",
    make_header(
      title = "Income stability: a trivial example to illustrate what a page can look like",
      authors = c("Jonas Lieth", "Dennis Abel", "Stefan Jünger"),
      affil = "GESIS - Leibniz Institute for the Social Sciences",
      date = "06-03-2023"
    ),
    fluidRow(
      col_6(
        box(
          title = "Introduction",
          status = "primary",
          width = 12,
          collapsible = TRUE,
          p2(
            "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam
            nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam
            erat, sed diam voluptua. At vero eos et accusam et justo duo dolores
            et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est
            Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur
            sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et
            dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam
            et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea
            takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit
            amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor
            invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua.
            At vero eos et accusam et justo duo dolores et ea rebum. Stet clita
            kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit
            amet. [1]"
          ),
          p2(
            "Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse
            molestie consequat, vel illum dolore eu feugiat nulla facilisis at
            vero eros et accumsan et iusto odio dignissim qui blandit praesent
            luptatum zzril delenit augue duis dolore te feugait nulla facilisi.
            Lorem ipsum dolor sit amet. [2]"
          )
        )
      ),
      col_6(
        box(
          title = "Methodology",
          status = "primary",
          width = 12,
          collapsible = TRUE,
          p2(
            "Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper
            suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem
            vel eum iriure dolor in hendrerit in vulputate velit esse molestie
            consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et
            accumsan et iusto odio dignissim qui blandit praesent luptatum zzril
            delenit augue duis dolore te feugait nulla facilisi."
            ),
          p2(
            "Nam liber tempor cum soluta nobis eleifend option congue nihil
            imperdiet doming id quod mazim placerat facer possim assum. Lorem
            ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy
            nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.
            Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper
            suscipit lobortis nisl ut aliquip ex ea commodo consequat."
            ),
          p2(
            "Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse
            molestie consequat, vel illum dolore eu feugiat nulla facilisis."
            ),
          p2(
            "At vero eos et accusam et justo duo dolores et ea rebum. Stet clita
            kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit
            amet. Lorem ipsum dolor sit amet, consetetur"
          )
        )
      )
    ), ## end first row
    fluidRow(
      col_12(
        box(
          width = 12,
          title = "Spatial distribution of income stability",
          leaflet::leafletOutput("tempmap", height = 700)
        )
      )
    ),
    fluidRow(
      col_6(box(
        plotly::plotlyOutput("tempdensity"),
        width = 12,
        title = "Distribution of income stability"
      )),
      col_6(box(
        plotly::plotlyOutput("tempscatter"),
        width = 12,
        title = "Relationship between age and income stability"
      ))
    ),
    fluidRow(
      col_6(
        box(
          width = 12,
          title = "Discussion",
          p2(
            "At vero eos et accusam et justo duo dolores et ea rebum. Stet
            clita kasd gubergren, no sea takimata sanctus est Lorem ipsum
            dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing
            elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore
            magna aliquyam erat, sed diam voluptua. At vero eos et accusam et
            justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea
            takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor
            sit amet, consetetur sadipscing elitr, At accusam aliquyam diam
            diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et
            et invidunt justo labore Stet clita ea et gubergren, kasd magna no
            rebum. sanctus sea sed takimata ut vero voluptua. est Lorem ipsum
            dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing
            elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore
            magna aliquyam erat."
          ),
          p2(
            "Consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt
            ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero
            eos et accusam et justo duo dolores et ea rebum. Stet clita kasd
            gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
            Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam
            nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam
            erat, sed diam voluptua. At vero eos et accusam et justo duo dolores
            et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est
            Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur
            sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore
            et dolore magna aliquyam erat, sed diam voluptua. At vero eos et
            accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren,
            no sea takimata sanctus." 
          )
        )
      ),
      col_6(
        box(
          width = 12,
          title = "Conclusion",
          p2(
            "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam
             nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam
             erat, sed diam voluptua. At vero eos et accusam et justo duo
             dolores et ea rebum. Stet clita kasd gubergren, no sea takimata
             sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet,
             consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt
             ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero
             eos et accusam et justo duo dolores et ea rebum. Stet clita kasd
             gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
             Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam
             nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam
             erat, sed diam voluptua. At vero eos et accusam et justo duo
             dolores et ea rebum. Stet clita kasd gubergren, no sea"
          )
        )
      )
    ),
    box(
      title = "References",
      width = 12,
      tags$ul(
        class = "list-style: none",
        p("[1] Bryan Bollinger, Kenneth Gillingham (2012). Peer Effects in the Diffusion of Solar Photovoltaic Panels. Marketing Science 31(6):900-912."),
        p("[2] Ron Boschma (2005) Proximity and Innovation: A Critical Assessment, Regional Studies, 39:1, 61-74."),
      )
    )
  )
  
  cs1_tab <- bs4Dash::tabItem(
    "cs1",
    fluidRow(
     bs4Dash::column(
       width = 6,
       bs4Dash::box(
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
     bs4Dash::column(
       width = 6,
       bs4Dash::box(
         title = "Case study map",
         width = 12,
         status = "primary",
         fluidRow(
           bs4Dash::column(
             width = 3.5,
             shinyWidgets::pickerInput(
               "cs1bounds",
               label = "Boundaries",
               choices = c("Quarters", "Zones", "Statistical areas"),
               shinyWidgets::pickerOptions(windowPadding = c(0, 0, 1000, 0))
             )
           ),
           tags$style("padding-left: 10px"),
           bs4Dash::column(width = 4,
             tags$head(includeCSS("inst/app/www/styles.css")),
             div(
               shinyWidgets::prettyRadioButtons(
                 "cs1bg",
                 label = "Base map",
                 choices = c("OpenStreetMap", "Satellite"),
                 selected = "OpenStreetMap",
                 status = "default",
                 animation = "smooth",
                 shape = "curve"
               ), style = "padding-left: 35px"
             )
           ),
           bs4Dash::column(
             width = 4,
             tags$head(includeCSS("inst/app/www/styles.css")),
             sliderInput(
               "cs1opacity",
               label = "Opacity",
               min = 0,
               max = 1,
               step = 0.1,
               value = 0.2,
               ticks = FALSE
             )
           )
         ),
         leaflet::leafletOutput("cs1map", width = "100%", height = 450)
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
  
  cs2_tab <- bs4Dash::tabItem(
    "cs2",
    bs4Dash::box(
     "bs4Dash::box content here", br(), "More bs4Dash::box content",
     sliderInput("slider", "Slider input:", 1, 100, 50),
     textInput("text", "Text input:")
    )
  )
  
  cs3_tab <- bs4Dash::tabItem(
    "cs3",
    bs4Dash::box(
     "bs4Dash::box content here", br(), "More bs4Dash::box content",
     sliderInput("slider", "Slider input:", 1, 100, 50),
     textInput("text", "Text input:")
    )
  )
  
  cs4_tab <- bs4Dash::tabItem(
    "cs4",
    bs4Dash::box(
     "bs4Dash::box content here", br(), "More bs4Dash::box content",
     sliderInput("slider", "Slider input:", 1, 100, 50),
     textInput("text", "Text input:")
    )
  )
  
  cs5_tab <- bs4Dash::tabItem(
    "cs5",
    bs4Dash::box(
     "bs4Dash::box content here", br(), "More bs4Dash::box content",
     sliderInput("slider", "Slider input:", 1, 100, 50),
     textInput("text", "Text input:")
    )
  )
  
  
  # UI assembly ----
  # TODO: find a way to darken link image hovers
  ui <- bs4Dash::dashboardPage(
    bs4Dash::dashboardHeader(
      tags$style(".fa-bars {color: #00000}"),
      tags$div(
        a(
          href = "https://projectgreta.eu/",
          tags$img(src = "www/greta_logo.svg", height = "35px"),
          style = "padding-top:10px; padding-bottom:10px;"
        ), class = "dropdown"
      ),
      tags$span(style = "display:inline-block; width: 350%"),
      tags$head(includeCSS("inst/app/www/styles.css")),
      tagList(
        a(
          class = "logo",
          href = "https://www.gesis.org/",
          img(src = "www/gesis_logo.png")
        ),
        a(
          class = "logo",
          href = "https://www.unibo.it/",
          img(src = "www/unibo_logo.png")
        ),
        a(
          class = "logo",
          href = "https://www.tecnalia.com/",
          img(src = "www/tecnalia_logo.png")
        )
      ),
      title = HTML(paste(
        img(src = "www/greta_flash.svg", width = 50, height = 50),
        span("GIS tool", class = "brand-text font-weight-light")
      ), sep = "\n"),
      status = "secondary",
      skin = "light",
      sidebarIcon = tags$i(class = "fa fa-bars", style = "color: rgb(0, 0, 0)")
    ),
    bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        shinyWidgets::searchInput(
          "textSearch",
          placeholder = "Search",
          btnSearch = icon("search"),
          btnReset = icon("remove"),
          width = "100%"
        ),
        bs4Dash::menuItem(
          text = "Home",
          icon = icon("house", lib = "font-awesome"),
          tabName = "home"
        ),
        bs4Dash::menuItem(
          text = "Multinational survey",
          icon = icon("map", lib = "font-awesome"),
          tabName = "explorer"
        ),
        bs4Dash::sidebarHeader("Case studies"),
        bs4Dash::menuItem(
          text = "Pilastro-Roveri, Italy",
          tabName = "cs1",
          icon = icon("map-pin", lib = "font-awesome")
        ),
        bs4Dash::menuItem(
          text = "Coopernico, Portugal",
          tabName = "cs2",
          icon = icon("map-pin", lib = "font-awesome")
        ),
        bs4Dash::menuItem(
          text = "The Earnest App, Germany",
          tabName = "cs3",
          icon = icon("map-pin", lib = "font-awesome")
        ),
        bs4Dash::menuItem(
          text = "Gas-free neighborhoods, Netherlands",
          tabName = "cs4",
          icon = icon("map-pin", lib = "font-awesome")
        ),
        bs4Dash::menuItem(
          text = "UR Beroa, Spain",
          tabName = "cs5",
          icon = icon("map-pin", lib = "font-awesome")
        ),
        bs4Dash::sidebarHeader("Individual analyses"),
        bs4Dash::menuItem(
          text = "Simulation study",
          icon = icon("users", lib = "font-awesome"),
          tabName = "simulation"
        ),
        bs4Dash::menuItem(
          text = "Spatial analysis",
          icon = icon("layer-group", lib = "font-awesome"),
          tabName = "spatial"
        ),
        bs4Dash::menuItem(
          text = "Document template",
          icon = icon("file-lines", lib = "font-awesome"),
          tabName = "document"
        ),
        flat = TRUE,
        id = "sidebar"
      ),
      skin = "light",
      minified = TRUE,
      collapsed = FALSE
    ),
    bs4Dash::dashboardBody(
      golem_add_external_resources(),
      bs4Dash::tabItems(
        home_tab,
        explorer_tab,
        simulation_tab,
        spatial_tab,
        document_tab,
        cs1_tab,
        #cs2_tab,
        #cs3_tab,
        #cs4_tab,
        cs5_tab
      )
    ),
    freshTheme = greta_theme,
    dark = NULL,
    preloader = preloader
  )
}