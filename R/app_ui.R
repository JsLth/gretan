app_ui <- function() {
  all_pals <- list_palettes()
  
  categories <- unique(cb_ext$topic[!is.na(cb_ext$topic)])
  titles <- categories %>%
    purrr::map(~dplyr::filter(cb_ext, topic == .x) %>%
                 dplyr::pull(title) %>% unique() %>%
                 as.list()) %>%
    purrr::set_names(categories)
  
  # source: agendahelsinki.tailored.css
  greta_theme <- fresh::create_theme(
    fresh::bs4dash_status(
      primary = "#FED22B",
      secondary = "#F4F4F2",
      info = "#B3DDFE",
      danger = "#BF616A",
      dark = "#F4F4F2"
    ),
    fresh::bs4dash_layout(main_bg = "#FDFDFD", sidebar_width = "350px"),
    fresh::bs4dash_sidebar_light(bg = "#F4F4F2", color = "#000"),
    fresh::bs4dash_color(
      white = "#FDFDFD",
      black = "#000",
      gray_600 = "#CFCFCF",
      gray_800 = "#B4B4B4",
      gray_900 = "#000",
      red = "#C1120E",
      purple = "#3F1354",
      yellow = "#FFFD37",
      blue = "#002562",
      teal = "#00767E"
    ),
    fresh::bs4dash_font(family_sans_serif = "Arial")
  )
  
  # Configure loading screen
  preloader <- list(
    html = tagList(waiter::spin_6(), "Loading ..."),
    color = "#B3DDFE"
  )
  
  home_tab <- bs4Dash::tabItem(
    "home",
    fluidRow(
      bs4Dash::column(
        width = 6,
        bs4Dash::box(
          title = "Welcome to the GRETA GIS tool",
          width = 12,
          status = "primary",
          p2("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
          p2("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat."),
          p2("Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi."),
          h2("Subtitle"),
          p2("Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat."),
          p2("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis."),
          p2("At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, At accusam aliquyam diam diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et et invidunt justo labore Stet clita ea et gubergren, kasd magna no rebum. sanctus sea sed takimata ut vero voluptua. est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur")
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
          shinyWidgets::pickerInput("pal", "Color palette", all_pals),
          shinyjs::disabled(div(
            id = "fixed_hide",
            shinyWidgets::prettyRadioButtons(
              "expfixed",
              "Legend values",
              choices = c("Full contrast", "Full range"),
              selected = "Full contrast",
              inline = TRUE
            )
          ))
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
    make_header(
      title = "Title for a GRETA-related analysis",
      authors = c("Author 1", "Author 2"),
      affil = list(
        "Author 1" = c("Institution 1", "Institution 2"),
        "Author 2" = c("Institution 2")
      ),
      date = "DD-MM-YYYY"
    )
  )
  
  spatial_tab <- bs4Dash::tabItem(
    "spatial",
    make_header(
      title = "Investment in the Coop\u00e9rnico project: An examplary analysis of two projects",
      authors = c("Dennis Abel", "Jonas Lieth"),
      affil = "GESIS - Leibniz Institute for the Social Sciences",
      date = "08-03-2023"
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          title = "Introduction",
          status = "primary",
          width = 12,
          collapsible = TRUE,
          p2(shinipsum::random_text(nwords = 200))
        )
      ),
      col_6(
        bs4Dash::box(
          title = "Methodology",
          status = "primary",
          width = 12,
          collapsible = TRUE,
          p2(shinipsum::random_text(nwords = 200))
        )
      )
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          width = 12,
          status = "primary",
          title = "Geographical distribution of Coop\u00e9rnico investments",
          leaflet::leafletOutput("coopmap1", height = 700)
        )
      ),
      col_6(
        bs4Dash::box(
          width = 12,
          status = "primary",
          title = "Spatial clusters of Coop\u00e9rnico investments",
          leaflet::leafletOutput("coopmap2", height = 700),
          sidebar = bs4Dash::boxSidebar(
            id = "coopmap2_sidebar",
            background = "#CFCFCF",
            shinyWidgets::prettyRadioButtons(
              "coopmap2_sidebar_scheme",
              label = "Coding scheme",
              choices = c(
                "Raw" = "raw",
                "Binary" = "B",
                "Row standardized" = "W",
                "Globally standardized" = "C",
                "Universally standardized" = "U",
                "Minmax" = "minmax",
                "Variance stabilizing" = "S"
              ),
              selected = "raw",
              status = "default",
              animation = "smooth"
            ),
            shinyWidgets::pickerInput(
              "coopmap2_sidebar_dist",
              label = "Distance modelling",
              choices = c(
                "Inverse distance weighting" = "idw",
                "Exponential distance decay" = "exp",
                "Double-power distance weighting" = "dpd"
              ),
              width = "90%"
            ),
            numericInput(
              "coopmap2_sidebar_alpha",
              label = "Distance modelling parameter",
              min = 0, max = NA, value = 1, step = 0.1
            ),
            numericInput(
              "coopmap2_sidebar_dmax",
              label = "Maximum distance threshold",
              min = 1, max = NA, value = 1
            ),
            actionButton(
              "coopmap2_sidebar_apply",
              label = "Apply changes",
              icon = icon("refresh", lib = "font-awesome")
            )
          )
        )
      )
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          title = "Discussion",
          status = "primary",
          width = 12,
          collapsible = TRUE,
          p2(shinipsum::random_text(nwords = 550))
        )
      ),
      col_6(
        bs4Dash::box(
          width = 12,
          status = "primary",
          title = "Scatterplot of Moran's I",
          plotly::plotlyOutput("coopscatter")
        ),
        bs4Dash::box(
          title = "References",
          status = "primary",
          width = 12,
          dummy_bibliography()
        )
      )
    )
  )
  
  income_tab <- bs4Dash::tabItem(
    "income",
    make_header(
      title = "Income stability: a trivial example to illustrate what a page can look like",
      authors = c("Jonas Lieth", "Dennis Abel", "Stefan J\u00fcnger"),
      affil = "GESIS - Leibniz Institute for the Social Sciences",
      date = "06-03-2023"
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          title = "Introduction",
          status = "primary",
          width = 12,
          collapsible = TRUE,
          p2(shinipsum::random_text(nwords = 200))
        )
      ),
      col_6(
        bs4Dash::box(
          title = "Methodology",
          status = "primary",
          width = 12,
          collapsible = TRUE,
          p2(shinipsum::random_text(nwords = 200))
        )
      )
    ), ## end first row
    fluidRow(
      col_12(
        bs4Dash::box(
          width = 12,
          status = "primary",
          title = "Spatial distribution of income stability",
          leaflet::leafletOutput("tempmap", height = 700)
        )
      )
    ),
    fluidRow(
      col_6(bs4Dash::box(
        plotly::plotlyOutput("tempdensity"),
        width = 12,
        status = "primary",
        title = "Distribution of income stability"
      )),
      col_6(bs4Dash::box(
        plotly::plotlyOutput("tempscatter"),
        width = 12,
        status = "primary",
        title = "Relationship between age and income stability"
      ))
    ),
    fluidRow(
      col_6(
        bs4Dash::box(
          width = 12,
          title = "Discussion",
          status = "primary",
          p2(shinipsum::random_text(nwords = 200))
        )
      ),
      col_6(
        bs4Dash::box(
          width = 12,
          title = "Conclusion",
          status = "primary",
          p2(shinipsum::random_text(nwords = 150))
        )
      )
    ),
    bs4Dash::box(
      title = "References",
      width = 12,
      status = "primary",
      dummy_bibliography()
    )
  )
  
  cs1_tab <- bs4Dash::tabItem(
    "cs1",
    make_header(
      title = "Case study 1: Reneweable energy district Pilastro-Roveri",
      authors = c("Prepared by: Author A", "Author B"),
      affil = list(
        "Author A" = "University of Bologna, Department of Architecture",
        "Author B" = "University of Bologna, Department of Architecture"
      ),
      date = "2023-mm-dd"
    ),
    fluidRow(
     bs4Dash::column(
       width = 6,
       bs4Dash::box(
         title = "Renewable energy district",
         width = 12,
         status = "primary",
         p2("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
         p2("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat."),
         p2("Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi."),
         h2("Subtitle"),
         p2("Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat."),
         p2("Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis."),
         p2("At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, At accusam aliquyam diam diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et et invidunt justo labore Stet clita ea et gubergren, kasd magna no rebum. sanctus sea sed takimata ut vero voluptua. est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur")
       ),
       bs4Dash::box(
         title = "Text with a figure",
         width = 12,
         status = "primary",
         p2(shinipsum::random_text(nwords = 150)),
         img(
           src = "https://projectgreta.eu/wp-content/uploads/2021/08/GRETA_3_KUVITUS_Valkotausta-1024x724.png",
           style = "width: 100%;"
         ),
         p(HTML("<b>Fig. 1:</b> An example figure"))
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
               shinyWidgets::pickerOptions(windowPadding = c(0, 0, 1000, 0)),
               width = 200
             )
           ),
           tags$style("padding-left: 10px"),
           bs4Dash::column(width = 4,
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
         solidHeader = FALSE,
         headerBorder = FALSE,
         collapsible = FALSE,
         uiOutput("cs1poi", style = "padding-bottom: -10px; padding-top: -20px;")
       ),
       bs4Dash::box(
         title = "Another text",
         width = 12,
         status = "primary",
         p2(shinipsum::random_text(nwords = 700))
       )
     )
    )
  )
  
  cs2_tab <- bs4Dash::tabItem(
    "cs2",
    make_header(
      title = "Case study 2: Coop\u00e9rnico \u2013 renewable energy-driven cooperative",
      authors = c("Prepared by: Author A", "Author B"),
      affil = list(
        "Author A" = "Cleanwatts",
        "Author B" = "Cleanwatts"
      ),
      date = "2023-mm-dd"
    )
  )
  
  cs3_tab <- bs4Dash::tabItem(
    "cs3",
    make_header(
      title = "Case study 3: The Earnest App \u2013 a virtual community for sustainable mobility in Darmstadt",
      authors = c("Prepared by: Author A", "Author B"),
      affil = list(
        "Author A" = "Fraunhofer Institute for Systems and Innovation Research",
        "Author B" = "Fraunhofer Institute for Systems and Innovation Research"
      ),
      date = "2023-mm-dd"
    )
  )
  
  cs4_tab <- bs4Dash::tabItem(
    "cs4",
    make_header(
      title = "Case study 4: Natural gas-free neighbourhoods",
      authors = c("Prepared by: Author A", "Author B"),
      affil = list(
        "Author A" = "Netherlands Organisation for Applied Scientific Research",
        "Author B" = "Netherlands Organisation for Applied Scientific Research"
      ),
      date = "2023-mm-dd"
    )
  )
  
  cs5_tab <- bs4Dash::tabItem(
    "cs5",
    make_header(
      title = "Case study 5: UR BEROA \u2013 energy efficiency-driven cooperative",
      authors = c("Prepared by: Author A", "Author B"),
      affil = list(
        "Author A" = "Tecnalia Research and Innovation",
        "Author B" = "Tecnalia Research and Innovation"
      ),
      date = "2023-mm-dd"
    )
  )
  
  
  # UI assembly ----
  # TODO: find a way to darken link image hovers
  ui <- bs4Dash::dashboardPage(
    bs4Dash::dashboardHeader(
      tags$style("
        /* color sidebar button in black */
        .fa-bars {
          color: #00000;
        }
        
        /* set sidebar header to the same height as navbar (roughly) */
        .sidebar-header {
          height: 4.37rem
        }
        
        /* align sidebar logo and title */
        .brand-text {
          display: inline-block;
          vertical-align: middle;
          font-weight: bold;
        }
        
        /* remove white space from header */
        .navbar {
          padding-top: 0em;
          padding-bottom: 0em;
          padding-right: 0em
        }"),
      div(
        a(
          href = "https://projectgreta.eu/",
          tags$img(src = "www/greta_logo.svg", height = "37em"),
          style = "padding: 0.45em"
        ), class = "logo"
      ),
      span(style = "display:inline-block; width: 100%"), # logos at the end of header
      div( # insert logos in a grid
        class = "container-logo",
        corp_logo("gesis"), corp_logo("lut"), corp_logo("unibo"),
        corp_logo("tecnalia"), corp_logo("cleanwatts"), corp_logo("tno"),
        corp_logo("isi"), corp_logo("kaskas")
      ),
      title = HTML(paste(
        #style = "display: inline-block; vertical-align: middle;",
        img(src = "www/greta_flash.svg", width = 50, height = 50),
        span("GIS tool", class = "brand-text")
      )),
      status = "secondary",
      skin = "light",
      sidebarIcon = tags$i(class = "fa fa-bars", style = "color: rgb(0, 0, 0)")
    ),
    bs4Dash::dashboardSidebar(
      tags$style(HTML("
        .layout-fixed .wrapper .sidebar {
          height: calc(95vh - (3.5rem + 1px));
        }" # TODO: quick workaround, maybe reconsider
      )),
      bs4Dash::sidebarMenu(
        tags$form(
          class = "sidebar-form",
          span(
            class = "input-group-btn",
            style = "display:inline-flex; width: 85%;",
            tags$button(
              id = "searchButton",
              type = "button",
              class = "btn btn-flat action-button",
              icon("search", lib = "font-awesome")
            ),
            div(
              class = "input-group",
              style = "margin-left: 0.5em",
              tags$input(
                id = "textSearch",
                type = "text",
                class = "form-control", 
                placeholder = "Search...",
                style = "margin: 5px"
              )
            )
          )
        ),
        bs4Dash::sidebarHeader("Start"),
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
          text = "Income stability",
          icon = icon("money-bill-trend-up", lib = "font-awesome"),
          tabName = "income"
        ),
        bs4Dash::sidebarHeader("Other"),
        bs4Dash::menuItem(
          text = "Sandbox",
          icon = icon("wand-magic", lib = "font-awesome"),
          tabName = "sandbox"
        ),
        flat = TRUE,
        id = "sidebar"
      ),
      skin = "light",
      minified = TRUE,
      collapsed = TRUE,
      fixed = TRUE
    ),
    bs4Dash::dashboardBody(
      add_external_resources(),
      bs4Dash::tabItems(
        home_tab,
        explorer_tab,
        simulation_tab,
        spatial_tab,
        income_tab,
        cs1_tab,
        cs2_tab,
        cs3_tab,
        cs4_tab,
        cs5_tab,
        sandbox
      )
    ),
    freshTheme = greta_theme,
    dark = NULL,
    preloader = preloader,
    options = list(sidebarSlimScroll = TRUE)
  )
}