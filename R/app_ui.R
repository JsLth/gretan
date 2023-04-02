# Create global list to store UI texts to prevent spamming code files
txts <- list()

app_ui <- function() {
  all_pals <- list_palettes()
  
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
  
  
  # UI assembly ----
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
      bs4Dash::sidebarMenu( # TODO: implement search bar
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
      mod_main_ui("main-ui")
    ),
    freshTheme = greta_theme,
    dark = NULL,
    preloader = preloader,
    options = list(sidebarSlimScroll = TRUE)
  )
}