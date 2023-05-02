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
    fresh::bs4dash_font(
      family_sans_serif = "Tablet Gothic",
      family_base = "Tablet Gothic"
    )
  )
  
  # Configure loading screen
  preloader <- list(
    html = tagList(waiter::spin_6(), "Loading ..."),
    color = "#B3DDFE"
  )
  
  
  # UI assembly ----
  ui <- bs4Dash::dashboardPage(
    dashboardHeader2(
      tags$style("
      /* color sidebar button in black */
      .fa-bars {
        color: #00000;
      }
      
      /* set sidebar header to the same height as navbar (roughly) */
      .sidebar-header {
        height: 4.37rem;
      }
      
      /* align sidebar logo and title */
      .brand-link {
        height: 3.1em;
        padding: 0.4rem 0.7rem;
      }
      .brand-text {
        display: inline-block;
        vertical-align: middle;
        font-weight: bold;
      }
      
      /* remove coloring from some elements */
      .nav-pills .nav-link:not(.active):hover {
        color: inherit
      }
      
      a.nav-link {
        color: #000;
      }
      
      /* remove white space from header */
      .navbar {
        padding-top: 0em;
        padding-bottom: 0em;
        padding-right: 0em
      }
      
      /* Align tooltip text */
      .tooltip-inner {
        text-align: left;
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
        span("GRETA Analytics", class = "brand-text")
      )),
      status = "secondary",
      skin = "light",
      sidebarIcon = tags$i(class = "fa fa-bars", style = "color: rgb(0, 0, 0)")
    ),
    bs4Dash::dashboardSidebar(
      id = "sidebarState",
      tags$style(HTML("
        .layout-fixed .wrapper .sidebar {
          height: calc(95vh - (3.5rem + 1px));
        }" # TODO: quick workaround, maybe reconsider
      )),
      bs4Dash::sidebarMenu(
        id = "sidebar",
        tags$form(
          class = "sidebar-form",
          span(
            class = "input-group-btn",
            style = "display:inline-flex; width: 85%;",
            div(class = "search-icon", icon("search", lib = "font-awesome")),
            div(class = "form-control-container input-group", tags$input(
              id = "textSearch",
              type = "search",
              style = "margin: 5px;",
              class = "form-control", 
              placeholder = "Search..."
            ))
          )
        ),
        bs4Dash::sidebarHeader("Start"),
        bs4Dash::menuItem(
          text = txts$home$title,
          icon = txts$home$icon,
          tabName = "home"
        ),
        bs4Dash::menuItem(
          text = "Multinational survey",
          icon = txts$exp$icon,
          tabName = "mns",
          bs4Dash::menuSubItem(text = "Explore data", tabName = "exp"),
          bs4Dash::menuSubItem(text = "Compare data", tabName = "cmp"),
          bs4Dash::menuSubItem(text = "Inspect data", tabName = "insp")
        ),
        bs4Dash::sidebarHeader("Case studies"),
        bs4Dash::menuItem(
          text = txts$cs1$title,
          icon = txts$cs1$icon,
          tabName = "cs1"
        ),
        bs4Dash::menuItem(
          text = txts$cs2$title,
          icon = txts$cs2$icon,
          tabName = "cs2"
        ),
        bs4Dash::menuItem(
          text = txts$cs3$title,
          icon = txts$cs3$icon,
          tabName = "cs3"
        ),
        bs4Dash::menuItem(
          text = txts$cs4$title,
          icon = txts$cs4$icon,
          tabName = "cs4"
        ),
        bs4Dash::menuItem(
          text = txts$cs5$title,
          icon = txts$cs5$icon,
          tabName = "cs5"
        ),
        bs4Dash::sidebarHeader("Individual analyses"),
        # bs4Dash::menuItem(
        #   text = txts$taxonomy$title,
        #   icon = txts$taxonomy$icon,
        #   tabName = "taxonomy"
        # ),
        bs4Dash::menuItem(
          text = txts$simulation$title,
          icon = txts$simulation$icon,
          tabName = "simulation"
        ),
        bs4Dash::menuItem(
          text = txts$spatial$title,
          icon = txts$spatial$icon,
          tabName = "spatial"
        ),
        bs4Dash::menuItem(
          text = txts$income$title,
          icon = txts$income$icon,
          tabName = "income"
        ),
        flat = TRUE
      ),
      skin = "light",
      minified = TRUE,
      collapsed = TRUE,
      fixed = TRUE
    ),
    bs4Dash::dashboardBody(
      add_external_resources(),
      mod_main_ui("main")
    ),
    freshTheme = greta_theme,
    dark = NULL,
    preloader = preloader,
    options = list(sidebarSlimScroll = TRUE)
  )
}