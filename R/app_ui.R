# Create global list to store UI texts to prevent spamming code files
txts <- list(main = list())


# UI assembly ----
#' @title GRETA Shiny base module
#' @description Create the UI and server function of the GRETA Shiny app.
#'
#' @param theme Fresh theme to apply to the UI
#' @param preloader Preloader to run when the UI is loading
#' @param options Further AdminLTE options, see \code{\link[bs4Dash]{dashboardPage}}
#' @param sidebar_fixed Whether to fix the sidebar to the screen
#' @param header_fixed Whether to fix the header to the screen
#' @param minifed Whether to minify the sidebar when collapsed
#' @param controlbar UI to add to the controlbar, see \code{\link[bs4Dash]{dashboardControlbar}}
#' @param footer UI to add to the footer, see \code{\link[bs4Dash]{dashboardFooter}}
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @keywords internal
#' @rdname mod_base
#' @export
app_ui <- function(theme = NULL,
                   preloader = NULL,
                   options = NULL,
                   sidebar_fixed = FALSE,
                   header_fixed = FALSE,
                   minified = TRUE,
                   controlbar = NULL,
                   footer = NULL) {
  all_pals <- list_palettes()
  get_text <- dispatch_to_txt("main")

  if (!is.null(theme)) {
    theme <- structure(
      paste(readLines(app_sys("app/www/theme.css"))),
      class = c("css", "html", "character"),
      html = TRUE
    )
  }
  
  
  # Configure loading screen
  if (is.null(preloader)) {
    preloader <- list(
      html = tagList(waiter::spin_6(), "Loading ..."),
      color = "#B3DDFE"
    )
  }

  bs4Dash::dashboardPage(
    bs4Dash::dashboardHeader(
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

      a {
        color: #000;
      }

      /* remove white space from header */
      .navbar {
        padding-top: 0em;
        padding-bottom: 0em;
        padding-right: 0em
      }
      
      .main-footer {
        padding: 0.5rem;
      }

      /* Align tooltip text */
      .tooltip-inner {
        text-align: left;
      }"),
      div(
        id = "greta-logo",
        a(
          href = "https://projectgreta.eu/",
          tags$img(src = "www/greta_logo.svg", height = "37em"),
          style = "padding: 0.45em"
        ), class = "logo"
      ),
      span(style = "display:inline-block; width: 100%"), # logos at the end of header
      # bs4Dash::tooltip(
      #   actionButton(
      #     "tour",
      #     label = "",
      #     icon = icon("question"),
      #     width = "50px",
      #     style = "margin-right: 20px;"
      #   ),
      #   title = "Start a guided tour"
      # ),
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
      tags$style(HTML(
        "
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
          text = get_text("home", "shortitle"),
          icon = icon(get_text("home", "icon")),
          tabName = "home"
        ),
        bs4Dash::menuItem(
          text = "Multinational survey",
          icon = icon(get_text("exp", "icon")),
          tabName = "mns",
          bs4Dash::menuSubItem(
            text = get_text("exp", "shortitle"),
            tabName = "exp"
          ),
          bs4Dash::menuSubItem(
            text = get_text("cmp", "shortitle"),
            tabName = "cmp"
          ),
          bs4Dash::menuSubItem(
            text = get_text("insp", "shortitle"),
            tabName = "insp"
          )
        ),
        bs4Dash::sidebarHeader("GRETA output"),
        bs4Dash::menuItem(
          text = get_text("cs1", "shortitle"),
          icon = icon(get_text("cs1", "icon")),
          tabName = "cs1italy"
        ),
        bs4Dash::menuItem(
          text = get_text("cs5", "shortitle"),
          icon = icon(get_text("cs5", "icon")),
          tabName = "cs5spain"
        ),
        bs4Dash::menuItem(
          text = get_text("taxonomy", "shortitle"),
          icon = icon(get_text("taxonomy", "icon")),
          tabName = "taxonomy"
        ),
        # bs4Dash::menuItem(
        #   text = get_text("stakeholder", "shortitle"),
        #   icon = icon(get_text("stakeholder", "icon")),
        #   tabName = "stakeholder"
        # ),
        bs4Dash::menuItem(
          text = get_text("persona", "shortitle"),
          icon = icon(get_text("persona", "icon")),
          tabName = "persona"
        ),
        # bs4Dash::menuItem(
        #   text = get_text("enpov", "shortitle"),
        #   icon = icon(get_text("enpov", "icon")),
        #   tabName = "enpov"
        # ),
        # bs4Dash::menuItem(
        #   text = get_text("attitudes", "shortitle"),
        #   icon = icon(get_text("attitudes", "icon")),
        #   tabName = "attitudes"
        # ),
        # bs4Dash::menuItem(
        #   text = get_text("research", "shortitle"),
        #   icon = icon(get_text("research", "icon")),
        #   tabName = "research"
        # ),
        flat = TRUE
      ),
      skin = "light",
      minified = TRUE,
      collapsed = TRUE,
      expandOnHover = FALSE,
      fixed = FALSE
    ),
    footer = bs4Dash::dashboardFooter(
      right = div(
        get_text("home", "disclaimer"),
        style = "font-size: 0.9rem;"
      ),
      fixed = TRUE
    ),
    bs4Dash::dashboardBody(
      add_external_resources(),
      mod_main_ui("main")
    ),
    freshTheme = theme,
    dark = NULL,
    preloader = preloader,
    help = TRUE,
    options = list(sidebarSlimScroll = TRUE)
  )
}
