mod_sandbox_ui <- function(id) {
  ns <- NS(id)
  
  countries <- list(
    "France", "United Kingdom", "Germany", "United States of America", "Belgium", "China", "Spain", "Netherlands", "Mexico",
    "Italy", "Canada", "Brazil", "Denmark", "Norway", "Switzerland", "Luxembourg", "Israel", "Russian Federation",
    "Turkey", "Saudi Arabia", "United Arab Emirates"
  )
  flags <- c("fr", "gb", "de", "us", "be", "cn", "es", "nl", "mx", "it", "ca", "br", "dk", "no", "ch", "lu", "il", "ru", "tr", "sa", "ae")
  flags <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", flags)
  
  bs4Dash::tabItem(
    "sandbox",
    fluidRow(
      col_6(
        bs4Dash::tabBox(
          title = "Example title",
          footer = "Example footer",
          status = "info",
          closable = TRUE,
          maximizable = TRUE,
          width = 12,
          type = "tabs",
          icon = icon("lightbulb", lib = "font-awesome"),
          label = bs4Dash::boxLabel("label", status = "info", tooltip = "This is an optional label"),
          sidebar = bs4Dash::boxSidebar(
            id = "sandbox_sidebar",
            shinyWidgets::switchInput("sandbox_switch")
          ),
          dropdownMenu = bs4Dash::boxDropdown(
            bs4Dash::boxDropdownItem("Dropdown item"),
            bs4Dash::boxDropdownItem("GRETA website", href = "https://projectgreta.eu/")
          ),
          tabPanel(
            title = "Tab 1",
            p2("This is an exemplary box content. Boxes constitute the general building
             blocks of the tabs in this tool. They can contain text paragraphs,
             interactive maps and plots as well as static figures. Boxes can also
             contain additional features such as sidebars, dropdown menus and buttons
             to minimize, maximize or close the box (as shown in the header of this box).")
          ),
          tabPanel(
            title = "Tab 2",
            p2("The following button dynamically generates text, in the same way it could
            also generate a figure or a plot."),
            actionButton("sandbox_button", label = "Generate text"),
            uiOutput("sandbox_text")
          )
        ),
        bs4Dash::box(
          title = "Checkbox buttons",
          status = "primary",
          width = 12,
          shinyWidgets::checkboxGroupButtons(
            inputId = "Id051",
            label = "Preselected buttons",
            choices = c("A", "B", "C", "D"),
            selected = c("B", "D")
          ),
          shinyWidgets::checkboxGroupButtons(
            inputId = "Id057",
            label = "Buttons with icons", 
            choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "line", 
                        `<i class='fa fa-pie-chart'></i>` = "pie"),
            justified = TRUE
          ),
          shinyWidgets::radioGroupButtons(
            inputId = "Id073",
            label = "Radio buttons",
            choices = c("Option 1", 
                        "Option 2", "Option 3", "Option 4"),
            individual = TRUE,
            checkIcon = list(
              yes = tags$i(class = "fa fa-circle", 
                           style = "color: steelblue"),
              no = tags$i(class = "fa fa-circle-o", 
                          style = "color: steelblue"))
          )
        ),
        bs4Dash::box(
          title = "Numeric input",
          status = "secondary",
          width = 12,
          sliderInput("slider2", label = h3("Slider Range"), min = 0, 
                      max = 100, value = c(40, 60)
          ),
          numericInput("num", label = h3("Numeric input"), value = 1),
          shinyWidgets::airDatepickerInput(
            "airdatesandbox",
            label = "Time picker",
            range = TRUE
          )
        )
      ),
      col_6(
        bs4Dash::box(
          title = "Checkboxes",
          status = "primary",
          width = 12,
          shinyWidgets::prettyCheckbox(
            inputId = "Id021",
            label = "Single checkbox", 
            value = TRUE,
            status = "danger",
            shape = "curve"
          ),
          shinyWidgets::prettyCheckboxGroup(
            inputId = "Id032",
            label = "Multiple checkboxes", 
            choices = c("Click me!", "Me!", "Or me!")
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = "Id036",
            label = "Radio Buttons", 
            choices = c("Click me !", "Me !", "Or me !")
          )
        ),
        bs4Dash::box(
          title = "Input pickers",
          status = "danger",
          width = 12,
          shinyWidgets::pickerInput(
            inputId = "Id082",
            label = "Picker with search and categories", 
            choices = list(
              lower = c("a", "b", "c", "d"),
              upper = c("A", "B", "C", "D")),
            options = list("live-search" = TRUE)
          ),
          tags$style(HTML(".item {color: black;}")),
          shinyWidgets::multiInput(
            inputId = "Id010",
            label = "Picker with multiple choices", 
            choices = NULL,
            choiceNames = lapply(seq_along(countries), function(i) {
              tagList(tags$img(src = flags[i],
                               width = 20, 
                               height = 15), countries[i]
              )
            }),
            choiceValues = countries
          )
        ),
        bs4Dash::box(
          title = "Buttons",
          status = "primary",
          width = 12,
          actionButton(inputId = "success", label = "Success!", width = "80%",
                       class = "btn-success", style = "color: #FFF"),
          br(),
          actionButton(inputId = "info", label = "Info", width = "80%",
                       class = "btn-info", style = "color: #FFF"),
          br(),
          actionButton(inputId = "error", label = "Error", width = "80%",
                       class = "btn-danger", style = "color: #FFF"),
          br(),
          actionButton(inputId = "warning", label = "Warning", width = "80%",
                       class = "btn-warning", style = "color: #FFF")
        )
      )
    )
  )
}


mod_sandbox <- function(input, output, session) {
  sbtext <- eventReactive(input$sandbox_button, {
    num <- sample(c("a","b","c","d","e"), 1)
    switch(
      num,
      a = "I'm a generated text.",
      b = "<b>I'm a bold text</b>",
      c = "<del>I'm not a text</del>",
      d = "<small>I'm a small text</small>",
      e = "<em>I'm an emphasized text</em>"
    )
  })
  
  output$sandbox_text <- renderUI({
    HTML(sbtext())
  })
  
  observeEvent(input$sandbox_switch, {
    bs4Dash::updateBoxSidebar("coopmap2_sidebar")
  })
  
  observeEvent(input$success, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "All in order",
      type = "success"
    )
  })
  
  observeEvent(input$error, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Oups !",
      type = "error"
    )
  })
  
  observeEvent(input$info, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Information",
      text = "Something helpful",
      type = "info"
    )
  })
  
  observeEvent(input$warning, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Warning !!!",
      text = NULL,
      type = "warning"
    )
  })
}


mod_sandbox_server <- function(id) {
  moduleServer(id, mod_sandbox)
}