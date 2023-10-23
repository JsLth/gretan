test_that("helpBox works", {
  expect_snapshot(helpBox(
    help_id = "test",
    title = "test title",
    footer = "test footer",
    status = "primary",
    solidHeader = TRUE,
    background = "primary",
    width = 2,
    height = "400px",
    collapsed = TRUE,
    closable = TRUE,
    maximizable = TRUE,
    icon = shiny::icon("refresh"),
    gradient = TRUE,
    boxToolSize = "lg",
    elevation = 1,
    headerBorder = FALSE,
    id = "testbox"
  ))

  expect_identical(helpBox(), bs4Dash::box())

  expect_snapshot(helpBox(help_id = "test", collapsible = FALSE))
})

test_that("leafletPanel works", {
  expect_snapshot(leafletPanel(
    "test",
    "this is a panel",
    title = "title",
    position = "bottomleft",
    width = 400,
    top = 10
  ))

  expect_snapshot(leafletPanel("test", position = "topleft"))
  expect_snapshot(leafletPanel("test", position = "bottomleft"))
  expect_snapshot(leafletPanel("test", position = "topright"))
  expect_snapshot(leafletPanel("test", position = "bottomright"))
})

test_that("groupRadioButtons works", {
  awesome <- shinyWidgets::awesomeRadio(
    "test",
    label = "",
    choices = c("A", "B", "C")
  )
  awesome <- groupRadioButtons(
    awesome,
    index = c(1, 2, 3),
    groups = c("1st", "2nd", "3rd"),
    type = "awesome"
  )
  expect_snapshot(awesome)

  pretty <- shinyWidgets::prettyRadioButtons(
    "test",
    label = "",
    choices = c("A", "B", "C")
  )
  pretty <- groupRadioButtons(
    pretty,
    index = c(1, 2, 3),
    groups = c("1st", "2nd", "3rd"),
    type = "pretty"
  )
  expect_snapshot(pretty)

  classic <- shiny::radioButtons(
    "test",
    label = "",
    choices = c("A", "B", "C")
  )
  classic <- groupRadioButtons(
    classic,
    index = c(1, 2, 3),
    groups = c("1st", "2nd", "3rd"),
    type = "default"
  )
  expect_snapshot(classic)
})

test_that("loadingButton works", {
  button <- loadingButton(
    "test",
    "button",
    icon = icon("wand-magic-sparkles"),
    status = "success",
    size = "lg"
  )
  expect_snapshot(button)
})


test_that("with_supref works", {
  expect_equal(with_supref("1"), "1", ignore_attr = TRUE)
  expect_s3_class(with_supref("{@1}"), "html")
})


test_that("with_literata works", {
  expect_match(with_literata("test")[[2]]$style, "font-family: Literata", fixed = TRUE)
})


test_that("with_gothic works", {
  expect_match(with_gothic("test")[[2]]$style, "font-family: Tablet Gothic", fixed = TRUE)
})


test_that("Test columns wrappers works", {
  expect_s3_class(col_12(), "shiny.tag")
  expect_s3_class(col_11(), "shiny.tag")
  expect_s3_class(col_10(), "shiny.tag")
  expect_s3_class(col_9(), "shiny.tag")
  expect_s3_class(col_8(), "shiny.tag")
  expect_s3_class(col_7(), "shiny.tag")
  expect_s3_class(col_6(), "shiny.tag")
  expect_s3_class(col_5(), "shiny.tag")
  expect_s3_class(col_4(), "shiny.tag")
  expect_s3_class(col_3(), "shiny.tag")
  expect_s3_class(col_2(), "shiny.tag")
  expect_s3_class(col_1(), "shiny.tag")

  expect_equal(as.character(col_12()), '<div class="col-sm-12"></div>')
  expect_equal(as.character(col_11()), '<div class="col-sm-11"></div>')
  expect_equal(as.character(col_10()), '<div class="col-sm-10"></div>')
  expect_equal(as.character(col_9()), '<div class="col-sm-9"></div>')
  expect_equal(as.character(col_8()), '<div class="col-sm-8"></div>')
  expect_equal(as.character(col_7()), '<div class="col-sm-7"></div>')
  expect_equal(as.character(col_6()), '<div class="col-sm-6"></div>')
  expect_equal(as.character(col_5()), '<div class="col-sm-5"></div>')
  expect_equal(as.character(col_4()), '<div class="col-sm-4"></div>')
  expect_equal(as.character(col_3()), '<div class="col-sm-3"></div>')
  expect_equal(as.character(col_2()), '<div class="col-sm-2"></div>')
  expect_equal(as.character(col_1()), '<div class="col-sm-1"></div>')
})


test_that("match_regex works", {
  expect_identical(match_regex("abcd", "c"), list("c"))
  expect_identical(match_regex("1_b", "[0-9](.)[a-z]"), list(c("1_b", "_")))
})


test_that("tag_to_text works", {
  html_dep <- shinyWidgets::html_dependency_awesome()()
  expect_identical(tag_to_text(html_dep), "")
  expect_identical(tag_to_text(tags$style("width: 20px;")), "")
  expect_identical(tag_to_text(tags$b()), "")
  expect_identical(tag_to_text(NULL), "")
  expect_identical(tag_to_text(HTML("<br/>")), "\n")
  expect_identical(tag_to_text(HTML("<p>text</b>")), "text")
  expect_identical(tag_to_text(tags$b("text")), "text")
  expect_identical(tag_to_text(list(tags$p("text"), tags$div("text2"))), "text\ntext2")
})


test_that("make_header works", {
  expect_snapshot(make_header("title", c("author 1", "author 2"), affil = "institution"))
  expect_snapshot(make_header(
    "title",
    c("author 1", "author 2"),
    affil = c("author 1" = "institution 1", "author 2" = "institution 2")
  ))
  expect_snapshot(make_header("title", c("author 1", "author 2")))
})


test_that("corp_logo works", {
  logo <- corp_logo("gesis")
  expect_s3_class(logo, "shiny.tag")
  expect_identical(logo[[2]], list(class = "logo"))
})


test_that("invert works", {
  lst <- list(a = "name_1", b = "name_2")
  expect_named(invert(lst), c("name_1", "name_2"))
  expect_contains(invert(lst), c("a", "b"))
})


test_that("p2 works", {
  expect_identical(p2("test")[[2]], list(class = "fancy"))
})


test_that("pbib works", {
  expect_identical(pbib("test")[[2]], list(class = "bib"))
})


test_that("noWS works", {
  expect_false(grepl("\\s", noWS(tags$ul)(tags$li("item"))))
})


test_that("list_palettes works", {
  expect_named(list_palettes(), c("Sequential", "Colorblind"))
  expect_named(list_palettes("qual"), "Qualitative")
})


test_that("list_to_li works", {
  li <- list_to_li(list("a", "b"))
  expect_identical(li[[1]], "ul")
  expect_length(li[[3]][[1]], 2)
})


test_that("named_to_dl works", {
  dl <- named_to_dl(list(a = "1", b = "2"))
  expect_identical(dl[[1]], "dl")
  expect_length(dl[[3]][[1]], 4)
})
