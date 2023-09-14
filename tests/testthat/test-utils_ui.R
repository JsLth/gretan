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

test_that("Test columns wrappers works", {
  expect_s3_class(col_12(), "shiny.tag")
  expect_s3_class(col_10(), "shiny.tag")
  expect_s3_class(col_8(), "shiny.tag")
  expect_s3_class(col_6(), "shiny.tag")
  expect_s3_class(col_4(), "shiny.tag")
  expect_s3_class(col_3(), "shiny.tag")
  expect_s3_class(col_2(), "shiny.tag")
  expect_s3_class(col_1(), "shiny.tag")

  expect_equal(as.character(col_12()), '<div class="col-sm-12"></div>')
  expect_equal(as.character(col_10()), '<div class="col-sm-10"></div>')
  expect_equal(as.character(col_8()), '<div class="col-sm-8"></div>')
  expect_equal(as.character(col_6()), '<div class="col-sm-6"></div>')
  expect_equal(as.character(col_4()), '<div class="col-sm-4"></div>')
  expect_equal(as.character(col_3()), '<div class="col-sm-3"></div>')
  expect_equal(as.character(col_2()), '<div class="col-sm-2"></div>')
  expect_equal(as.character(col_1()), '<div class="col-sm-1"></div>')
})
