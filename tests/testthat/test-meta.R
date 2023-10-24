test_that("ui works", {
  expect_s3_class(app_ui(), "shiny.tag.list")
})


test_that("app_sys works", {
  expect_true(dir.exists(app_sys("")))
  expect_true(file.exists(app_sys("DESCRIPTION")))
})


test_that("add_external_resources works", {
  expect_identical(add_external_resources()[[1]], "head")
})


test_that("run_app works", {
  skip_if_not(has_dependencies())

  expect_s3_class(run_app(), "shiny.appobj")

  app <- run_greta(console = TRUE, track = TRUE)
  expect_named(app$appOptions$greta_options, c("console", "track"))

  options(app.prod = TRUE)
  app <- run_greta()
  # expect_named(app$appOptions$greta_options, c("console", "track", "logging"))
  expect_false(app$appOptions$greta_options$console)
  options(app.prod = NULL)

  app <- run_greta(reactlog = TRUE)
  expect_true(getOption("shiny.reactlog"))
})
