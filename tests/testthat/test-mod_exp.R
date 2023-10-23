test_that("mod_exp_ui works", {
  ui <- mod_exp_ui("main", "")
  expect_s3_class(ui, "shiny.tag")
  expect_contains(ui[[2]], list(role = "tabpanel", id = "shiny-tab-exp"))
})

test_that("mod_exp works", {
  txts <- get0("txts", envir = asNamespace("gretan"))
  cb_ext <- get0("cb_ext", envir = asNamespace("gretan"))

  testServer(mod_exp_server, {
    # Test defaults ----
    session$setInputs(title = "Energy behavior")
    session$setInputs(subitem = "Unplug electronic devices that are not being used")
    session$setInputs(option = "Never")
    session$setInputs(mode = FALSE)
    session$setInputs(aggr = "nuts0")
    session$setInputs(fixed = "Full contrast")
    session$setInputs(pal = "Blues")
    
    expect_identical(invar(), "c6_1_never")
    expect_identical(pal(), "Blues")
    
    params <- exp_params()
    
    # test params output
    expect_type(params, "list")
    expect_named(params, c("poly", "aggr", "invar", "pal", "values", "labels", "lgd",  "unit"))
    
    # test survey dataset
    expect_s3_class(params$poly, c("sf", "tbl_df", "tbl", "data.frame"))
    expect_named(params$poly, c("sample", "nuts0", "c6_1_never", "geometry"))
    expect_length(params$poly$sample, 16)
    expect_type(params$poly$c6_1_never, "double")
    
    # test aggreggation level
    expect_identical(params$aggr, "nuts0")
    
    # test formula
    expect_s3_class(params$values, "formula")
    
    # test labels
    expect_type(params$labels, "list")
    expect_length(params$labels, 16)
    expect_s3_class(params$labels[[1]], "html")
    
    # test extras
    expect_identical(params$lgd, "Share")
    expect_identical(params$unit, "%")
    
    
    # Test mode switch ----
    session$setInputs(mode = TRUE)
    
    params <- exp_params()
    expect_type(params$poly$c6_1_never, "character")
    expect_identical(params$lgd, "Mode")
    expect_identical(params$unit, "")
    expect_s3_class(params$values, "factor")
    expect_type(params$pal, "closure")
    
    
    # Test aggregation level ----
    session$setInputs(mode = FALSE)
    session$setInputs(aggr = "nuts1")

    params <- exp_params()
    expect_identical(params$aggr, "nuts1")
    expect_type(params$poly$c6_1_never, "double")
    expect_named(params$poly, c("sample", "nuts0", "nuts1", "c6_1_never", "geometry"))
    expect_length(params$poly$sample, 76)
    expect_length(params$labels, 76)
  }, args = list(id = "main-exp", track = FALSE))
})
