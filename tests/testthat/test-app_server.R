testServer(app_server, {
  session$setInputs(textSearch = "")

  expect_false(searchbox_input())
  expect_error(search_results(), class = "shiny.silent.error")

  session$setInputs(textSearch = "absolutely nonsensical search term")
  expect_true(searchbox_input())
  expect_s3_class(search_results(), "shiny.tag")
  expect_identical(tag_to_text(search_results()), "No results")

  session$setInputs(textSearch = "stakeholder")
  expect_true(searchbox_input())
  expect_s3_class(search_results(), "shiny.tag")
  expect_length(search_results()[[3]], 3)

  session$setInputs(textSearch = "stakeholder interactions")
  expect_true(searchbox_input())
  expect_s3_class(search_results(), "shiny.tag")
  expect_length(search_results()[[3]], 1)

  session$setInputs(sidebar = "home")
  expect_identical(tabsel(), "home")
})
