# test_that("mod_exp works", {
#   txts <- get0("txts", envir = asNamespace("gretan"))
#   cb_ext <- get0("cb_ext", envir = asNamespace("gretan"))
# 
#   testServer(mod_exp_server, {
#     session$setInputs(
#       title = "Energy behavior",
#       subitem = "Unplug electronic devices that are not being used",
#       option = "Never",
#       pal = "Blues",
#       aggr = "nuts0",
#       fixed = "Full contrast",
#       mode = FALSE
#     )
#     browser()
# 
#     expect_identical(invar(), "c6_1_never")
#     expect_identical(pal(), "Blues")
#     output$explorer
# 
# 
#   }, args = list(id = "main-exp", track = FALSE))
# })
