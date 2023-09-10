test_that("as_likert works", {
  expect_s3_class(as_likert(1:7), "ordered")
  expect_length(as_likert(1:7), 7)
  expect_equal(
    as_likert(1:3, c("Disagree", "Neutral", "Agree")),
    factor(
      c("Disagree", "Neutral", "Agree"),
      levels = c("Disagree", "Neutral", "Agree"),
      ordered = TRUE
    )
  )
  as_likert(1:3, c("Disagree", "Neutral", "Agree"))
})

test_that("align_dl works", {
  expect_length(align_dl(a = c(1, 2, 3), b = c(1, 2)), 3)
  expect_s3_class(align_dl(a = 1)[[1]], c("html", "character"))
})

test_that("align_td works", {
  expect_true(grepl("color: red", align_td("Sample", 5)[[1]], fixed = TRUE))
  expect_true(grepl("<b>", align_td("a", "b", bold = TRUE)[[1]], fixed = TRUE))
  expect_true(grepl("---", align_td("a", "b", char = "---")[[1]], fixed = TRUE))
  expect_true(grepl("N/A", align_td("a", "NA")[[1]], fixed = TRUE))
})

test_that("rlang_error_to_html works", {
  e <- tryCatch(expr = rlang::abort("test"), error = function(e) return(e))
  expect_snapshot(rlang_error_to_html(e))
})

test_that("with_eval_args works", {
  funfact <- function(expr) {
   expr <- deparse(substitute(expr))
   pframe <- parent.frame()
   function() eval(parse(text = expr), envir = pframe)
  }
  
  fun2 <- lapply(1:3, \(x) with_eval_args(testf(x)))
  x <- vapply(fun2, \(f) f(), FUN.VALUE = numeric(1))
  expect_equal(x, 1:3)
})

test_that("protect_html works", {
  expect_s3_class(protect_html(tags$br()), c("html", "character"))
  expect_s3_class(protect_html("<br>"), c("html", "character"))
})

test_that("riffle works", {
  expect_identical(
    riffle(c(1, 2, 3), c("a", "b", "c")),
    c("1", "a", "2", "b", "3", "c")
  )
  
  expect_identical(
    riffle(c(1,2,3), c("a", "b", "c", "d", "f")),
    c("1", "a", "2", "b", "3", "c", "d", "f")
  )
})

test_that("not_in works", {
  expect_true(1 %not_in% 2:10)
  expect_false(1 %not_in% 1:10)
})

test_that("not_null works", {
  expect_true(not_null(1))
  expect_false(not_null(NULL))
})

test_that("not_na works", {
  expect_true(not_na(1))
  expect_false(not_na(NA))
})

test_that("drop_nulls works", {
  expect_equal(
    drop_nulls(
      list(x = NULL, y = 2)
    ),
    list(y = 2)
  )
})

test_that("%||% works", {
  expect_equal(
    NULL %||% 1,
    1
  )
  expect_equal(
    2 %||% 1,
    2
  )
})

test_that("%|NA|% works", {
  expect_equal(
    NA %|NA|% 1,
    1
  )
  expect_equal(
    2 %|NA|% 1,
    2
  )
})
