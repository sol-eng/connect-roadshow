context("types-coercion")

test_that("no method dispatch", {
  as.logical.foo <- function(x) "wrong"
  expect_identical(as_integer(structure(TRUE, class = "foo")), 1L)

  as.list.foo <- function(x) "wrong"
  expect_identical(as_list(structure(1:10, class = "foo")), as.list(1:10))
})

test_that("input is left intact", {
  x <- structure(TRUE, class = "foo")
  y <- as_integer(x)
  expect_identical(x, structure(TRUE, class = "foo"))
})

test_that("as_list() zaps attributes", {
  expect_identical(as_list(structure(list(), class = "foo")), list())
})

test_that("as_list() only coerces vector or dictionary types", {
  expect_identical(as_list(1:3), list(1L, 2L, 3L))
  expect_error(as_list(quote(symbol)), "`symbol` to `list`")
})

test_that("as_list() bypasses environment method and leaves input intact", {
  as.list.foo <- function(x) "wrong"
  x <- structure(child_env(), class = "foo")
  y <- as_list(x)

  expect_is(x, "foo")
  expect_identical(y, set_names(list(), character(0)))
})

test_that("as_integer() and as_logical() require integerish input", {
  expect_error(as_integer(1.5), "fractional `double`")
  expect_error(as_logical(1.5), "fractional `double`")
})

test_that("names are preserved", {
  nms <- as.character(1:3)
  x <- set_names(1:3, nms)
  expect_identical(names(as_double(x)), nms)
  expect_identical(names(as_list(x)), nms)
})
