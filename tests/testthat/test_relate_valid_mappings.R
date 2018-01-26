library(relatable)
context("Valid mappings with relate")

test_that("relate outputs correct vectors for one-to-one relations",	{
  A1 <- c("a", "b", "c")
  B1 <- c(1, 2, 3)
  B2 <- list("apple", c("banana", "berry"), list("cherry", "coconut"))
  expect_identical(relate('a', A1, B1),
    1)
  expect_identical(relate(c("a", "b"), A1, B1),
    c(1, 2))
  expect_identical(relate(c("a", "b"), A1, B1, atomic = FALSE),
    list(1, 2))
  expect_identical(relate(c("a", "b"), A1, B1, named = TRUE),
    c(a = 1, b = 2))
  expect_identical(relate(c("a", "b", "c"), A1, B2, atomic = FALSE),
    list("apple", c("banana", "berry"), list("cherry", "coconut")))
})

test_that("relate outputs correct vectors for non-one-to-one relations",	{
  A1 <- c("a", "b", "c", "d")
  A2 <- c("a", "a", "b", "c")
  A3 <- c("a", "b", "b", "c", "d")
  B1 <- c(1, 2, 3, 4)
  B2 <- c(1, 2, 2, 3)
  B3 <- c(1, 2, 3, 4, 4)
  expect_identical(relate(c("a", "b", "c"), A1, B2,
    relation_type = "many_to_one"),
    c(1, 2, 2))
  expect_identical(relate(c("a", "b", "c"), A1, B2,
    relation_type = NULL,
    restrictions = list(max_one_y_per_x = TRUE)),
    c(1, 2, 2))
  expect_identical(relate(c("a", "b", "c"), A2, B1,
    relation_type = "one_to_many",
    atomic = FALSE),
    list(c(1, 2), 3, 4))
  expect_identical(relate(c("a", "b", "c"), A2, B1,
    relation_type = NULL,
    restrictions = list(max_one_x_per_y = TRUE),
    atomic = FALSE),
    list(c(1, 2), 3, 4))
  expect_identical(relate(c("a", "b", "c", "d"), A3, B3,
    relation_type = "many_to_many",
    named = TRUE,
    atomic = FALSE),
    list(a = 1, b = c(2, 3), c = 4, d = 4))
})

# test_that("relate outputs default values as required",	{
#   A1 <- c("a", "b", "c", "d")
#   A2 <- c("a", "b", "c")
#   B1 <- c(1, 2, 3, 4)
#   B2 <- c(1, 2, 3)
#   expect_identical(relate(c("e"), A1, B1),
#     NA)
#   expect_identical(relate(c("a", "e"), A1, B1,
#     default = 0),
#     c(1, 0))
#   expect_identical(relate(c("a", "d"), A1, B2,
#     default = 0),
#     c(1, 0))
# })
