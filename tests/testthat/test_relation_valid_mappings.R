library(relatable)
context("Valid mappings with relation")

test_that("relation outputs correct vectors for one-to-one relations",	{
  A1 <- c("a", "b", "c")
  B1 <- c(1, 2, 3)
  B2 <- list("apple", c("banana", "berry"), list("cherry", "coconut"))
  expect_identical(relation(A1, B1)('a'),
    1)
  expect_identical(relation(A1, B1)(c("a", "b")),
    c(1, 2))
  expect_identical(relation(A1, B1, atomic = FALSE)(c("a", "b")),
    list(1, 2))
  expect_identical(relation(A1, B1, named = TRUE)(c("a", "b")),
    c(a = 1, b = 2))
  expect_identical(relation(A1, B2, atomic = FALSE)(c("a", "b", "c")),
    list("apple", c("banana", "berry"), list("cherry", "coconut")))
})

test_that("relation outputs correct vectors for non-one-to-one relations",	{
  A1 <- c("a", "b", "c", "d")
  A2 <- c("a", "a", "b", "c")
  A3 <- c("a", "b", "b", "c", "d")
  B1 <- c(1, 2, 3, 4)
  B2 <- c(1, 2, 2, 3)
  B3 <- c(1, 2, 3, 4, 4)
  expect_identical(relation(A1, B2,
    relation_type = "many_to_one")(c("a", "b", "c")),
    c(1, 2, 2))
  expect_identical(relation(A1, B2,
    relation_type = NULL,
    restrictions = list(max_one_y_per_x = TRUE))(c("a", "b", "c")),
    c(1, 2, 2))
  expect_identical(relation(A2, B1,
    relation_type = "one_to_many",
    atomic = FALSE)(c("a", "b", "c")),
    list(c(1, 2), 3, 4))
  expect_identical(relation(A2, B1,
    relation_type = NULL,
    restrictions = list(max_one_x_per_y = TRUE),
    atomic = FALSE)(c("a", "b", "c")),
    list(c(1, 2), 3, 4))
  expect_identical(relation(A3, B3,
    relation_type = "many_to_many",
    named = TRUE,
    atomic = FALSE)(c("a", "b", "c", "d")),
    list(a = 1, b = c(2, 3), c = 4, d = 4))
})
