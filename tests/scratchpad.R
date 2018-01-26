A1 <- c("a", "a", "b")
B1 <- c(1, 1, 2)

A <- as.list(A1); B <- as.list(B1)

# precise_properties <- list(
#   "min_one_y_per_x" = FALSE,
#   "min_one_x_per_y" = FALSE,
#   "max_one_y_per_x" = TRUE,
#   "max_one_x_per_y" = TRUE
# )
# c <- 0; d <- 0
# while (c %in% A) {
#   c <- c + 1
# }
# while (d %in% B) {
#   d <- d + 1
# }
# # Compute the image of relation R(A) and image of inverse relation R_inv(B)
# im <- relate(A, A, B, default = d, atomic = FALSE, relation_type = NULL, heterogeneous_outputs = TRUE)
# inv_im <- relate(B, B, A, default = c, atomic = FALSE, relation_type = NULL, named = F)
#
# for (i in seq_len(min(length(A), length(B)))) {
#   print(i)
#   print(unique(im[[i]]))
#   print(B[i])
#   if (compare::compareCoerce(B[i], unique(im[[i]]))$result == FALSE) {
#     precise_properties$max_one_y_per_x = FALSE
#     break
#   }
# }
#
# precise_properties

R <- relation(A1, B1, atomic = TRUE, relation_type = "bijection", handle_duplicate_mappings = TRUE)
R('b')

# BACKUP
#
# # Check if max_one_y_per_x does not hold
# for (i in seq_len(min(length(A), length(B)))) {
#   if (compare::compareCoerce(B[i], unique(im[i]))$result == FALSE) {
#     precise_properties$max_one_y_per_x = FALSE
#     break
#   }
# }
# # Check if max_one_x_per_y does not hold
# for (i in seq_len(min(length(A), length(B)))) {
#   if (compare::compareCoerce(A[i], unique(inv_im[i]))$result == FALSE) {
#     precise_properties$max_one_x_per_y = FALSE
#     break
#   }
# }