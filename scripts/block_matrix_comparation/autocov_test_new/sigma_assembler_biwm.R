#' @export
sigma_assembler_biwm <- function(sigmas, a, rho, nus, coords_matrix){

  d <- dist(coords_matrix)

  # Maybe remove since check is done elsewhere
  if(length(nus) == 2) nus[3] <- mean(nus[1:2])

  M_1 <- sigmas[1] * matern_cov_wrapper(d,
                                        a = a,
                                        nu = nus[1])

  M_2 <- sigmas[2] * matern_cov_wrapper(d,
                                        a = a,
                                        nu = nus[2])

  M_12 <- rho * sqrt(sigmas[1] * sigmas[2]) * matern_cov_wrapper(d,
                                                           a = a,
                                                           nu = nus[3])

  # ret <- matrices_assembler(M_1, M_2, M_12)
  ret <- list(C_11 = M_1,C_22 = M_2,C_12 = M_12)

  return(ret)
}
