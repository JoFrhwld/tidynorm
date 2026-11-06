test_that(
  "Mahalanobis is calculated correctly",
  {
    sim_mat <- withr::with_seed(
      2026 - 11 - 04,
      {
        cbind(rnorm(100), rnorm(100)) %*% cbind(x = c(1, 0.3), y = c(0.3, 1))
      }
    )
    mu <- colMeans(sim_mat)
    Sigma <- cov(sim_mat)
    m <- mahalanobis(sim_mat, mu, Sigma)

    df <- tibble::as_tibble(sim_mat) |>
      dplyr::mutate(g = 1) |>
      identify_outliers(
        x:y,
        .by = g,
        .silent = T
      )

    expect_equal(
      df$.mahal, m
    )
  }
)

test_that(
  "DCT Mahalanobis",
  {
    n_param <- 5
    n_dim <- 2

    sim_vec <- withr::with_seed(
      2026 - 11 - 04,
      {
        rnorm(100 * n_param * n_dim)
      }
    )
    sim_mat <- matrix(sim_vec, ncol = n_param * n_dim)
    dim_names <- stringr::str_glue("F{1:n_dim}") |>
      purrr::map(\(x) stringr::str_glue("{x}_{1:n_param}")) |>
      unlist()
    colnames(sim_mat) <- dim_names

    mu <- colMeans(sim_mat)
    Sigma <- cov(sim_mat)

    m <- mahalanobis(sim_mat, mu, Sigma)

    sim_mahal <- tibble::as_tibble(
      sim_mat
    ) |>
      dplyr::mutate(
        token = dplyr::row_number(),
        group = 1
      ) |>
      tidyr::pivot_longer(
        -c(token, group)
      ) |>
      tidyr::separate_wider_delim(
        name,
        names = c("formant", "param"),
        delim = "_"
      ) |>
      tidyr::pivot_wider(
        names_from = "formant",
        values_from = value
      ) |>
      identify_dct_outliers(
        F1:F2,
        .token_id_col = token,
        .param_col = param,
        .by = group
      )

    sim_mahal |>
      dplyr::filter(param == 1) |>
      pull(.mahal) ->
    m2

    expect_equal(m, m2)
  }
)
