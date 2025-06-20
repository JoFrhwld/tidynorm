% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/norm.R
\name{norm_nearey}
\alias{norm_nearey}
\title{Nearey Normalize}
\usage{
norm_nearey(
  .data,
  ...,
  .by = NULL,
  .by_formant = FALSE,
  .drop_orig = FALSE,
  .keep_params = FALSE,
  .names = "{.formant}_lm",
  .silent = FALSE
)
}
\arguments{
\item{.data}{A data frame containing vowel formant data}

\item{...}{\code{\link[dplyr:dplyr_tidy_select]{<tidy-select>}} One or more unquoted
expressions separated by commas. These should target the vowel formant
data columns.}

\item{.by}{\code{\link[dplyr:dplyr_tidy_select]{<tidy-select>}} A selection of
columns to group by. Typically a column of speaker IDs.}

\item{.by_formant}{Whether or not the normalization method is formant
intrinsic.}

\item{.drop_orig}{Whether or not to drop the original formant data columns.}

\item{.keep_params}{Whether or not to keep the Location (\verb{*_.L}) and Scale
(\verb{*_.S}) normalization parameters}

\item{.names}{A \code{\link[glue:glue]{glue::glue()}} expression for naming the normalized
data columns. The \code{"{.formant}"} portion corresponds to the name of the original
formant columns.}

\item{.silent}{Whether or not the informational message should be printed.}
}
\value{
A data frame of Nearey normalized formant values.
}
\description{
Nearey Normalize
}
\details{
When formant extrinsic:
\deqn{
 \hat{F}_{ij} = \log(F_{ij}) - L
}
\deqn{
 L = \frac{1}{MN}\sum_{i=1}^M\sum_{j=1}^N \log(F_{ij})
}

When formant intrinsic:
\deqn{
 \hat{F}_{ij} = \log(F_{ij}) - L_{i}
}

\deqn{
  L_i = \frac{1}{N}\sum_{j=1}^{N}\log(F_{ij})
}

Where
\itemize{
\item \eqn{\hat{F}} is the normalized formant
\item \eqn{i} is the formant number
\item \eqn{j} is the token number
}
}
\examples{
library(tidynorm)
ggplot2_inst <- require(ggplot2)

speaker_data_nearey <- speaker_data |>
  norm_nearey(
    F1:F3,
    .by = speaker,
    .by_formant = FALSE,
    .names = "{.formant}_nearey"
  )

if (ggplot2_inst) {
  ggplot(
    speaker_data_nearey,
    aes(
      F2_nearey,
      F1_nearey,
      color = speaker
    )
  ) +
    stat_density_2d(
      bins = 4
    ) +
    scale_color_brewer(
      palette = "Dark2"
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    coord_fixed() +
    labs(
      title = "Formant extrinsic"
    )
}

speaker_data_nearey2 <- speaker_data |>
  norm_nearey(
    F1:F3,
    .by = speaker,
    .by_formant = TRUE,
    .names = "{.formant}_nearey"
  )

if (ggplot2_inst) {
  ggplot(
    speaker_data_nearey2,
    aes(
      F2_nearey,
      F1_nearey,
      color = speaker
    )
  ) +
    stat_density_2d(
      bins = 4
    ) +
    scale_color_brewer(
      palette = "Dark2"
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    coord_fixed() +
    labs(
      title = "Formant intrinsic"
    )
}
}
\references{
Nearey, T. M. (1978). Phonetic Feature Systems for Vowels [Ph.D.].
University of Alberta.
}
