% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track_norm.R
\name{norm_track_generic}
\alias{norm_track_generic}
\title{Generic Formant Track Normalization Procedure}
\usage{
norm_track_generic(
  .data,
  ...,
  .token_id_col,
  .by = NULL,
  .by_formant = FALSE,
  .by_token = FALSE,
  .time_col = NULL,
  .L = 0,
  .S = 1/sqrt(2),
  .pre_trans = function(x) x,
  .post_trans = function(x) x,
  .order = 5,
  .return_dct = FALSE,
  .drop_orig = FALSE,
  .names = "{.formant}_n",
  .silent = FALSE,
  .call = caller_env()
)
}
\arguments{
\item{.data}{A data frame containing vowel formant data}

\item{...}{\code{\link[dplyr:dplyr_tidy_select]{<tidy-select>}} One or more unquoted
expressions separated by commas. These should target the vowel formant
data columns.}

\item{.token_id_col}{\code{\link[rlang:args_data_masking]{<data-masking>}} A column
that identifies token ids.}

\item{.by}{\code{\link[dplyr:dplyr_tidy_select]{<tidy-select>}} A selection of
columns to group by. Typically a column of speaker IDs.}

\item{.by_formant}{Whether or not the normalization method is formant
intrinsic.}

\item{.by_token}{Whether or not the normalization method is token
intrinsic}

\item{.time_col}{\code{\link[rlang:args_data_masking]{<data-masking>}} A time column.
(optional)}

\item{.L}{An expression defining the location parameter.
See Details for more information.}

\item{.S}{An expression defining the scale parameter.
See Details for more information.}

\item{.pre_trans}{A function to apply to formant values before normalization.}

\item{.post_trans}{A function to apply to formant values after normalization.}

\item{.order}{The number of DCT parameters to use.}

\item{.return_dct}{Whether or not the normalized DCT coefficients themselves
should be returned.}

\item{.drop_orig}{Should the originally targeted columns be dropped.}

\item{.names}{A \code{\link[glue:glue]{glue::glue()}} expression for naming the normalized
data columns. The \code{"{.formant}"} portion corresponds to the name of the original
formant columns.}

\item{.silent}{Whether or not the informational message should be printed.}

\item{.call}{Used for internal purposes.}
}
\value{
A data frame of normalized formant tracks.
}
\description{
Normalize formant tracks using Discrete Cosine Transform normalization
}
\details{
The following \verb{norm_track_*} procedures were built on top of
\code{norm_track_generic}.
\itemize{
\item \link{norm_track_lobanov}
\item \link{norm_track_nearey}
\item \link{norm_track_deltaF}
\item \link{norm_track_wattfab}
\item \link{norm_track_barkz}
}

This will normalize vowel formant tracks in the following steps:
\enumerate{
\item Any \code{.pre_trans} transformations will be applied to the formant data.
\item The Discrete Cosine Transform will be applied to the formant data.
\item Location \code{.L} and Scale \code{.S} expressions will be used to summarize the zero\if{html}{\out{<sup>}}th\if{html}{\out{</sup>}}
DCT coefficients.
\item These location and scale will be used to normalize the DCT coefficients.
\item If \code{.return_dct = TRUE}, these normalized DCT coefficients will be returned.
Otherwise, the Inverse Discrete Cosine Transform will be applied to the
normalized DCT coefficients.
\item Any \code{.post_trans} transformations will be applied.
}
\subsection{Location and Scale expressions}{

All normalization procedures built on \link{norm_track_generic} work by normalizing
DCT coefficients directly. If \eqn{F_k} is the k\if{html}{\out{<sup>}}th\if{html}{\out{</sup>}} DCT coefficient
the normalization procedure is

\deqn{
\hat{F}_k = \frac{F_k - L'}{\sqrt{2}S}
}
\deqn{
L' = \begin{cases}
   L & \text{for }k=0\\
   0 & \text{for }k>0
 \end{cases}
}

Rather than requiring users to remember to multiply expressions for \eqn{S}
by \eqn{\sqrt{2}}, this is done by \link{norm_track_generic} itself, to allow greater
parallelism with how \link{norm_generic} works.

\strong{Note}: If you want to scale values by a constant in the normalization,
you'll need to divide the constant by \code{sqrt(2)}. Post-normalization scaling
(e.g. re-scaling to formant-like values) is probably best handled with a
function passed to \code{.post_trans}.

The expressions for calculating \eqn{L} and \eqn{S} can be
passed to \code{.L} and \code{.S}, respectively. Available values for
these expressions are

\describe{
\item{\code{.formant}}{The original formant value}
\item{\code{.formant_num}}{The number of the formant. (e.g. 1 for F1, 2 for F2 etc)}
}

Along with any data columns from your original data.
}

\subsection{Identifying tokens}{

Track normalization requires identifying individual tokens, so there must be a column that
uniquely identifies (or, in combination with a \code{.by} grouping, uniquely
identifies) each individual token. This column should be passed to
\code{.token_id_col}.
}

\subsection{Order}{

The number of DCT coefficients used is defined by \code{.order}. The default
value is 5. Larger numbers will lead to less smoothing, and smaller numbers
will lead to more smoothing.
}
}
\examples{
library(tidynorm)
library(dplyr)
ggplot2_inst <- require(ggplot2)

track_subset <- speaker_tracks |>
  filter(
    .by = c(speaker, id),
    if_all(
      F1:F3,
      .fns = \(x) mean(is.finite(x)) > 0.9
    ),
    row_number() \%\% 2 == 1
  )

track_norm <- track_subset |>
  norm_track_generic(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .by_formant = TRUE,
    .L = median(.formant, na.rm = TRUE),
    .S = mad(.formant, na.rm = TRUE),
    .time_col = t,
    .drop_orig = TRUE,
    .names = "{.formant}_mad"
  )

if (ggplot2_inst) {
  track_norm |>
    ggplot(
      aes(F2_mad, F1_mad, color = speaker)
    ) +
    stat_density_2d(bins = 4) +
    scale_x_reverse() +
    scale_y_reverse() +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed()
}

# returning the DCT coefficients
track_norm_dct <- track_subset |>
  norm_track_generic(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .by_formant = TRUE,
    .L = median(.formant, na.rm = TRUE),
    .S = mad(.formant, na.rm = TRUE),
    .time_col = t,
    .drop_orig = TRUE,
    .return_dct = TRUE,
    .names = "{.formant}_mad"
  )

track_norm_means <- track_norm_dct |>
  summarise(
    .by = c(speaker, vowel, .param),
    across(
      ends_with("_mad"),
      mean
    )
  ) |>
  reframe_with_idct(
    ends_with("_mad"),
    .by = speaker,
    .token_id_col = vowel,
    .param_col = .param
  )


if (ggplot2_inst) {
  track_norm_means |>
    ggplot(
      aes(F2_mad, F1_mad, color = speaker)
    ) +
    geom_path(
      aes(
        group = interaction(speaker, vowel)
      )
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed()
}
}
