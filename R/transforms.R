#' Hz to Bark
#'
#' Converts Hz to Bark
#'
#' @param hz Frequency in Hz
#'
#' @details
#' \deqn{
#' \hat{b} = \frac{26.81 hz}{1960 + hz} - 0.53
#' }
#' \deqn{
#' b = \begin{cases}
#' \hat{b} + 0.15(2-\hat{b}) & \text{if}~\hat{b} < 2\\
#' \hat{b} + 0.22(\hat{b} - 20.1) & \text{if}~\hat{b} > 20.1\\
#' \hat{b} & \text{otherwise}
#' \end{cases}
#' }
#'
#' @returns
#' A vector of bark scaled values
#'
#' @references
#' Traunmüller, H. (1990). Analytical expressions for the tonotopic
#' sensory scale. The Journal of the Acoustical Society of America,
#' 88(1), 97–100. \doi{10.1121/1.399849}
#'
#' @examples
#' hz <- seq(150, 2000, length = 100)
#' bark <- hz_to_bark(hz)
#' plot(hz, bark)
#'
#' @export
hz_to_bark <- function(hz) {
  bark <- ((26.81 * hz) / (1960 + hz)) - 0.53

  bark <- dplyr::case_when(
    bark < 2 ~ bark + ((0.15) * (2 - bark)),
    bark > 20.1 ~ bark + (0.22 * (bark - 20.1)),
    .default = bark
  )

  return(bark)
}

#' Bark to Hz
#'
#' Converts bark to Hz
#'
#' @param bark Frequency in Bark
#' @details
#' \deqn{
#' \hat{b} = \begin{cases}
#' \frac{b - 0.3}{0.85} & \text{if} ~ b < 2\\
#' \frac{b + 4.422}{1.22} & \text{if} ~ b > 20.1\\
#' b & \text{otherwise}
#' \end{cases}
#' }
#'
#' \deqn{
#' hz = 1960\frac{\hat{b} + 0.53}{26.28 - \hat{b}}
#' }
#'
#' @returns
#' A vector of Hz scaled values
#'
#' @references
#' Traunmüller, H. (1990). Analytical expressions for the tonotopic
#' sensory scale. The Journal of the Acoustical Society of America,
#' 88(1), 97–100. \doi{10.1121/1.399849}
#'
#' @examples
#' bark <- seq(1.5, 13, length = 100)
#' hz <- bark_to_hz(bark)
#' plot(bark, hz)
#' @export
bark_to_hz <- function(bark) {
  bark <- dplyr::case_when(
    bark < 2 ~ (bark - 0.3) / 0.85,
    bark > 20.1 ~ (bark + 4.422) / 1.22,
    .default = bark
  )
  hz <- 1960 * ((bark + 0.53) / (25.28 - bark))
  return(hz)
}


#' Hz to Mel
#'
#' Convert Hz to Mel
#'
#' @param hz Numeric values in Hz
#' @param htk Whether or not to use the HTK formula
#'
#' @details
#' This is a direct re-implementation of the `hz_to_mel`
#' function from the [librosa](https://librosa.org/) library.
#'
#' The default method is to use the method due to Slaney (1998), which
#' is linear below 1000Hz, and logarithmic above.
#'
#' If `htk=TRUE`, the method from HTK, due to O'Shaughnessy (1987) is used.
#'
#' @returns A numeric vector of Mel values
#'
#' @references
#' McFee, B., C. Raffel, D. Liang, D. PW Ellis, M. McVicar, E. Battenberg,
#' and O. Nieto. librosa: Audio and music signal analysis in python.
#' In Proceedings of the 14th python in science conference, pp. 18-25.
#'
#' O'Shaughnessy, D (1987). Speech communication: human and machine.
#' Addison-Wesley. p. 150. ISBN 978-0-201-16520-3.
#'
#' Slaney, M. (1998) Auditory Toolbox: A MATLAB Toolbox for Auditory Modeling
#' Work. Technical Report, version 2, Interval Research Corporation.
#'
#' @examples
#' hz_to_mel(c(500, 1000, 2000, 3000))
#'
#' @export
hz_to_mel <- function(
    hz,
    htk = FALSE) {
  if (htk) {
    mels <- 2595.0 * log10(1.0 + (hz / 700.0))
    return(mels)
  }
  f_sp <- 200 / 3
  min_log_hz <- 1000.0
  min_log_mel <- min_log_hz / f_sp
  logstep <- log(6.4) / 27.0

  mels <- dplyr::case_when(
    hz >= 1000 ~ min_log_mel + (log(hz / min_log_hz) / logstep),
    .default = hz / f_sp
  )

  return(mels)
}

#' Mel to Hz
#'
#' Convert Mel to Hz
#'
#' @param mel Numeric values in Hz
#' @param htk Whether or not to use the HTK formula
#'
#' @details
#' This is a direct re-implementation of the `hz_to_mel`
#' function from the [librosa](https://librosa.org/) library.
#'
#' The default method is to use the method due to Slaney (1998), which
#' is linear below 1000Hz, and logarithmic above.
#'
#' If `htk=TRUE`, the method from HTK, due to O'Shaughnessy (1987) is used.
#'
#' @returns A numeric vector of Hz values
#'
#' @references
#' McFee, B., C. Raffel, D. Liang, D. PW Ellis, M. McVicar, E. Battenberg,
#' and O. Nieto. librosa: Audio and music signal analysis in python.
#' In Proceedings of the 14th python in science conference, pp. 18-25.
#'
#' O'Shaughnessy, D (1987). Speech communication: human and machine.
#' Addison-Wesley. p. 150. ISBN 978-0-201-16520-3.
#'
#' Slaney, M. (1998) Auditory Toolbox: A MATLAB Toolbox for Auditory Modeling
#' Work. Technical Report, version 2, Interval Research Corporation.
#'
#'
#' @examples
#' mel_to_hz(c(7.5, 15, 25, 31))
#'
#' @export
mel_to_hz <- function(
    mel,
    htk = FALSE) {
  if (htk) {
    hz <- 700.0 * (10.0^(mel / 2595.0) - 1.0)
    return(hz)
  }

  f_sp <- 200.0 / 3
  min_log_hz <- 1000.0
  min_log_mel <- min_log_hz / f_sp
  logstep <- log(6.4) / 27.0

  hz <- dplyr::case_when(
    mel >= min_log_mel ~ min_log_hz * exp(logstep * (mel - min_log_mel)),
    .default = f_sp * mel
  )

  return(hz)
}
