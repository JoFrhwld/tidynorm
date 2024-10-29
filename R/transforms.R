#' Hz to Bark
#' Converts Hz to Bark
#' @param hz Frequency in Hz
#'
#' @details
#' \deqn{
#' \hat{b} = \frac{26.81 hz}{1960 + hz} - 0.53
#'}
#'\deqn{
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
#' 88(1), 97–100.
#' [https://doi.org/10.1121/1.399849](https://doi.org/10.1121/1.399849)
#'
#' @examples
#' hz <- seq(150, 2000, length = 100)
#' bark <- hz_to_bark(hz)
#' plot(hz, bark)
#'
#' @export
hz_to_bark <- function(hz){
  bark <- ((26.81 * hz) / (1960 + hz))-0.53

  bark <- dplyr::case_when(
    bark < 2    ~ bark + ((0.15)*(2-bark)),
    bark > 20.1 ~ bark + (0.22 * (bark - 20.1)),
    .default = bark
  )

  return(bark)
}

#' Bark to Hz
#' Converts bark to Hz
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
#' #' @references
#' Traunmüller, H. (1990). Analytical expressions for the tonotopic
#' sensory scale. The Journal of the Acoustical Society of America,
#' 88(1), 97–100.
#' [https://doi.org/10.1121/1.399849](https://doi.org/10.1121/1.399849)
#'
#' @examples
#' bark <- seq(1.5, 13, length = 100)
#' hz <- bark_to_hz(bark)
#' plot(bark, hz)
#' @export
bark_to_hz <- function(bark){
  bark <- dplyr::case_when(
    bark < 2 ~ (bark-0.3)/0.85,
    bark > 20.1 ~ (bark + 4.422)/1.22,
    .default = bark
  )
  hz <- 1960 * ((bark + 0.53)/(25.28-bark))
  return(hz)
}

