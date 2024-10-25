#' Speaker Data
#' @format ## `speaker_data`
#' A data frame with 10,697 rows and 8 columns
#' \describe{
#'   \item{speaker}{Speaker ID column}
#'   \item{vowel}{CMU Dictionary vowel class}
#'   \item{plt_vclass}{Modified Labov-Trager vowel class}
#'   \item{ipa_vclas}{IPA-like vowel class}
#'   \item{word}{Word that the vowel appeared in}
#'   \item{F1, F2, F3}{The first, second and third formants, in Hz}
#' }
"speaker_data"

#' Speaker Tracks
#' @format ## `speaker_tracks`
#' A data frame with 20,000 rows and 9 columns
#' \describe{
#'   \item{speaker}{Speaker ID column}
#'   \item{id}{Within speaker id for each token}
#'   \item{vowel}{CMU Dictionary vowel class}
#'   \item{plt_vclass}{Modified Labov-Trager vowel class}
#'   \item{ipa_vclas}{IPA-like vowel class}
#'   \item{word}{Word that the vowel appeared in}
#'   \item{t}{Measurement time point}
#'   \item{F1, F2, F3}{The first, second and third formants, in Hz}
#' }
"speaker_tracks"
