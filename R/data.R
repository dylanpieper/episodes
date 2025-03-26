#' Substance Use Treatment Dataset
#'
#' A simulated dataset containing information about substance use treatment clients,
#' their visits, medication, outcomes, and demographics.
#'
#' @format A tibble with 16,823 rows and 13 variables:
#' \describe{
#'   \item{client_id}{Integer ID for each client}
#'   \item{visit_date}{Date of clinical visit}
#'   \item{substance}{Factor with substance types (e.g., Alcohol, Opiates)}
#'   \item{medication}{Factor with medication types (e.g., Acamprosate, Naltrexone)}
#'   \item{frequency_of_use}{Ordered factor of use frequency (Monthly, Several times per week, Daily)}
#'   \item{age}{Numeric client age}
#'   \item{gender}{Factor of client gender}
#'   \item{race}{Factor of client race}
#'   \item{housing}{Ordered factor of housing status (Unhoused, Unstable, Stable)}
#'   \item{mental_health_disorder}{Logical indicating presence of mental health disorder}
#'   \item{discontinued}{Logical indicating if treatment was discontinued}
#'   \item{died_ever}{Logical indicating if client died at any point in study}
#'   \item{died_last_visit}{Logical indicating if client died after their last recorded visit}
#' }
#'
#' @source
#' Simulated data created to reflect realistic patterns observed in substance use treatment programs.
#'
#' @examples
#' summary(substance_use)
"substance_use"
