#' Simulated substance use treatment data
#'
#' A dataset containing longitudinal observations of clients receiving treatment
#' for substance use disorders. This simulated dataset reflects realistic
#' patterns of treatment engagement, substance use, quality of life measures,
#' and medication treatment patterns for clients with alcohol or opioid use disorders.
#'
#' @format A tibble with 15,779 rows and 12 variables:
#' \describe{
#'   \item{client_id}{Character. Unique identifier for each client}
#'   \item{visit_date}{Date. Date of treatment visit}
#'   \item{substance_use_past_week}{Numeric. Number of days (0-7) client used substances in past week}
#'   \item{quality_of_life_score}{Numeric. Self-reported quality of life score (0-100)}
#'   \item{medication_dose_mg}{Numeric. Medication dose in milligrams, if applicable}
#'   \item{medication_name}{Character. Name of medication prescribed, if applicable}
#'   \item{Age}{Integer. Client age in years}
#'   \item{Gender}{Factor with levels: Male, Female, Non-Binary}
#'   \item{Race}{Factor with 8 levels including White, Hispanic, Black, etc.}
#'   \item{primary_substance}{Factor with levels: Alcohol, Opioids}
#'   \item{baseline_housing}{Factor with levels: Stable, Temporary, Unstable, Unhoused}
#'   \item{trauma_history}{Logical. Whether client has reported history of trauma}
#' }
#'
#' @details
#' The data simulates different patterns of treatment engagement including:
#' regular attendance, episodic treatment with gaps, and sporadic attendance.
#' Treatment outcomes vary with engagement patterns, which are distributed across
#' three types: improving, declining, and fluctuating.
#'
#' Medication information reflects evidence-based medications for opioid use disorder
#' (buprenorphine, methadone, naltrexone) and alcohol use disorder (naltrexone,
#' acamprosate, disulfiram) with realistic dosing patterns.
#'
#' @source
#' Simulated data created using the wakefield package with parameters designed
#' to reflect realistic patterns observed in substance use treatment programs.
#'
#' @examples
#' summary(substance_use)
"substance_use"
