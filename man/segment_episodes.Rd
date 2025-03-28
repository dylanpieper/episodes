% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/episodes.R
\name{segment_episodes}
\alias{segment_episodes}
\title{Segment time series data into episodes based on date gaps}
\usage{
segment_episodes(
  .data,
  date_col,
  max_date = NULL,
  gap_threshold = Inf,
  gap_unit = "days",
  inactive_threshold = gap_threshold,
  inactive_unit = gap_unit,
  episodes = "all",
  progress = TRUE
)
}
\arguments{
\item{.data}{A grouped data frame. Must be pre-grouped with \code{dplyr::group_by()}.}

\item{date_col}{Column containing date values to analyze.}

\item{max_date}{Optional reference date for determining active/inactive status.
Defaults to the maximum date in the dataset.}

\item{gap_threshold}{Numeric value specifying the gap size that defines a new episode.
Default is Inf.}

\item{gap_unit}{Units for the gap threshold. One of "days", "weeks", or "months".
Default is "days".}

\item{inactive_threshold}{Threshold for considering an episode inactive.
Default is the same as \code{gap_threshold}.}

\item{inactive_unit}{Units for the inactive threshold. One of "days", "weeks", or "months".
Default is the same as \code{gap_unit}.}

\item{episodes}{Which episodes to return. Options are "all", "first", "last", or a
specific episode number. Default is "all".}

\item{progress}{Logical. Display a progress bar while processing? Default is TRUE.}
}
\value{
A data frame with one row per episode containing:
\itemize{
\item \strong{episode_id}: Numeric episode identifier
\item \strong{episode_start}: Date when the episode started
\item \strong{episode_end}: Date when the episode ended
\item \strong{episode_days}: Duration of the episode in days
\item \strong{discontinued}: Logical indicating if the episode is discontinued
\item \strong{status}: Character indicating episode status ("Active", "Inactive", or "Gap")
\item \strong{dates_count}: Number of dates in the episode
\item \strong{dates_avg_days_between}: Average number of days between consecutive dates in the episode
\item \strong{dates_sd_days_between}: Standard deviation of days between consecutive dates
\item \strong{gap_days}: Number of days between this episode and the next one (NA for the last episode)
\item \strong{other variables}: Any fixed variables from the input data frame
}
}
\description{
\code{segment_episodes()} identifies episodes in temporal data by detecting gaps between
dates that exceed a specified threshold. An episode ends when the gap between
consecutive dates is larger than \code{gap_threshold}. Episodes are also classified as
'active' or 'inactive' based on recency.
}
\examples{
data(substance_use)
treatment_episodes <- substance_use |>
  group_by(client_id) |>
  segment_episodes(
    visit_date,
    gap_threshold = 2,
    gap_unit = "months"
  )

}
\seealso{
\code{\link{segment_episodes_by_covars}} for creating episode segments with covariate change tracking
}
