# episodes 0.1.0

* Initial CRAN submission
* Core functionality for time series data segmentation:
  - `segment_episodes()` to identify episodes based on temporal gaps
  - `segment_episodes_by_covars()` to track variable changes within episodes
  - `split_episode()` to analyze retention across time thresholds
* Includes sample dataset `substance_use` for demonstration
* Includes a vignette for how to use this package for survival analyses using 'tidymodels' (`vignette("survival", package = "episodes")`)
* Designed for analyzing treatment patterns, patient journeys, and other longitudinal data
