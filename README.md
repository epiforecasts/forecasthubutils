# forecasthubutils

> **Archived.** No longer maintained; last updated June 2021.

A small R package of utility functions for working with COVID-19 forecast hub data, such as aggregating daily truth data to weeks. The package metadata in `DESCRIPTION` was never filled in beyond the title.

- `R/utils.R`: `make_weekly()` aggregates daily incidence data to epidemiological weeks (dropping incomplete weeks by default), `latest_weekday()` finds the most recent given weekday, and `check_dir()` creates a directory if it is missing.
- `tests/testthat/test-make_weekly.R`: tests for `make_weekly()`.
