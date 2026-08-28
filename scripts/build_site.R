# Build the static site that GitHub Pages publishes.
# Add one shinylive::export() call here for each future Shiny app.

if (dir.exists("site")) {
  unlink("site", recursive = TRUE)
}

dir.create("site")
file.copy(
  from = list.files("site-source", full.names = TRUE),
  to = "site",
  recursive = TRUE
)

shinylive::export(
  appdir = "apps/mean-se",
  destdir = "site",
  subdir = "mean-se"
)
