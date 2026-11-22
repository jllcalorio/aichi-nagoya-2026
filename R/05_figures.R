# Step 5: figures -> output/figures/*.png (600 DPI) and output/downloads/ag2026_figures_600dpi.zip

source("R/00_helpers.R")
source("R/figures_lib.R")
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/downloads", recursive = TRUE, showWarnings = FALSE)
unlink(list.files("output/figures", full.names = TRUE))

m <- readRDS("output/metrics.rds")
figs <- make_figures(m)

files <- character()
for (i in seq_along(figs)) {
  f <- figs[[i]]
  fn <- file.path("output/figures", sprintf("figure_%02d_%s.png", i, names(figs)[i]))
  ggsave(fn, f$plot + labs(title = paste0("Figure ", i, ". ", f$title)),
         width = f$w, height = f$h, dpi = 600, bg = SURF)
  files <- c(files, fn)
}
zip_path <- "output/downloads/ag2026_figures_600dpi.zip"
unlink(zip_path)
zip::zip(file.path(normalizePath("output/downloads"), basename(zip_path)), files = basename(files), root = "output/figures")
message("figures written to output/figures/ and ", zip_path)
