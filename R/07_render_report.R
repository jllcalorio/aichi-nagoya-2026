# Step 7: render the self-contained Quarto report -> output/ag2026_philippines_report.html
# Needs the Quarto CLI (bundled with Positron and RStudio).

qmd <- "report/ag2026_philippines_report.qmd"
cand <- c(Sys.which("quarto"),
          "C:/Program Files/Positron/resources/app/quarto/bin/quarto.exe",
          "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
          "/Applications/Positron.app/Contents/Resources/app/quarto/bin/quarto",
          "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto")
quarto <- cand[nzchar(cand) & file.exists(cand)][1]
if (is.na(quarto)) stop("Quarto CLI not found - install it from https://quarto.org")

status <- system2(quarto, c("render", shQuote(qmd), "--to", "html"))
if (status != 0) stop("quarto render failed")
out <- sub("\\.qmd$", ".html", qmd)
file.copy(out, "output/ag2026_philippines_report.html", overwrite = TRUE)
unlink(out)
message("Report: output/ag2026_philippines_report.html (",
        round(file.size("output/ag2026_philippines_report.html") / 1e6, 1), " MB, self-contained)")
