# Raleigh DNA Methylation Classifier (V4)

> V4 is a redesign of [V3](https://github.com/NefiXa579/Raleigh_DNAMethylation_Classifier_V3).
> The analysis pipeline is unchanged; the interface has been reworked.

An R/Shiny web application for **meningioma DNA-methylation classification** from
Illumina IDAT files. It detects the chip type (HM450 / EPIC / EPICv2), runs the
SVM classifiers, produces a copy-number variation (CNV) plot per sample, and
supports batch processing of multiple IDAT pairs at once.

This is a re-build of the classifier originally developed by **William C. Chen
(UCSF)**, with a modernised interface, multi-sample batch processing, an
interactive CNV viewer, and an optimised processing pipeline.

> **Research use only.** This tool is **not clinically validated** and is not
> HIPAA compliant. Any clinical interpretation is the sole responsibility of the
> treating physician. See the in-app **Disclaimer** tab for full terms.

---

## How to run it

### Windows (easiest)
1. Make sure **R 4.5.x** is installed.
2. Double-click **`run_app.bat`**.
3. Wait ~1 minute — your browser opens automatically at
   **http://localhost:7771**. Keep the console window open while using the app;
   close it to stop the app.

> If R is installed somewhere other than `C:\Program Files\R\R-4.5.2\`, edit the
> `Rscript.exe` path near the bottom of `run_app.bat`.

### Any platform (command line)
```bash
Rscript launch.R
```
The launcher is self-locating, so it can be run from any directory.

---

## First-time setup (R packages)

The app uses CRAN and Bioconductor packages:

```r
install.packages(c("shiny", "bslib", "shinyjs", "caret", "kernlab",
                   "DNAcopy", "ggplot2", "plotly", "scales", "BiocManager"))
BiocManager::install(c("sesame", "sesameData", "GenomicRanges", "IRanges"))
```

On first run, `sesame` downloads its reference data via `sesameDataCache()`
(one time per machine).

---

## Project structure

```
.
├── app.R                 # entry point (sources global/ui/server)
├── global.R              # libraries, models, CNV pipeline, helpers
├── ui.R                  # interface (Classifier / Disclaimer / About)
├── server.R             # processing logic + per-sample result blocks
├── launch.R             # portable launcher (port 7771)
├── run_app.bat          # one-click Windows launcher
├── models/              # SVM classifiers + probe lists (loaded at startup)
└── www/                 # CSS, logo, reference figure
```

---

## Reference

Choudhury A. *et al.* **Meningioma DNA methylation groups identify biological
drivers and therapeutic vulnerabilities.** *Nature Genetics*, 2022.
<https://doi.org/10.1101/2020.11.23.20237495>
