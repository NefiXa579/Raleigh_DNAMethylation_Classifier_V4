## ui.R

ui <- page(
  title = "Menin Methyl Classifier V3",
  theme = bs_theme(
    version     = 5,
    bootswatch  = "flatly",
    primary     = "#1b2435",
    secondary   = "#a9791f",
    bg          = "#faf6ee",
    fg          = "#1f2430",
    font_scale  = 0.95,
    base_font   = font_google("Inter")
  ),

  useShinyjs(),
  ## Version the stylesheet by its modify-time so CSS edits always bypass the
  ## browser cache (otherwise a stale custom.css can hide style changes).
  tags$head(tags$link(
    rel  = "stylesheet",
    href = paste0("custom.css?v=",
                  tryCatch(as.integer(file.mtime(file.path("www", "custom.css"))),
                           error = function(e) "1"))
  )),
  tags$head(tags$script(
    src = paste0("custom.js?v=",
                 tryCatch(as.integer(file.mtime(file.path("www", "custom.js"))),
                          error = function(e) "1"))
  )),

  ## ── Top bar — logo + title, full width ─────────────────────────────────────
  tags$div(class = "app-topbar",
    span(
      img(src = "logo.png", height = "30px",
          style = "margin-right:10px; vertical-align:middle; border-radius:6px;"),
      "Meningioma Methylation Classifier"
    )
  ),

  ## ── Sidebar nav + content ────────────────────────────────────────────────────
  tags$div(class = "container-fluid app-shell",

    navset_pill_list(
      widths = c(2, 10),

      ## ── Classifier tab ──────────────────────────────────────────────────────
      nav_panel(
        "Classifier", icon = icon("chart-column"),

        ## Hero intro banner — includes the upload/run controls
        tags$div(class = "hero-banner",
          tags$div(class = "hero-eyebrow", "UCSF · BRAIN TUMOR CENTER · RALEIGH LAB"),
          tags$h2("Meningioma Methylation Classifier"),
          tags$p(class = "hero-sub",
            "Upload IDAT files to predict meningioma methylation class and estimate copy-number variation."),

          tags$div(class = "hero-action-bar",
            tags$div(class = "hero-pill-col",
              tags$div(class = "hero-idat-pill",
                icon("magnifying-glass", class = "hero-idat-pill-icon"),
                fileInput("idat_files",
                          label    = NULL,
                          multiple = TRUE,
                          accept   = ".idat",
                          width    = "100%")
              ),

              ## Validation message + progress steps — hidden until relevant
              tags$div(class = "hero-status",
                uiOutput("engine_status_ui"),
                uiOutput("pair_validation_ui"),
                uiOutput("progress_ui")
              )
            ),
            actionButton("go_btn", "Run Classifier",
                         icon  = icon("play"),
                         class = "btn btn-primary hero-run-btn")
          )
        ),

        ## Results — full width below the banner
        layout_columns(
          col_widths = 12,

          card(
            card_header(
              icon("table-list"), " Results Summary",
              downloadButton("dl_csv", "Download CSV",
                             class = "btn btn-sm btn-outline-navy float-end")
            ),
            card_body(uiOutput("results_table_ui"))
          ),

          ## One block per sample: CNV plot + survival curves side by side
          uiOutput("sample_blocks_ui")
        )
      ),

      ## ── Disclaimer tab ──────────────────────────────────────────────────────
      nav_panel(
        "Disclaimer", icon = icon("triangle-exclamation"),

        layout_columns(
          col_widths = c(10, 2),   # centre the content
          offset = 1,

          card(
            card_header(icon("triangle-exclamation"), " Disclaimer & Terms of Use"),
            card_body(

              ## Acceptance banner
              tags$div(class = "disclaimer-banner",
                icon("circle-exclamation"),
                tags$strong(" By using this tool you accept these terms. If you do not accept them, please close this page.")
              ),

              br(),

              ## Section 1
              tags$div(class = "disclaimer-section",
                tags$h6(icon("scale-balanced"), "  License"),
                tags$p("Distributed under ",
                  tags$a("CC BY-NC 3.0", href="https://creativecommons.org/licenses/by-nc/3.0/", target="_blank"),
                  ": you may copy, redistribute, and adapt this work for ",
                  tags$strong("non-commercial research purposes only"),
                  ", with attribution to the original authors.")
              ),

              ## Section 3
              tags$div(class = "disclaimer-section",
                tags$h6(icon("hospital"), "  Clinical Use — Important"),
                tags$p("This tool is ", tags$strong("not prospectively clinically validated"), " and is a research instrument under development.
                  It was developed on N=565 meningioma samples from two retrospective international cohorts.
                  Any clinical application is the ", tags$strong("sole responsibility"),
                  " of the treating physician. This tool is ", tags$strong("not HIPAA compliant"), ".")
              ),

              ## Section 4
              tags$div(class = "disclaimer-section",
                tags$h6(icon("microchip"), "  Version 2 — Chip Compatibility (updated 1/26/24)"),
                tags$p("The original 2022 publication used only the ", tags$strong("EPIC chip"),
                  ". Version 2 extends compatibility to the 450K and EPICv2 chips using re-trained SVM models:"),
                tags$ul(
                  tags$li(tags$strong("450K chip:"), " 1,242 / 2,000 probes used — 100% concordance with EPIC model"),
                  tags$li(tags$strong("EPICv2 chip:"), " 1,184 / 2,000 probes used — >98% concordance with EPIC model")
                ),
                tags$p(class="text-muted small", "Re-trained on N=200 UCSF samples, tested on N=365 UHK samples.
                  These extended results are unpublished. Use at your own discretion.")
              )
            )
          ),

          tags$div()   # empty right gutter
        )
      ),

      ## ── Methods tab ─────────────────────────────────────────────────────────────
      nav_panel(
        "Methods", icon = icon("file-lines"),

        layout_columns(
          col_widths = c(10, 2), offset = 1,

          card(
            card_header(icon("gears"), " Methods — How the Output Is Produced"),
            card_body(

              tags$p(class = "text-muted section-intro",
                "Each uploaded sample passes through the pipeline below. Preprocessing and
                 copy-number analysis use the ", tags$em("sesame"), " Bioconductor package;
                 the methylation classes follow the approach of Choudhury et al, Nature Genetics 2022."),

              ## Step 1
              tags$div(class = "disclaimer-section",
                tags$h6(icon("upload"), "  1. Upload & pairing"),
                tags$p("IDAT files are uploaded together. Green (", tags$code("_Grn.idat"),
                  ") and red (", tags$code("_Red.idat"),
                  ") files are matched into sample pairs automatically by filename.")
              ),

              ## Step 2
              tags$div(class = "disclaimer-section",
                tags$h6(icon("file-import"), "  2. Read raw signal"),
                tags$p(tags$code("readIDATpair"), " loads the raw red/green intensities for each pair
                  into a signal data frame.")
              ),

              ## Step 3
              tags$div(class = "disclaimer-section",
                tags$h6(icon("wand-magic-sparkles"), "  3. Preprocessing"),
                tags$p("Signals are corrected in three steps before any analysis:"),
                tags$ul(
                  tags$li(tags$strong("pOOBAH"), " — masks unreliable probes using detection p-values."),
                  tags$li(tags$strong("noob"), " — normal-exponential out-of-band background correction."),
                  tags$li(tags$strong("Dye-bias correction"), " — normalises the red/green dye channels.")
                ),
                tags$p("Corrected signals are converted to beta values (methylation fraction, 0–1).")
              ),

              ## Step 4
              tags$div(class = "disclaimer-section",
                tags$h6(icon("microchip"), "  4. Chip detection"),
                tags$p("The array type is detected automatically (450K, EPIC, or EPICv2), and the
                  matching probe set and trained model are selected for that platform.")
              ),

              ## Step 5
              tags$div(class = "disclaimer-section",
                tags$h6(icon("brain"), "  5. Methylation classification"),
                tags$p("A linear support-vector-machine (SVM) classifier assigns each sample to a
                  methylation class, reported in both the ", tags$strong("3-group"), " and ",
                  tags$strong("4-group"), " schemes (e.g. Merlin-intact, Immune-enriched,
                  Hypermitotic). Probes absent on the sample are set to zero before prediction.")
              ),

              ## Step 6
              tags$div(class = "disclaimer-section",
                tags$h6(icon("chart-area"), "  6. Copy-number variation (CNV)"),
                tags$p("Total intensities are compared against a panel of normal reference samples,
                  normalised with a linear model, and grouped into genomic bins. ",
                  tags$strong("Circular binary segmentation"), " (DNAcopy, ",
                  tags$code("nperm = 10,000"), ") then identifies copy-number segments, plotted as
                  genome-wide log2 ratios — gains, losses, and segment means by chromosome.")
              ),

              ## Step 7
              tags$div(class = "disclaimer-section",
                tags$h6(icon("table-list"), "  7. Output"),
                tags$p("Results are presented as a summary table (chip type, 3- and 4-group class),
                  an interactive copy-number plot per sample, and downloadable CSV (classes) and
                  PNG (CNV plot) files.")
              )
            )
          ),
          tags$div()
        )
      ),

      ## ── About tab ───────────────────────────────────────────────────────────────
      nav_panel(
        "About", icon = icon("circle-info"),
        layout_columns(
          col_widths = c(10, 2), offset = 1,
          card(
            card_header(icon("circle-info"), " About"),
            card_body(
              tags$p(class = "text-muted section-intro",
                tags$strong("Meningioma Methylation Classifier V3"),
                HTML(" — built on the classifier originally developed by <a href=\"https://radonc.ucsf.edu/about/our-team/william-c-chen-md/\" target=\"_blank\" style=\"font-weight:700;\">William C. Chen, MD</a>. This version adds multi-sample batch processing, an interactive CNV viewer, an optimised computational pipeline, and a modern interface.")
              ),

              tags$div(class = "disclaimer-section",
                tags$h6(icon("flask"), "  Scientific Basis"),
                tags$p("This classifier is based on research by the University of California, San Francisco (UCSF).
                  Full reference: ",
                  tags$a("Choudhury et al, Nature Genetics, January 2022",
                         href="https://doi.org/10.1101/2020.11.23.20237495", target="_blank"), "."),
                tags$div(class = "figure-frame",
                  img(src = "nomogram_static_paper_image.png"),
                  tags$p(class = "text-muted small fst-italic fig-cap",
                    "Nomogram and Kaplan-Meier curves for local freedom from recurrence.")
                )
              ),

              tags$div(class = "disclaimer-section",
                tags$h6(icon("lightbulb"), "  Development"),
                tags$p(HTML("This version (V3) was developed by <a href=\"https://www.linkedin.com/in/nefeli-chanoutsi/\" target=\"_blank\" style=\"font-weight:700;\">Nefeli Chanoutsi</a>, building on the original classifier by William C. Chen, MD.")),
                tags$p("Improvements in this version, measured against the original pipeline:"),
                tags$ul(
                  tags$li(tags$strong("Multi-sample batch processing — "),
                          "from one IDAT pair per run to an unlimited number of pairs in a single
                           upload, with automatic green/red pairing (previously a single pair, uploaded
                           as two separate files)."),
                  tags$li(tags$strong("~35–45% faster per sample — "),
                          "end-to-end processing reduced from roughly 56 s to about 33 s per sample."),
                  tags$li(tags$strong("Copy-number step up to ~3× faster — "),
                          "CNV analysis cut from ~37 s to ~19 s for the first sample and ~12 s for each
                           subsequent sample in a batch, by replacing per-element loops with vectorised
                           operations (removing ~21 s) and caching the platform-constant reference data
                           loaded once per session (~8 s saved per later sample).")
                )
              ),

              tags$div(class = "disclaimer-section",
                tags$h6(icon("envelope"), "  Contact"),
                tags$p("For questions about this tool, please contact:"),
                tags$p(
                  tags$strong("David Raleigh, MD PhD"), tags$br(),
                  "david.raleigh@ucsf.edu"
                )
              ),

              tags$div(class = "disclaimer-section",
                tags$h6(icon("link"), "  Original app"),
                tags$p(tags$a("william-c-chen.shinyapps.io/MeninMethylClass_V2_450K_added",
                              href="https://william-c-chen.shinyapps.io/MeninMethylClass_V2_450K_added/",
                              target="_blank"))
              )
            )
          ),
          tags$div()
        )
      ),

      ## ── Sidebar footer: partner logos (pinned to bottom via CSS) ───────────
      nav_item(
        tags$div(
          class = "sidebar-logos",
          tags$a(href = "https://braintumorcenter.ucsf.edu", target = "_blank", rel = "noopener",
                 class = "sidebar-logo-link sidebar-logo-link-btc",
                 img(src = "Brain_Tumor_Center_logo.svg", alt = "UCSF Brain Tumor Center",
                     class = "sidebar-logo")),
          tags$a(href = "https://raleighlab.ucsf.edu", target = "_blank", rel = "noopener",
                 class = "sidebar-logo-link sidebar-logo-link-raleigh",
                 img(src = "raleigh_lab_logo.png", alt = "Raleigh Lab",
                     class = "sidebar-logo"))
        )
      )
    )
  )
)
