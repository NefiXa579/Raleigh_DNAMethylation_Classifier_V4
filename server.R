## server.R

server <- function(input, output, session) {

  results_list <- reactiveVal(list())
  step_status  <- reactiveVal(list())

  ## ── Analysis engine: deferred background loading ────────────────────────────
  ## The page is rendered first; the heavy libraries/models then load in chunks
  ## so the user sees the interface immediately instead of a blank screen.
  ## Only the first visitor pays this cost — it is shared across sessions.
  engine_state <- reactiveVal(
    if (engine_ready())
      list(done = engine_total(), total = engine_total(), label = "Ready", ready = TRUE)
    else
      list(done = 0L, total = engine_total(), label = "Starting", ready = FALSE)
  )

  ## Run stays disabled until the engine is ready
  if (!engine_ready()) shinyjs::disable("go_btn")

  ## onFlushed(once) fires *after* the first page has been sent to the browser,
  ## so this loading never delays the initial render.
  session$onFlushed(function() {
    observe({
      if (engine_ready()) {
        engine_state(list(done = engine_total(), total = engine_total(),
                          label = "Ready", ready = TRUE))
        shinyjs::enable("go_btn")
        return(invisible(NULL))   # no dependencies -> observer stops here
      }
      invalidateLater(120)
      isolate({
        engine_run_next()
        p <- engine_progress()
        engine_state(list(done = p$done, total = p$total,
                          label = p$label, ready = engine_ready()))
      })
    })
  }, once = TRUE)

  ## Status indicator — hidden once ready
  output$engine_status_ui <- renderUI({
    st <- engine_state()
    if (isTRUE(st$ready)) return(NULL)
    pct <- round(100 * st$done / max(st$total, 1))
    tags$div(
      class = "engine-status",
      tags$div(class = "progress", style = "height:6px; border-radius:4px;",
        tags$div(class = "progress-bar progress-bar-striped progress-bar-animated",
                 role = "progressbar",
                 style = paste0("width:", pct, "%; transition:width .3s ease;"))
      ),
      tags$div(
        class = "engine-status-label small",
        tags$span(class = "spinner-border spinner-border-sm me-1"),
        sprintf("Preparing analysis engine (%d/%d) — %s",
                st$done, st$total, st$label)
      )
    )
  })

  ## ── Pair detection ───────────────────────────────────────────────────────────
  matched_pairs <- reactive({
    req(input$idat_files)
    all_files <- input$idat_files
    all_names <- all_files$name
    grn_idx   <- grep("_Grn\\.idat$", all_names, ignore.case = TRUE)
    red_idx   <- grep("_Red\\.idat$", all_names, ignore.case = TRUE)
    other_idx <- setdiff(seq_len(nrow(all_files)), c(grn_idx, red_idx))
    grn_pre   <- sub("_Grn\\.idat$", "", all_names[grn_idx], ignore.case = TRUE)
    red_pre   <- sub("_Red\\.idat$", "", all_names[red_idx], ignore.case = TRUE)
    list(
      matched   = intersect(grn_pre, red_pre),
      grn_only  = setdiff(grn_pre, red_pre),
      red_only  = setdiff(red_pre, grn_pre),
      other     = all_names[other_idx],
      all_files = all_files,
      grn_idx   = grn_idx, red_idx = red_idx,
      grn_pre   = grn_pre, red_pre = red_pre
    )
  })

  ## ── Validation banner ────────────────────────────────────────────────────────
  output$pair_validation_ui <- renderUI({
    req(input$idat_files)
    if (length(results_list()) > 0) return(NULL)
    p <- matched_pairs()
    tags$div(
      if (length(p$matched) > 0)
        tags$div(class = "alert alert-success p-2 small",
                 icon("circle-check"),
                 sprintf(" %d pair(s) detected and ready", length(p$matched)),
                 tags$ul(class = "mb-0 mt-1 ps-3",
                         lapply(p$matched, tags$li))),
      if (length(p$grn_only) > 0)
        tags$div(class = "alert alert-warning p-2 small", icon("triangle-exclamation"),
                 " No matching red for: ", paste(p$grn_only, collapse = ", ")),
      if (length(p$red_only) > 0)
        tags$div(class = "alert alert-warning p-2 small", icon("triangle-exclamation"),
                 " No matching green for: ", paste(p$red_only, collapse = ", ")),
      if (length(p$other) > 0)
        tags$div(class = "alert alert-warning p-2 small", icon("triangle-exclamation"),
                 " Unrecognized files: ", paste(p$other, collapse = ", "))
    )
  })

  ## ── Progress UI ──────────────────────────────────────────────────────────────
  output$progress_ui <- renderUI({
    steps <- step_status()
    if (length(steps) == 0) return(NULL)

    step_defs <- list(
      load    = list(label = "Loading IDAT files & pairs detected", pct = 15),
      preproc = list(label = "Preprocessing (noob + dye bias)",     pct = 40),
      predict = list(label = "SVM classification",                 pct = 65),
      cnv     = list(label = "CNV segmentation",                   pct = 100)
    )
    pct <- 0
    for (k in names(step_defs)) {
      st <- steps[[k]]
      if (st == "done")    pct <- step_defs[[k]]$pct
      if (st == "running") pct <- step_defs[[k]]$pct - 10
    }
    is_error  <- any(unlist(steps) == "error")
    all_done  <- pct >= 100 && !is_error
    bar_class <- if (is_error) "bg-danger"
                 else if (pct >= 100) "bg-success"
                 else "bg-navy progress-bar-animated progress-bar-striped"

    tags$div(
      class = "mt-3",
      if (!all_done)
        tags$div(class = "progress mb-3", style = "height:10px; border-radius:6px;",
          tags$div(class = paste("progress-bar", bar_class), role = "progressbar",
                   style = paste0("width:", pct, "%; transition: width 0.5s ease;"),
                   `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100)
        ),
      tags$div(
        class = "step-list",
        lapply(names(step_defs), function(k) {
          st  <- steps[[k]]
          ico <- switch(st,
            pending = tags$span(class = "step-icon text-muted",   icon("circle")),
            running = tags$span(class = "spinner-border spinner-border-sm text-navy me-1"),
            done    = tags$span(class = "step-icon text-success", icon("circle-check")),
            error   = tags$span(class = "step-icon text-danger",  icon("circle-xmark")),
            tags$span(class = "step-icon text-muted", icon("circle"))
          )
          cls <- switch(st,
            pending = "step-row text-muted", running = "step-row text-navy fw-semibold",
            done    = "step-row text-success", error = "step-row text-danger",
            "step-row text-muted")
          tags$div(class = cls, ico, tags$span(step_defs[[k]]$label))
        })
      )
    )
  })

  ## ── Run classifier — synchronous with withProgress ───────────────────────────
  observeEvent(input$go_btn, {
    p_data <- matched_pairs()
    validate(need(length(p_data$matched) > 0,
                  "No pairs found. Upload both _Grn.idat and _Red.idat files together."))

    n_pairs <- length(p_data$matched)
    disable("go_btn")
    results_list(list())

    ## Safety net: if the user got here before background loading finished,
    ## finish it now (blocking) so the analysis always has what it needs.
    if (!engine_ready()) {
      withProgress(message = "Preparing analysis engine...", value = 0, {
        engine_ensure_loaded()
      })
      engine_state(list(done = engine_total(), total = engine_total(),
                        label = "Ready", ready = TRUE))
    }

    all_results <- vector("list", n_pairs)

    tryCatch({
      withProgress(message = paste("Processing", n_pairs, "sample(s)..."), value = 0, {

        for (i in seq_len(n_pairs)) {
          pre   <- p_data$matched[[i]]
          lbl   <- if (n_pairs > 1) paste0("[", i, "/", n_pairs, "] ") else ""
          gi    <- p_data$grn_idx[which(p_data$grn_pre == pre)]
          ri    <- p_data$red_idx[which(p_data$red_pre == pre)]
          grn_path <- p_data$all_files$datapath[gi]
          grn_name <- p_data$all_files$name[gi]
          red_path <- p_data$all_files$datapath[ri]
          red_name <- p_data$all_files$name[ri]

          ## Step 1 — load
          setProgress(value = (i - 1) / n_pairs,
                      detail = paste0(lbl, "Loading IDAT files"))
          step_status(list(load="running", preproc="pending", predict="pending", cnv="pending"))

          tmp <- tempdir()
          file.copy(grn_path, file.path(tmp, grn_name), overwrite = TRUE)
          file.copy(red_path, file.path(tmp, red_name), overwrite = TRUE)
          prefix <- file.path(tmp, sub("_Grn\\.idat$", "", grn_name, ignore.case = TRUE))

          ## Step 2 — preprocess
          setProgress(value = (i - 1 + 0.15) / n_pairs,
                      detail = paste0(lbl, "Preprocessing"))
          step_status(list(load="done", preproc="running", predict="pending", cnv="pending"))

          sdf   <- readIDATpair(prefix) %>% pOOBAH() %>% noob() %>% dyeBiasCorrTypeINorm()
          chip  <- sdfPlatform(sdf)
          betas <- getBetas(sdf)

          ## Step 3 — classify
          setProgress(value = (i - 1 + 0.40) / n_pairs,
                      detail = paste0(lbl, "SVM classification"))
          step_status(list(load="done", preproc="done", predict="running", cnv="pending"))

          if (chip == "EPIC") {
            nd <- as.data.frame(t(betas[names(betas) %in% probes]))
            nd[is.na(nd)] <- 0
            p3 <- predict(svm_3g_EPIC,  newdata = nd)
            p4 <- predict(svm_4g_EPIC,  newdata = nd)
            s3 <- decode_3group(p3, "EPIC")
            s4 <- decode_4group(p4, "EPIC")

          } else if (chip == "HM450") {
            nd <- as.data.frame(t(betas[names(betas) %in% probes_450k]))
            nd[is.na(nd)] <- 0
            p3 <- predict(svm_3g_450K, newdata = nd)
            p4 <- predict(svm_4g_450K, newdata = nd)
            s3 <- as.character(p3);  s4 <- as.character(p4)

          } else if (chip == "EPICv2") {
            nd <- as.data.frame(t(betas[names(betas) %in% probes_v2]))
            nd[is.na(nd)] <- 0
            names(nd) <- gsub("_TC21|_BC21", "", names(nd))
            p3 <- predict(svm_3g_EPICv2, newdata = nd)
            p4 <- predict(svm_4g_EPICv2, newdata = nd)
            s3 <- as.character(p3);  s4 <- as.character(p4)

          } else {
            s3 <- s4 <- paste("Unknown chip:", chip)
          }

          ## Step 4 — CNV
          setProgress(value = (i - 1 + 0.65) / n_pairs,
                      detail = paste0(lbl, "CNV segmentation (this takes ~1-2 min)"))
          step_status(list(load="done", preproc="done", predict="done", cnv="running"))

          if (chip == "EPICv2") {
            segs <- cnSegmentation_EPICv2(sdf, sdfs.normal)
          } else {
            segs <- cnSegmentation(sdf, sdfs.normal)
          }

          setProgress(value = i / n_pairs,
                      detail = paste0(lbl, "Done"))

          all_results[[i]] <- list(
            sample_id = pre, chip = chip,
            class_3g  = s3, class_4g = s4,
            segs      = segs
          )
        }
      })

      step_status(list(load="done", preproc="done", predict="done", cnv="done"))
      results_list(all_results)

    }, error = function(e) {
      step_status(list(load="error", preproc="error", predict="error", cnv="error"))
      showNotification(paste("Error:", e$message), type = "error", duration = 20)
    })

    enable("go_btn")
  })

  ## ── Results table ────────────────────────────────────────────────────────────
  output$results_table_ui <- renderUI({
    res <- results_list()
    if (length(res) == 0)
      return(p("Upload IDAT files and click Run Classifier to see results.",
               class = "text-muted small"))
    rows <- lapply(res, function(r) {
      tags$tr(
        tags$td(tags$code(style = "font-size:0.82rem;", r$sample_id)),
        tags$td(tags$span(class = "badge", style = "background:#6b7280;", r$chip)),
        tags$td(tags$span(class = "badge",
                          style = paste0("background:", class_color(r$class_3g), ";"),
                          r$class_3g)),
        tags$td(tags$span(class = "badge",
                          style = paste0("background:", class_color(r$class_4g), ";"),
                          r$class_4g))
      )
    })
    tags$table(class = "table table-hover table-sm align-middle",
      tags$thead(tags$tr(
        tags$th("Sample"), tags$th("Chip"),
        tags$th("3-Group Class"), tags$th("4-Group Class")
      )),
      tags$tbody(rows)
    )
  })

  ## ── CSV download ─────────────────────────────────────────────────────────────
  output$dl_csv <- downloadHandler(
    filename = function() paste0("menin_results_", Sys.Date(), ".csv"),
    content  = function(file) {
      res <- results_list()
      df  <- do.call(rbind, lapply(res, function(r)
        data.frame(sample_id    = r$sample_id,
                   chip         = r$chip,
                   class_3group = r$class_3g,
                   class_4group = r$class_4g,
                   stringsAsFactors = FALSE)))
      write.csv(df, file, row.names = FALSE)
    }
  )

  ## ── Per-sample result blocks (full-width CNV plot per sample) ───────────────
  output$sample_blocks_ui <- renderUI({
    res <- results_list()
    if (length(res) == 0) return(NULL)
    tagList(lapply(seq_along(res), function(i) {
      r <- res[[i]]
      card(
        card_header(
          icon("chart-area"),
          paste0(" CNV — ", r$sample_id),
          tags$span(class = "badge ms-1",
                    style = paste0("background:", class_color(r$class_3g), ";"),
                    r$class_3g),
          downloadButton(paste0("dl_cnv_", i), "PNG",
                         class = "btn btn-sm btn-outline-navy float-end")
        ),
        card_body(plotlyOutput(paste0("cnv_plot_", i), height = "300px"))
      )
    }))
  })

  ## Dynamic per-sample outputs (CNV plotly + PNG download)
  observe({
    res <- results_list()
    if (length(res) == 0) return()
    lapply(seq_along(res), function(i) {
      local({
        r       <- res[[i]]
        plot_id <- paste0("cnv_plot_", i)
        dl_id   <- paste0("dl_cnv_",  i)

        output[[plot_id]] <- renderPlotly({ plotly_cnv(r$segs, r$sample_id) })

        ## Download as PNG via ggplot2 (no kaleido/orca needed)
        output[[dl_id]] <- downloadHandler(
          filename = function() paste0(r$sample_id, "_CNV_", Sys.Date(), ".png"),
          content  = function(file) {
            p <- cnv_ggplot(r$segs)
            ggplot2::ggsave(file, plot = p, width = 16, height = 4.5,
                            dpi = 150, bg = "white")
          }
        )
      })
    })
  })
}
