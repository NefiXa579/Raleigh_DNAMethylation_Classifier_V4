## global.R — loaded once per R process, shared across all sessions
##
## STARTUP STRATEGY
## Only the libraries needed to *draw* the interface are loaded here, so the
## page appears in a couple of seconds. Everything heavy (sesame, caret, the
## reference panel, the SVM models — ~25 s in total) is loaded afterwards, in
## chunks, by the "engine" below. The server kicks this off automatically as
## soon as the page has been sent to the browser, so it finishes while the user
## is still locating and uploading their IDAT files.
##
## NOTE: R is single-threaded, so this is not true background loading — it is
## deferred, chunked loading. Nothing about the analysis itself changes; the
## same libraries and data are loaded, just a few seconds later.

## ── UI-critical libraries (fast — needed to render the page) ─────────────────
library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(plotly)
library(scales)

options(shiny.maxRequestSize = 100 * 1024^2)

## Models + probe lists ship with the app, in ./models (relative to the app dir).
## Shiny sets the working directory to the app folder while running, so this
## resolves correctly regardless of where the launcher is started from.
BASE_DIR <- "models"

## ── Analysis engine: deferred, chunked loading ──────────────────────────────
.engine <- new.env(parent = emptyenv())
.engine$done  <- 0L
.engine$ready <- FALSE

## Each step is one chunk of work. They run one per tick, so the app can
## respond to the browser in between. Objects are assigned into the global
## environment, exactly where the original code expected to find them.
ENGINE_STEPS <- list(
  list(label = "Loading methylation toolkit", fn = function() {
    suppressMessages({ library(sesame); library(sesameData) })
  }),
  list(label = "Loading classifier libraries", fn = function() {
    suppressMessages({ library(caret); library(kernlab) })
  }),
  list(label = "Loading genomics libraries", fn = function() {
    suppressMessages({
      library(DNAcopy); library(GenomicRanges); library(IRanges); library(BiocManager)
    })
    options(repos = BiocManager::repositories())
  }),
  list(label = "Reading probe lists", fn = function() {
    g <- globalenv()
    assign("probes",      scan(file.path(BASE_DIR, "probes.txt"),       character(), quote = "", quiet = TRUE), envir = g)
    assign("probes_450k", scan(file.path(BASE_DIR, "probes_450K.txt"),  character(), quote = "", quiet = TRUE), envir = g)
    assign("probes_v2",   scan(file.path(BASE_DIR, "probes_EPICv2.txt"),character(), quote = "", quiet = TRUE), envir = g)
  }),
  list(label = "Preparing reference data", fn = function() {
    sesameDataCache()
  }),
  list(label = "Loading normal reference panel", fn = function() {
    assign("sdfs.normal", sesameDataGet("EPIC.5.SigDF.normal"), envir = globalenv())
  }),
  list(label = "Loading classification models", fn = function() {
    g <- globalenv()
    assign("svm_3g_EPIC",   readRDS(file.path(BASE_DIR, "sesame_classifier.rds")),             envir = g)
    assign("svm_4g_EPIC",   readRDS(file.path(BASE_DIR, "sesame_classifier_four.rds")),        envir = g)
    assign("svm_3g_450K",   readRDS(file.path(BASE_DIR, "svm_Linear_450K.rds")),               envir = g)
    assign("svm_4g_450K",   readRDS(file.path(BASE_DIR, "svm_Linear_450K_4groups.rds")),       envir = g)
    assign("svm_3g_EPICv2", readRDS(file.path(BASE_DIR, "svm_linear_EPICv2.rds")),             envir = g)
    assign("svm_4g_EPICv2", readRDS(file.path(BASE_DIR, "svm_Linear_EPICv2_4groups.rds")),     envir = g)
  })
)

engine_total <- function() length(ENGINE_STEPS)
engine_ready <- function() isTRUE(.engine$ready)

## Runs the next pending chunk. Returns TRUE if work was done.
engine_run_next <- function() {
  i <- .engine$done + 1L
  if (i > length(ENGINE_STEPS)) { .engine$ready <- TRUE; return(FALSE) }
  ENGINE_STEPS[[i]]$fn()
  .engine$done <- i
  if (.engine$done >= length(ENGINE_STEPS)) .engine$ready <- TRUE
  TRUE
}

## Current progress + the label of whatever comes next.
engine_progress <- function() {
  n <- length(ENGINE_STEPS)
  list(
    done  = .engine$done,
    total = n,
    label = if (.engine$done < n) ENGINE_STEPS[[.engine$done + 1L]]$label else "Ready"
  )
}

## Blocking fallback: guarantees the engine is fully loaded before analysis,
## in case the user clicks Run before background loading has finished.
engine_ensure_loaded <- function() {
  while (!engine_ready()) engine_run_next()
  invisible(TRUE)
}

## ── Optimized leftRightMerge1 ────────────────────────────────────────────────
## Original was O(n²) due to dataframe copy on every iteration.
## This version operates on vectors only — no frame rebuilds inside the loop.
leftRightMerge1 <- function(chrom.windows, min.probes.per.bin = 20) {
  start  <- chrom.windows$start
  end    <- chrom.windows$end
  probes <- chrom.windows$probes
  seqn   <- chrom.windows$seqnames

  while (length(probes) > 0 && min(probes) < min.probes.per.bin) {
    i <- which.min(probes)
    n <- length(probes)

    can_left  <- i > 1 && start[i] - 1 == end[i - 1]
    can_right <- i < n && end[i] + 1 == start[i + 1]

    if (can_left && can_right) {
      if (probes[i - 1] <= probes[i + 1]) can_right <- FALSE else can_left <- FALSE
    }

    if (can_left) {
      end[i - 1]    <- end[i]
      probes[i - 1] <- probes[i - 1] + probes[i]
      keep <- seq_len(n)[-i]
    } else if (can_right) {
      start[i + 1]  <- start[i]
      probes[i + 1] <- probes[i + 1] + probes[i]
      keep <- seq_len(n)[-i]
    } else {
      keep <- seq_len(n)[-i]
    }

    start  <- start[keep]
    end    <- end[keep]
    probes <- probes[keep]
    seqn   <- seqn[keep]
  }

  data.frame(seqnames = seqn, start = start, end = end, probes = probes,
             stringsAsFactors = FALSE)
}

## ── getBinCoordinates (unchanged logic, uses fixed leftRightMerge1) ──────────
getBinCoordinates <- function(seqLength, gapInfo, tilewidth = 50000, probeCoords) {
  tiles <- sort(GenomicRanges::tileGenome(seqLength, tilewidth = tilewidth,
                                          cut.last.tile.in.chrom = TRUE))
  tiles <- sort(c(
    GenomicRanges::setdiff(tiles[seq(1, length(tiles), 2)], gapInfo),
    GenomicRanges::setdiff(tiles[seq(2, length(tiles), 2)], gapInfo)))
  GenomicRanges::values(tiles)$probes <- GenomicRanges::countOverlaps(tiles, probeCoords)

  bin.coords <- do.call(rbind, lapply(
    split(tiles, as.vector(GenomicRanges::seqnames(tiles))),
    function(chrom.tiles)
      leftRightMerge1(GenomicRanges::as.data.frame(GenomicRanges::sort(chrom.tiles)))))

  bin.coords <- GenomicRanges::sort(GenomicRanges::GRanges(
    seqnames = bin.coords$seqnames,
    IRanges::IRanges(start = bin.coords$start, end = bin.coords$end),
    seqinfo = GenomicRanges::seqinfo(tiles)))

  chr.cnts <- table(as.vector(GenomicRanges::seqnames(bin.coords)))
  names(bin.coords) <- paste(
    as.vector(GenomicRanges::seqnames(bin.coords)),
    formatC(unlist(lapply(GenomicRanges::seqnames(bin.coords)@lengths, seq_len)),
            width = nchar(max(chr.cnts)), format = "d", flag = "0"), sep = "-")
  bin.coords
}

## ── binSignals (unchanged) ───────────────────────────────────────────────────
binSignals <- function(probe.signals, bin.coords, probeCoords) {
  ov <- GenomicRanges::findOverlaps(probeCoords, bin.coords)
  if (.hasSlot(ov, "queryHits")) {
    .bins         <- names(bin.coords)[ov@subjectHits]
    .probe.signals <- probe.signals[names(probeCoords)[ov@queryHits]]
  } else {
    .bins         <- names(bin.coords)[ov@to]
    .probe.signals <- probe.signals[names(probeCoords)[ov@from]]
  }
  vapply(split(.probe.signals, .bins), median, 1, na.rm = TRUE)
}

## ── segmentBins (nperm kept at 10000 per clinical standard) ─────────────────
segmentBins <- function(bin.signals, bin.coords) {
  bin.coords <- bin.coords[names(bin.signals)]
  maplocs <- as.integer((GenomicRanges::start(bin.coords) +
                           GenomicRanges::end(bin.coords)) / 2)
  cna <- DNAcopy::CNA(genomdat = bin.signals,
                      chrom = as.character(GenomicRanges::seqnames(bin.coords)),
                      maploc = maplocs, data.type = "logratio")
  seg <- DNAcopy::segment(cna, min.width = 5, nperm = 10000, alpha = 0.001,
                          undo.splits = "sdundo", undo.SD = 2.2, verbose = 0)
  summary <- DNAcopy::segments.summary(seg)
  pval    <- DNAcopy::segments.p(seg)
  seg.signals <- cbind(summary, pval[, c("pval", "lcl", "ucl")])
  seg.signals$chrom <- as.character(seg.signals$chrom)
  seg.signals
}

## ── CNV static-data cache ────────────────────────────────────────────────────
## genome info, probe-coordinate manifest, and the normal-panel total-intensity
## matrix are identical for every sample on a given platform. Compute them once
## (lazily, on first use) and reuse across all samples in a batch.
.cnv_cache <- new.env(parent = emptyenv())
get_cnv_static <- function(platform, sdfs.normal) {
  hit <- .cnv_cache[[platform]]
  if (!is.null(hit)) return(hit)
  genome      <- sesameData_check_genome(NULL, platform)
  genomeInfo  <- sesameData_getGenomeInfo(genome)
  probeCoords <- sesameData_getManifestGRanges(platform, genome = genome)
  normal.full <- do.call(cbind, lapply(sdfs.normal, totalIntensities))
  res <- list(genomeInfo = genomeInfo, probeCoords = probeCoords,
              normal.full = normal.full)
  .cnv_cache[[platform]] <- res
  res
}

## ── cnSegmentation for EPICv2 (same logic as original) ──────────────────────
cnSegmentation_EPICv2 <- function(sdf, sdfs.normal = NULL, genomeInfo = NULL,
                                   probeCoords = NULL, tilewidth = 50000,
                                   verbose = FALSE) {
  stopifnot(is(sdf, "SigDF"))
  platform <- sdfPlatform(sdf, verbose = verbose)
  if (is.null(sdfs.normal))
    stop("Please provide sdfs.normal=. No default for EPICv2.")

  ## Reuse cached platform-constant data (manifest, genome, normal panel)
  static <- get_cnv_static(platform, sdfs.normal)
  if (is.null(genomeInfo))  genomeInfo  <- static$genomeInfo
  if (is.null(probeCoords)) probeCoords <- static$probeCoords
  normal.intens <- static$normal.full

  seqLength   <- genomeInfo$seqLength
  gapInfo     <- genomeInfo$gapInfo
  target.intens <- na.omit(totalIntensities(sdf))

  ## gsub is vectorized — apply it once to the whole name vector, not per element
  pb_suffixincluded <- names(target.intens)
  pb_suffixremoved  <- gsub("_.*", "", pb_suffixincluded)
  pb   <- intersect(rownames(normal.intens), pb_suffixremoved)
  pb_i <- match(rownames(normal.intens), pb_suffixremoved)
  pb_suffixincluded <- pb_suffixincluded[pb_i[!is.na(pb_i)]]
  pb   <- intersect(names(probeCoords), pb_suffixincluded)
  pb_suffixremoved_2 <- gsub("_.*", "", pb)

  target.intens <- target.intens[pb]
  normal.intens <- normal.intens[as.character(pb_suffixremoved_2), ]
  probeCoords   <- probeCoords[pb]

  fit <- lm(y ~ ., data = data.frame(y = target.intens, X = normal.intens))
  probe.signals <- setNames(log2(target.intens / pmax(predict(fit), 1)), pb)
  bin.coords    <- getBinCoordinates(seqLength, gapInfo, tilewidth, probeCoords)
  bin.signals   <- binSignals(probe.signals, bin.coords, probeCoords)

  structure(list(seg.signals = segmentBins(bin.signals, bin.coords),
                 bin.coords = bin.coords, bin.signals = bin.signals),
            class = "CNSegment")
}

## ── Class label helpers ──────────────────────────────────────────────────────
decode_3group <- function(p, chip) {
  if (chip == "EPIC") {
    switch(as.character(p),
           "3" = "Merlin-intact", "1" = "Immune-enriched",
           "2" = "Hypermitotic", "Unclassified")
  } else {
    as.character(p)
  }
}

decode_4group <- function(p, chip) {
  if (chip == "EPIC") {
    switch(as.character(p),
           "3" = "Merlin-intact", "1" = "Immune-enriched",
           "2" = "Hypermetabolic", "4" = "Proliferative", "Unclassified")
  } else {
    as.character(p)
  }
}

class_color <- function(label) {
  switch(label,
         "Merlin-intact"   = "#0d9488",
         "Immune-enriched" = "#7c3aed",
         "Hypermitotic"    = "#ea580c",
         "Hypermetabolic"  = "#ca8a04",
         "Proliferative"   = "#be123c",
         "#6b7280")
}

## ── cnv_ggplot — ggplot2 CNV for PNG download (avoids sesame name collision) ──
cnv_ggplot <- function(seg, to.plot = NULL) {
  stopifnot(is(seg, "CNSegment"))
  bin.coords  <- seg$bin.coords
  bin.seqinfo <- seqinfo(bin.coords)
  bin.signals <- seg$bin.signals
  sigs        <- seg$seg.signals

  total.length <- sum(as.numeric(bin.seqinfo@seqlengths), na.rm = TRUE)
  if (is.null(to.plot))
    to.plot <- (bin.seqinfo@seqlengths > total.length * 0.01)
  seqlen    <- as.numeric(bin.seqinfo@seqlengths[to.plot])
  seq.names <- bin.seqinfo@seqnames[to.plot]
  totlen    <- sum(seqlen, na.rm = TRUE)
  seqcumlen <- cumsum(seqlen)
  seqstart  <- setNames(c(0, seqcumlen[-length(seqcumlen)]), seq.names)

  bin.coords  <- bin.coords[as.vector(seqnames(bin.coords)) %in% seq.names]
  bin.signals <- bin.signals[names(bin.coords)]
  bin.x_norm  <- (seqstart[as.character(seqnames(bin.coords))] +
                    (start(bin.coords) + end(bin.coords)) / 2) / totlen

  ## Bin scatter
  df_bins <- data.frame(x = bin.x_norm, y = bin.signals)
  p <- ggplot2::ggplot(df_bins, ggplot2::aes(x = x, y = y, color = y)) +
    ggplot2::geom_point(alpha = 0.8, size = 0.6) +
    ggplot2::scale_colour_gradient2(
      limits = c(-0.3, 0.3), low = "red", mid = "grey70", high = "green",
      oob = scales::squish, guide = "none")

  ## Segment lines — use data frame with annotate to avoid aes scoping issues
  seg.beg  <- (seqstart[sigs$chrom] + sigs$loc.start) / totlen
  seg.end  <- (seqstart[sigs$chrom] + sigs$loc.end)   / totlen
  df_segs  <- data.frame(x = seg.beg, xend = seg.end,
                          y = sigs$seg.mean, yend = sigs$seg.mean)
  p <- p + ggplot2::geom_segment(data = df_segs,
                                  ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                                  linewidth = 1.0, color = "blue", inherit.aes = FALSE)

  ## Chromosome boundaries
  p <- p + ggplot2::geom_vline(xintercept = seqstart[-1] / totlen,
                                linetype = "dotted", alpha = 0.5, color = "grey50")

  ## Chromosome labels
  chr.breaks <- (seqstart + seqlen / 2) / totlen
  p <- p + ggplot2::scale_x_continuous(labels = seq.names, breaks = chr.breaks) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 7),
                   legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::xlab("") + ggplot2::ylab("Log2 ratio")
  p
}

## ── Process one IDAT pair — returns a list with results + seg object ─────────
process_idat_pair <- function(grn_path, grn_name, red_path, red_name,
                               probes, probes_450k, probes_v2,
                               svm_3g_EPIC, svm_4g_EPIC,
                               svm_3g_450K, svm_4g_450K,
                               svm_3g_EPICv2, svm_4g_EPICv2,
                               sdfs.normal) {
  tmp <- tempdir()
  file.copy(grn_path, file.path(tmp, grn_name))
  file.copy(red_path, file.path(tmp, red_name))
  prefix <- file.path(tmp, sub("_Grn\\.idat$", "", grn_name))

  sdf     <- readIDATpair(prefix) %>% pOOBAH() %>% noob() %>% dyeBiasCorrTypeINorm()
  chip    <- sdfPlatform(sdf)
  betas   <- getBetas(sdf)

  if (chip == "EPIC") {
    nd  <- as.data.frame(t(betas[names(betas) %in% probes]))
    nd[is.na(nd)] <- 0
    p3  <- predict(svm_3g_EPIC, newdata = nd)
    p4  <- predict(svm_4g_EPIC, newdata = nd)
    s3  <- decode_3group(p3, "EPIC")
    s4  <- decode_4group(p4, "EPIC")
  } else if (chip == "HM450") {
    nd  <- as.data.frame(t(betas[names(betas) %in% probes_450k]))
    nd[is.na(nd)] <- 0
    p3  <- predict(svm_3g_450K, newdata = nd)
    p4  <- predict(svm_4g_450K, newdata = nd)
    s3  <- as.character(p3)
    s4  <- as.character(p4)
  } else if (chip == "EPICv2") {
    nd  <- as.data.frame(t(betas[names(betas) %in% probes_v2]))
    nd[is.na(nd)] <- 0
    names(nd) <- gsub("_TC21|_BC21", "", names(nd))
    p3  <- predict(svm_3g_EPICv2, newdata = nd)
    p4  <- predict(svm_4g_EPICv2, newdata = nd)
    s3  <- as.character(p3)
    s4  <- as.character(p4)
  } else {
    s3 <- s4 <- "Unknown chip"
    chip <- chip
  }

  if (chip == "EPICv2") {
    segs <- cnSegmentation_EPICv2(sdf, sdfs.normal)
  } else {
    segs <- cnSegmentation(sdf, sdfs.normal)
  }

  list(
    sample_id = sub("_Grn\\.idat$", "", grn_name),
    chip      = chip,
    class_3g  = s3,
    class_4g  = s4,
    segs      = segs
  )
}

## ── Interactive plotly CNV — matches original ggplot2 output ─────────────────
plotly_cnv <- function(seg, sample_id) {
  bin.coords  <- seg$bin.coords
  bin.seqinfo <- seqinfo(bin.coords)
  bin.signals <- seg$bin.signals
  sigs        <- seg$seg.signals

  total.length <- sum(as.numeric(bin.seqinfo@seqlengths), na.rm = TRUE)
  to.plot  <- (bin.seqinfo@seqlengths > total.length * 0.01)
  seqlen   <- as.numeric(bin.seqinfo@seqlengths[to.plot])
  seq.names <- bin.seqinfo@seqnames[to.plot]
  totlen   <- sum(seqlen, na.rm = TRUE)
  seqcumlen <- cumsum(seqlen)
  seqstart  <- setNames(c(0, seqcumlen[-length(seqcumlen)]), seq.names)

  bin.coords  <- bin.coords[as.vector(seqnames(bin.coords)) %in% seq.names]
  bin.signals <- bin.signals[names(bin.coords)]

  bin.mids <- (start(bin.coords) + end(bin.coords)) / 2
  bin.x_n  <- (seqstart[as.character(seqnames(bin.coords))] + bin.mids) / totlen

  ## Continuous colour scale: red → grey → green clamped at ±0.3
  ## matching sesame's visualizeSegments default
  clamp  <- function(x, lo, hi) pmax(pmin(x, hi), lo)
  norm01 <- (clamp(bin.signals, -0.3, 0.3) + 0.3) / 0.6   # 0=red, 0.5=grey, 1=green

  ## Chromosome break positions and vertical dividers
  chr.mid    <- (seqstart + seqlen / 2) / totlen
  chr.vlines <- seqstart[-1] / totlen

  ## Segment lines
  seg.beg <- (seqstart[sigs$chrom] + sigs$loc.start) / totlen
  seg.end <- (seqstart[sigs$chrom] + sigs$loc.end)   / totlen

  ## Y range: auto with a bit of padding, always including 0
  y_lo <- min(-0.5, min(bin.signals, na.rm = TRUE) * 1.1)
  y_hi <- max(0.3,  max(bin.signals, na.rm = TRUE) * 1.1)

  ## Colour vector matching gradient2(low=red, mid=grey, high=green)
  bin_col <- grDevices::rgb(
    r = ifelse(norm01 < 0.5, 1,           1 - 2*(norm01 - 0.5)),
    g = ifelse(norm01 < 0.5, 2*norm01,    1),
    b = ifelse(norm01 < 0.5, 2*norm01,    1 - 2*(norm01 - 0.5)),
    alpha = 0.85
  )

  p <- plot_ly() %>%
    ## Scatter bins
    add_trace(
      x = bin.x_n, y = bin.signals,
      type = "scatter", mode = "markers",
      marker = list(color = bin_col, size = 3),
      text   = paste0(as.character(seqnames(bin.coords)),
                      "<br>Signal: ", round(bin.signals, 3)),
      hoverinfo = "text", name = "Bins", showlegend = FALSE
    ) %>%
    ## Segment means (blue lines)
    add_segments(
      x = seg.beg, xend = seg.end, y = sigs$seg.mean, yend = sigs$seg.mean,
      line = list(color = "#1d4ed8", width = 2.5),
      hoverinfo = "skip", showlegend = FALSE
    )

  ## Chromosome boundary dotted lines
  for (v in chr.vlines) {
    p <- p %>% add_segments(
      x = v, xend = v, y = y_lo - 1, yend = y_hi + 0.5,
      line      = list(color = "rgba(120,120,120,0.3)", dash = "dot", width = 1),
      hoverinfo = "skip", showlegend = FALSE
    )
  }

  p %>% layout(
    xaxis = list(
      tickmode  = "array",
      tickvals  = chr.mid,
      ticktext  = seq.names,
      tickangle = -90,
      title     = "",
      showgrid  = FALSE,
      zeroline  = FALSE
    ),
    yaxis = list(
      title    = "Log2 ratio",
      range    = c(y_lo, y_hi),
      zeroline = TRUE, zerolinecolor = "#94a3b8", zerolinewidth = 1
    ),
    plot_bgcolor  = "#f9fafb",
    paper_bgcolor = "#ffffff",
    showlegend    = FALSE,
    margin        = list(b = 70, l = 55, r = 10, t = 10)
  )
}
