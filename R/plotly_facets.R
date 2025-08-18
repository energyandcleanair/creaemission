# Utilities to normalize ggplotly facet_wrap panel widths and strip labels

#' Fix uneven facet widths from ggplotly(facet_wrap(...))
#'
#' ggplotly can assign larger domains to first/last panels. This post-processor
#' normalizes panel domains per row, optionally recenters facet strip annotations,
#' aligns strip rectangles, and adds margins to avoid label overlap.
#'
#' @param fig A plotly object created by ggplotly.
#' @param hgap Numeric. Horizontal gap between adjacent panels in a row (paper coords).
#' @param outer Numeric length 1 or 2. Left/right outer margins per row (paper coords).
#' @param recenter_strips Logical. If TRUE, recenter facet title annotations per panel.
#' @param adjust_strip_rects Logical. If TRUE, align strip rectangles to panel domains.
#' @param y_title_standoff Integer px. Distance between y-axis title and ticks.
#' @param left_margin Integer px. Extra left margin for the whole figure to keep y-title outside.
#' @param rename_strip_prefix Optional character. If set (e.g. "T"), facet titles will be
#'   replaced by T1, T2, ... in reading order for debugging purposes.
#' @param enumerate_from Integer. Starting index when renaming strips.
#' @return The modified plotly object.
#' @export
fix_ggplotly_facets <- function(fig,
                                hgap = 0.04,
                                outer = 0.03,           # number or c(left, right)
                                vgap = 0.04,            # vertical gap between rows (paper coords)
                                vouter = 0.03,          # number or c(bottom, top)
                                recenter_strips = TRUE,
                                adjust_strip_rects = TRUE,
                                y_title_standoff = 20,
                                left_margin = NULL,
                                right_margin = NULL,
                                axis_tick_font_size = 10,
                                axis_tick_font_color = "grey60",
                                legend_font_size = 12,
                                rename_strip_prefix = NULL,
                                enumerate_from = 1,
                                constant_slots = TRUE,  # keep same number of slots per row
                                strip_position = c("auto", "bottom", "top"),
                                strip_row_offset = 0    # additional downward offset for top strips on non-first rows
                                ) {
  if (is.null(fig$x$layout)) return(fig)

  # Normalize colors to hex to ensure Plotly applies them reliably
  to_hex <- function(col) {
    if (is.null(col)) return(NULL)
    tryCatch({
      rgb <- grDevices::col2rgb(col)
      sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
    }, error = function(...) col)
  }
  axis_tick_font_color <- to_hex(axis_tick_font_color)

  layout_list <- fig$x$layout
  xaxis_names <- names(layout_list)[grepl("^xaxis\\d*$", names(layout_list))]
  yaxis_names <- names(layout_list)[grepl("^yaxis\\d*$", names(layout_list))]
  if (length(xaxis_names) <= 1L) return(fig)

  get_suffix <- function(axis_name) {
    suf <- sub("^[xy]axis", "", axis_name)
    if (identical(suf, "") || is.na(suf)) 1L else suppressWarnings(as.integer(suf))
  }

  axis_meta <- lapply(xaxis_names, function(xnm) {
    idx <- get_suffix(xnm)
    ynm <- if (idx == 1L) "yaxis" else paste0("yaxis", idx)
    xdom <- layout_list[[xnm]]$domain
    ydom <- layout_list[[ynm]]$domain
    list(xnm = xnm, ynm = ynm, xdom = xdom, ydom = ydom)
  })

  row_keys <- vapply(axis_meta, function(m) paste(round(unlist(m$ydom), 4), collapse = ":"), character(1))
  unique_rows <- unique(row_keys)

  if (length(outer) == 1L) outer <- c(outer, outer)
  left_outer <- outer[1]
  right_outer <- outer[2]

  row_to_domains <- list()
  row_to_yhigh <- list()
  # Map row key -> yaxis names present in that row
  row_to_yaxes <- list()

  # Determine maximum number of panels in any row (slots per row)
  row_to_indices <- split(seq_along(axis_meta), row_keys)
  max_slots <- max(vapply(row_to_indices, length, integer(1)))

  for (rk in unique_rows) {
    idxs <- which(row_keys == rk)
    idxs <- idxs[order(vapply(axis_meta[idxs], function(m) m$xdom[[1]], numeric(1)))]
    n <- length(idxs)
    if (n == 0) next

    # number of horizontal slots to use in this row
    slots <- if (constant_slots) max_slots else n
    total_gap <- hgap * (slots - 1)
    available <- 1 - (left_outer + right_outer) - total_gap
    if (available <= 0) {
      available <- max(0.01, 1 - total_gap)
      left_outer <- right_outer <- max(0, (1 - total_gap - available) / 2)
    }
    width <- available / slots
    starts <- left_outer + (seq_len(slots) - 1) * (width + hgap)

    row_domains <- vector("list", n)
    for (i in seq_len(n)) {
      xnm <- axis_meta[[idxs[i]]]$xnm
      dom <- c(starts[i], starts[i] + width)
      fig$x$layout[[xnm]]$domain <- dom
      fig$x$layout[[xnm]]$automargin <- TRUE
      row_domains[[i]] <- dom
    }
    row_to_domains[[rk]] <- row_domains
    ydom <- axis_meta[[idxs[1]]]$ydom
    row_to_yhigh[[rk]] <- ydom[[2]]
    # collect yaxes for this row
    row_to_yaxes[[rk]] <- unique(vapply(axis_meta[idxs], function(m) m$ynm, character(1)))
  }

  for (ynm in yaxis_names) {
    fig$x$layout[[ynm]]$automargin <- TRUE
    existing_title <- fig$x$layout[[ynm]]$title
    title_text <- if (is.list(existing_title) && !is.null(existing_title$text)) existing_title$text else existing_title
    if (is.null(title_text)) title_text <- ""
    fig$x$layout[[ynm]]$title <- list(text = title_text, standoff = y_title_standoff)
    fig$x$layout[[ynm]]$tickfont <- list(size = axis_tick_font_size, color = axis_tick_font_color)
    fig$x$layout[[ynm]]$color <- axis_tick_font_color
  }
  for (xnm in xaxis_names) {
    if (is.null(fig$x$layout[[xnm]])) next
    fig$x$layout[[xnm]]$tickfont <- list(size = axis_tick_font_size, color = axis_tick_font_color)
    fig$x$layout[[xnm]]$color <- axis_tick_font_color
  }

  # Equalize vertical row heights by setting consistent y-axis domains per row
  row_order <- names(row_to_domains)[order(unlist(row_to_yhigh), decreasing = TRUE)]
  n_rows <- length(row_order)
  if (length(vouter) == 1L) vouter <- c(vouter, vouter) # c(bottom, top)
  vbottom <- vouter[1]
  vtop <- vouter[2]
  total_vgap <- vgap * (n_rows - 1)
  v_available <- 1 - (vbottom + vtop) - total_vgap
  if (v_available <= 0) {
    v_available <- max(0.01, 1 - total_vgap)
    vbottom <- vtop <- max(0, (1 - total_vgap - v_available) / 2)
  }
  row_height <- v_available / n_rows
  # Assign bottom-up domains
  row_y_domains <- vector("list", n_rows)
  for (i in seq_len(n_rows)) {
    y0 <- vbottom + (i - 1) * (row_height + vgap)
    row_y_domains[[i]] <- c(y0, y0 + row_height)
  }
  # Map to rows in top-to-bottom order
  for (i in seq_along(row_order)) {
    rk <- row_order[[i]]
    # Convert top index to bottom-up index
    bottom_index <- n_rows - i + 1
    ydom <- row_y_domains[[bottom_index]]
    ynms <- row_to_yaxes[[rk]]
    for (ynm in ynms) {
      fig$x$layout[[ynm]]$domain <- ydom
    }
  }

  # Precompute per-row strip target vertical ranges and midpoints, with auto detection
  strip_position <- match.arg(strip_position)
  # Helper to detect where strips currently sit (top/bottom) using shapes
  detect_row_side <- function(ydomain, shapes) {
    if (is.null(shapes)) return(NA_character_)
    ylow <- ydomain[[1]]; yhigh <- ydomain[[2]]
    near <- function(y, target) abs(y - target) < 0.15
    top_ct <- 0; bot_ct <- 0
    for (s in shapes) {
      if (!is.list(s) || !identical(s$type, "rect") || !identical(s$xref, "paper") || !identical(s$yref, "paper") ||
          !is.numeric(s$y0) || !is.numeric(s$y1)) next
      ymid <- (s$y0 + s$y1) / 2
      if (near(ymid, yhigh)) top_ct <- top_ct + 1 else if (near(ymid, ylow)) bot_ct <- bot_ct + 1
    }
    if (top_ct == 0 && bot_ct == 0) return(NA_character_) else if (top_ct >= bot_ct) return("top") else return("bottom")
  }
  shapes <- fig$x$layout$shapes
  row_y_targets <- lapply(seq_along(row_order), function(i_top) {
    rk <- row_order[[i_top]]
    bottom_index <- n_rows - i_top + 1
    d <- row_y_domains[[bottom_index]]
    ylow <- d[[1]]; yhigh <- d[[2]]
    side <- if (strip_position == "auto") detect_row_side(d, shapes) else strip_position
    if (is.na(side)) side <- "bottom"
    if (side == "bottom") {
      y0 <- max(0, ylow - vgap * 0.95)
      y1 <- max(0, ylow - vgap * 0.05)
    } else {
      y0 <- min(1, yhigh + vgap * 0.05)
      y1 <- min(1, yhigh + vgap * 0.95)
    }
    # Apply additional downward offset for non-first rows when strips are on top
    if (side == "top" && i_top > 1 && strip_row_offset > 0) {
      y0 <- max(yhigh + vgap * 0.02, y0 - strip_row_offset)
      y1 <- max(yhigh + vgap * 0.03, y1 - strip_row_offset)
    }
    list(y0 = y0, y1 = y1, ymid = (y0 + y1) / 2, side = side)
  })

  row_order <- names(row_to_domains)[order(unlist(row_to_yhigh), decreasing = TRUE)]
  ordered_centers <- unlist(lapply(row_order, function(rk) vapply(row_to_domains[[rk]], function(d) mean(d), numeric(1))), use.names = FALSE)
  nearest_panel_idx <- function(x) which.min(abs(ordered_centers - x))

  if (recenter_strips && !is.null(fig$x$layout$annotations)) {
    rows <- lapply(names(row_to_domains), function(rk) {
      yvals <- as.numeric(strsplit(rk, ":", fixed = TRUE)[[1]])
      list(key = rk, ylow = yvals[1], yhigh = yvals[2], centers = vapply(row_to_domains[[rk]], function(d) mean(d), numeric(1)))
    })
    for (i in seq_along(fig$x$layout$annotations)) {
      a <- fig$x$layout$annotations[[i]]
      if (!is.list(a) || !identical(a$xref, "paper") || !identical(a$yref, "paper") || !is.numeric(a$x) || !is.numeric(a$y)) next
      dists <- vapply(rows, function(r) min(abs(a$y - r$ylow), abs(a$y - r$yhigh)), numeric(1))
      r_idx <- which.min(dists)
      centers <- rows[[r_idx]]$centers
      c_idx <- which.min(abs(centers - a$x))
      a$x <- centers[c_idx]
      a$xanchor <- "center"
      # Vertically center text inside computed strip band for this row
      a$y <- row_y_targets[[r_idx]]$ymid
      a$yanchor <- "middle"
      if (!is.null(rename_strip_prefix)) {
        global_idx <- enumerate_from - 1 + nearest_panel_idx(a$x)
        a$text <- paste0(rename_strip_prefix, global_idx)
      }
      fig$x$layout$annotations[[i]] <- a
    }
  }

  if (adjust_strip_rects && !is.null(fig$x$layout$shapes)) {
    rows <- lapply(names(row_to_domains), function(rk) {
      yvals <- as.numeric(strsplit(rk, ":", fixed = TRUE)[[1]])
      list(key = rk, ylow = yvals[1], yhigh = yvals[2], doms = row_to_domains[[rk]], centers = vapply(row_to_domains[[rk]], function(d) mean(d), numeric(1)))
    })
    for (i in seq_along(fig$x$layout$shapes)) {
      s <- fig$x$layout$shapes[[i]]
      if (!is.list(s) || !identical(s$type, "rect") || !identical(s$xref, "paper") || !identical(s$yref, "paper") ||
          !is.numeric(s$x0) || !is.numeric(s$x1) || !is.numeric(s$y0) || !is.numeric(s$y1)) next
      ymid <- (s$y0 + s$y1) / 2
      dists <- vapply(rows, function(r) min(abs(ymid - r$ylow), abs(ymid - r$yhigh)), numeric(1))
      r_idx <- which.min(dists)
      centers <- rows[[r_idx]]$centers
      doms <- rows[[r_idx]]$doms
      xmid <- (s$x0 + s$x1) / 2
      c_idx <- which.min(abs(centers - xmid))
      s$x0 <- doms[[c_idx]][1]
      s$x1 <- doms[[c_idx]][2]
      # Also align vertically to the computed strip band for this row
      s$y0 <- row_y_targets[[r_idx]]$y0
      s$y1 <- row_y_targets[[r_idx]]$y1
      # Remove border around strip rectangle
      s$line <- list(width = 0)
      fig$x$layout$shapes[[i]] <- s
    }
  }

  if (!is.null(left_margin)) {
    if (is.null(fig$x$layout$margin)) fig$x$layout$margin <- list()
    fig$x$layout$margin$l <- left_margin
  }
  if (!is.null(right_margin)) {
    if (is.null(fig$x$layout$margin)) fig$x$layout$margin <- list()
    fig$x$layout$margin$r <- right_margin
  }
  if (is.null(fig$x$layout$legend)) fig$x$layout$legend <- list()
  if (is.null(fig$x$layout$legend$font)) fig$x$layout$legend$font <- list()
  fig$x$layout$legend$font$size <- legend_font_size

  fig
}


