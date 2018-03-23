
library(purrr)
library(grid)
library(ggplot2)


trim_data <- function(data, min, max){
  data <- rbind(data, data.frame(tt=c(min, max), ttx=c(NA, NA), qs=c(NA, NA), id=c(0, 100)))
  data <- data[order(data$tt),]
  data[is.na(data$ttx), 'ttx'] <- approx(data$tt, data$ttx, xout = data[is.na(data$ttx), 'tt'])$y 
  data <- subset(data, tt <= max & tt >= min & !is.na(ttx))
  data[!duplicated(data[,c('tt', 'ttx')]), ]
}

# Detect and prevent collisions.
# Powers dodging, stacking and filling.
collide_setup <- function(data, width = NULL, name, strategy,
                          check.width = TRUE, reverse = FALSE) {
  # Determine width
  if (!is.null(width)) {
    # Width set manually
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin <- data$x - width / 2
      data$xmax <- data$x + width / 2
    }
  } else {
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin <- data$x
      data$xmax <- data$x
    }

    # Width determined from data, must be floating point constant
    widths <- unique(data$xmax - data$xmin)
    widths <- widths[!is.na(widths)]

#   # Suppress warning message since it's not reliable
#     if (!zero_range(range(widths))) {
#       warning(name, " requires constant width: output may be incorrect",
#         call. = FALSE)
#     }
    width <- widths[1]
  }

  list(data = data, width = width)  
}

collide <- function(data, width = NULL, name, strategy,
                    ..., check.width = TRUE, reverse = FALSE) {
  dlist <- collide_setup(data, width, name, strategy, check.width, reverse)
  data <- dlist$data
  width <- dlist$width

  # Reorder by x position, then on group. The default stacking order reverses
  # the group in order to match the legend order.
  if (reverse) {
    data <- data[order(data$xmin, data$group), ]
  } else {
    data <- data[order(data$xmin, -data$group), ]
  }

  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("xmin", "xmax")])))
  intervals <- intervals[!is.na(intervals)]

  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping x intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots.
    # The American Statistician, 1999.] should be used
  }
  
  if (!is.null(data$ymax)) {
    plyr::ddply(data, "xmin", strategy, ..., width = width)
  } else if (!is.null(data$y)) {
    data$ymax <- data$y
    data <- plyr::ddply(data, "xmin", strategy, ..., width = width)
    data$y <- data$ymax
    data
  } else {
    stop("Neither y nor ymax defined")
  }
}

# Alternate version of collide() used by position_dodge2()
collide2 <- function(data, width = NULL, name, strategy,
                     ..., check.width = TRUE, reverse = FALSE) {
  dlist <- collide_setup(data, width, name, strategy, check.width, reverse)
  data <- dlist$data
  width <- dlist$width

  # Reorder by x position, then on group. The default stacking order is
  # different than for collide() because of the order in which pos_dodge2 places
  # elements
  if (reverse) {
    data <- data[order(data$x, -data$group), ]
  } else {
    data <- data[order(data$x, data$group), ]
  }

  pos <- match.fun(strategy)
  pos(data, width, ...)
}



draw_key_eboxplot <- function(data, params, size) {
  grobTree(
    linesGrob(0.5, c(0.1, 0.25)),
    linesGrob(0.5, c(0.75, 0.9)),
    rectGrob(height = 0.5, width = 0.75),
    linesGrob(c(0.125, 0.875), 0.5),
    gp = gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype
    )
  )
}

ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

PositionDodge2 <- ggproto("PositionDodge2", PositionDodge,
  preserve = "single",
  padding = 0.1,
  reverse = FALSE,

  setup_params = function(self, data) {
    if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warning("Width not defined. Set with `position_dodge2(width = ?)`",
        call. = FALSE)
    }

    if (identical(self$preserve, "total")) {
      n <- NULL
    } else if ("x" %in% names(data)){
      n <- max(table(data$x))
    } else {
      n <- max(table(find_x_overlaps(data)))
    }

    list(
      width = self$width,
      n = n,
      padding = self$padding,
      reverse = self$reverse
    )
  },

  compute_panel = function(data, params, scales) {
    collide2(
      data,
      params$width,
      name = "position_dodge2",
      strategy = pos_dodge2,
      n = params$n,
      padding = params$padding,
      check.width = FALSE,
      reverse = params$reverse
    )
  }
)

pos_dodge2 <- function(df, width, n = NULL, padding = 0.1) {

  if (length(unique(df$group)) == 1) {
    return(df)
  }

  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }

  # xid represents groups of boxes that share the same position
  df$xid <- find_x_overlaps(df)

  # based on xid find newx, i.e. the center of each group of overlapping
  # elements. for boxes, bars, etc. this should be the same as original x, but
  # for arbitrary rects it may not be
  newx <- (tapply(df$xmin, df$xid, min) + tapply(df$xmax, df$xid, max)) / 2
  df$newx <- newx[df$xid]

  if (is.null(n)) {
    # If n is null, preserve total widths of elements at each position by
    # dividing widths by the number of elements at that position
    n <- table(df$xid)
    df$new_width <- (df$xmax - df$xmin) / as.numeric(n[df$xid])
  } else {
    df$new_width <- (df$xmax - df$xmin) / n
  }

  df$xmin <- df$x - (df$new_width / 2)
  df$xmax <- df$x + (df$new_width / 2)

  # Find the total width of each group of elements
  group_sizes <- stats::aggregate(
    list(size = df$new_width),
    list(newx = df$newx),
    sum
  )

  # Starting xmin for each group of elements
  starts <- group_sizes$newx - (group_sizes$size / 2)

  # Set the elements in place
  for (i in seq_along(starts)) {
    divisions <- cumsum(c(starts[i], df[df$xid == i, "new_width"]))
    df[df$xid == i, "xmin"] <- divisions[-length(divisions)]
    df[df$xid == i, "xmax"] <- divisions[-1]
  }

  # x values get moved to between xmin and xmax
  df$x <- (df$xmin + df$xmax) / 2

  # If no elements occupy the same position, there is no need to add padding
  if (!any(duplicated(df$xid))) {
    return(df)
  }

  # Shrink elements to add space between them
  df$pad_width <- df$new_width * (1 - padding)
  df$xmin <- df$x - (df$pad_width / 2)
  df$xmax <- df$x + (df$pad_width / 2)

  df$xid <- NULL
  df$newx <- NULL
  df$new_width <- NULL
  df$pad_width <- NULL

  df
}

# Find groups of overlapping elements that need to be dodged from one another
find_x_overlaps <- function(df) {
  overlaps <- vector(mode = "numeric", length = nrow(df))
  overlaps[1] <- counter <- 1
  for (i in 2:nrow(df)) {
    if (df$xmin[i] >= df$xmax[i - 1]) {
      counter <- counter + 1
    }
    overlaps[i] <- counter
  }
  overlaps
}





stat_eboxplot <- function(mapping = NULL, data = NULL,
                         geom = "eboxplot", position = "dodge2",
                         ...,
                         coef = 1.5,
                         na.rm = FALSE,
                         show.legend = NA,
                         rot=0,
                         inherit.aes = TRUE,
                         shade.upper=NA,
                         shade.lower=NA,
                         shade.fill="pink",
                         shade.value.absolute=FALSE
                         ) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatEboxplot,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      coef = coef,
      shade.upper=shade.upper,
      shade.lower=shade.lower,
      shade.fill=shade.fill,
      shade.value.absolute=shade.value.absolute,
      ...
    )
  )
}


StatEboxplot <- ggproto("StatEboxplot", Stat,
  required_aes = c("x", "y"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    params$width <- params$width %||% (resolution(data$x) * 0.75)

    if (is.double(data$x) && !has_groups(data) && any(data$x != data$x[1L])) {
      warning(
        "Continuous x aesthetic -- did you forget aes(group=...)?",
        call. = FALSE)
    }
    params
  },

  compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5, 
    shade.upper=NA,
    shade.lower=NA,
    shade.fill=NULL,
    shade.value.absolute=FALSE
    ) {

    if (!shade.value.absolute){
      if (is.na(shade.upper)) shade.upper = 0.975
      if (is.na(shade.lower)) shade.lower = 0.025
      shade.upper.q <- shade.upper
      shade.lower.q <- shade.lower
    } else {
      if (is.na(shade.upper)) shade.upper = max(data$y)
      if (is.na(shade.lower)) shade.lower = min(data$y)
      shade.lower.q <- 0.025
      shade.upper.q <- 0.975
    }

    qs = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
    n_qs <- length(qs)

    qs_all <- c(qs, 0.025, 0.50, 0.975, shade.lower.q, shade.upper.q)
    if (!is.null(data$weight)) {
      mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs_all)
      quants <- as.numeric(stats::coef(mod))
    } else {
      quants <- as.numeric(stats::quantile(data$y, qs_all))
    }
    tt <- quants[1:n_qs]
    p025 <- quants[n_qs+1]
    p500 <- quants[n_qs+2]
    p975 <- quants[n_qs+3]
    shade.lower.p <- quants[n_qs+4]
    shade.upper.p <- quants[n_qs+5]

    if (!shade.value.absolute){
      shade.lower <- shade.lower.p
      shade.upper <- shade.upper.p
    }

    shade.upper <- ifelse(shade.upper > p975, p975, shade.upper)
    shade.upper <- ifelse(shade.upper < p025, p025, shade.upper)
    shade.lower <- ifelse(shade.lower > p975, p975, shade.lower)
    shade.lower <- ifelse(shade.lower < p025, p025, shade.lower)

    if (shade.lower > shade.upper) shade.upper = shade.lower

    stats <- range(data$y)
    stats <- c(stats, p500)
    names(stats) <- c("ymin", "ymax", "ymedian")
    outliers <- data$y < p025 | data$y > p975

    metadata <- data.frame(qs = qs,
      ttx = c(0.05, 0.1, 0.2, 0.5, 1, 0.5, 0.2, 0.1, 0.05),
      tt = tt)
    metadata$id <- 1:length(qs)

    metadata_s <- data.frame(qs = qs,
      ttx = c(0.05, 0.1, 0.2, 0.5, 1, 0.5, 0.2, 0.1, 0.05),
      tt = tt)
    metadata_s$id <- 1:length(qs)
    metadata_s <- trim_data(metadata_s, shade.lower, shade.upper)
    if (length(unique(data$x)) > 1)
      width <- diff(range(data$x)) * 0.9

    df <- as.data.frame(as.list(stats))
    df$outliers <- list(data$y[outliers])

    df$tt <- list(metadata$tt)
    df$ttx <- list(metadata$ttx)
    df$id <- list(metadata$id)

    df$tt_s <- list(metadata_s$tt)
    df$ttx_s <- list(metadata_s$ttx)
    df$id_s <- list(metadata_s$id)

    if (is.null(data$weight)) {
      n <- sum(!is.na(data$y))
    } else {
      # Sum up weights for non-NA positions of y and weight
      n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
    }

    df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
    df$width <- width
    df$relvarwidth <- sqrt(n)
    df
  }
)


geom_eboxplot <- function(mapping = NULL, data = NULL,
                         stat = "eboxplot", position = "dodge2",
                         ...,
                         outlier.show = TRUE,
                         outlier.colour = NULL,
                         outlier.color = NULL,
                         outlier.fill = NULL,
                         outlier.shape = 19,
                         outlier.size = 1.5,
                         outlier.stroke = 0.5,
                         outlier.alpha = NULL,
                         median.show=TRUE,
                         median.font.size = 3.88,
                         median.font.angle = 0,
                         median.digits=0,
                         percent.show.group=NULL,
                         percent.show.side=1,
                         percent.font.size=3.88,
                         percent.font.angle=0,
                         percent.side.offset=0.1,
                         percent.font.color="black",
                         varwidth = FALSE,
                         shade.upper = NA,
                         shade.lower = NA,
                         shade.fill = "pink",
                         shade.alpha= 1,
                         shade.value.absolute = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  # varwidth = TRUE is not compatible with preserve = "total"
  if (!is.character(position)) {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      warning("Can't preserve total widths when varwidth = TRUE.", call. = FALSE)
      position$preserve <- "single"
    }
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomEboxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.show = outlier.show,
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      median.show=median.show,
      median.font.size = median.font.size,
      median.font.angle=median.font.angle,
      median.digits=median.digits,
      percent.show.group=percent.show.group,
      percent.show.side=percent.show.side,
      percent.font.size=percent.font.size,
      percent.font.angle=percent.font.angle,
      percent.font.color=percent.font.color,
      percent.side.offset=percent.side.offset,
      varwidth = varwidth,
      na.rm = na.rm,
      shade.upper=shade.upper,
      shade.lower=shade.lower,
      shade.fill=shade.fill,
      shade.alpha=shade.alpha,
      shade.value.absolute=shade.value.absolute,
      ...
    )
  )
}


GeomEboxplot <- ggproto("GeomEboxplot", Geom,
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

    if (!is.null(data$outliers)) {
      suppressWarnings({
        out_min <- vapply(data$outliers, min, numeric(1))
        out_max <- vapply(data$outliers, max, numeric(1))
      })
    }

    # if `varwidth` not requested or not available, don't use it
    if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(data$relvarwidth)) {
      data$xmin <- data$x - data$width / 2
      data$xmax <- data$x + data$width / 2
    } else {
      # make `relvarwidth` relative to the size of the largest group
      data$relvarwidth <- data$relvarwidth / max(data$relvarwidth)
      data$xmin <- data$x - data$relvarwidth * data$width / 2
      data$xmax <- data$x + data$relvarwidth * data$width / 2
    }
    data$width <- NULL
    if (!is.null(data$relvarwidth)) data$relvarwidth <- NULL

    data
  },

  draw_group = function(data, panel_params, coord, fatten = 2,
                        outlier.show = TRUE,
                        outlier.colour = NULL, 
                        outlier.fill = NULL,
                        outlier.shape = 19,
                        outlier.size = 1.5, 
                        outlier.stroke = 0.5,
                        outlier.alpha = NULL, 
                        median.show=TRUE,
                        median.font.size = 3.88, 
                        median.font.angle=0, 
                        median.digits=0,
                        percent.show.group=NULL,
                        percent.show.side=1,
                        percent.font.size=3.88,
                        percent.font.angle=0,
                        percent.font.color="black",
                        percent.side.offset=0.1,
                        varwidth = FALSE, 
                        shade.fill="pink", 
                        shade.alpha=1,
                        shade.upper=NA, 
                        shade.lower=NA
                        ) {

    if (is.na(shade.lower) & is.na(shade.upper)) {
      shade.draw = FALSE
    } else {
      shade.draw =TRUE
    }

    common <- data.frame(
      colour = data$colour,
      size = data$size,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      stringsAsFactors = FALSE
    )
    ttx <- data$ttx[[1]]
    width <- data$xmax - data$xmin

    polygon <- data.frame(x = c(data$x - ttx*width/2, data$x + ttx[order(-data$id[[1]])]*width/2),
      y = c(data$tt[[1]], data$tt[[1]][order(-data$id[[1]])]),
      alpha = data$alpha,
      side = c(rep(1,9),rep(2,9)),
      id = c(1:9, 9:1),
      common,
      stringsAsFactors = FALSE
      )
    polygon_glob <- GeomPolygon$draw_panel(polygon, panel_params, coord)
    polygon$fill = alpha(data$colour, 0)
    polygon_redraw_glob <- GeomPolygon$draw_panel(polygon, panel_params, coord)

    if (is.null(percent.show.group)){
      percent_glob = NULL
    } else {
      group = polygon$group[1]
      if (group %in% percent.show.group){
        percent_side = subset(polygon, side == percent.show.side)
        percent_side$x = percent_side$x + percent.side.offset*(-1)^percent.show.side
        percent_side$size = percent.font.size
        percent_side$angle = percent.font.angle
        percent_side$colour = percent.font.color
        percent_side$label = c("2.5%","5%","10%","25%","50%","75%","90%","95%","97.5%")
        percent_side = percent_side[order(data$id[[1]]),]
        percent_glob = GeomText$draw_panel(percent_side, panel_params, coord)    
      } else {
        percent_glob = NULL
      }
    }

    ttx_s <- data$ttx_s[[1]]
    if (shade.draw==TRUE){
      polygon_s <- data.frame(x = c(data$x - ttx_s*width/2, data$x + ttx_s[order(-data$id_s[[1]])]*width/2),
        y = c(data$tt_s[[1]], data$tt_s[[1]][order(-data$id_s[[1]])]),
        alpha = data$alpha,
        common,
        stringsAsFactors = FALSE
        )
      polygon_s$fill = alpha(shade.fill, shade.alpha)
      polygon_s$colour = alpha(data$colour, 0)
      shade_glob <- GeomPolygon$draw_panel(polygon_s, panel_params, coord)
    } else {
      shade_glob <- NULL
    }

    segments <- data.frame(
        x = data$x-ttx*width/2,
        xend = data$x+ttx*width/2,
        y = data$tt[[1]],
        yend = data$tt[[1]],
        alpha = NA,
        common,
        ttx = ttx,
        stringsAsFactors = FALSE
      )

    median_gap_coef = ifelse(median.show, 0.3, 1)

    segments_median <- subset(segments, ttx == 1)
    segments_median_a <- segments_median_b <- segments_median
    segments_median_a$xend = with(segments_median_a, x + (xend-x)*median_gap_coef)
    segments_median_b$x = with(segments_median_b, xend - (xend-x)*median_gap_coef)
    
    segments <- rbind(subset(segments, ttx != 1),
      segments_median_a,
      segments_median_b
      )

    if (median.show){
      median <- data.frame(
        x = data$x,
        y = data$ymedian,
        label = sprintf(paste0("%0.", median.digits, "f"), data$ymedian),
        colour=data$colour,
        group=data$group,
        alpha = NA,
        angle=median.font.angle,
        size=median.font.size)
      median_glob <- GeomText$draw_panel(median, panel_params, coord)
    }else{
      median_glob <- NULL
    }

    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1) && outlier.show == TRUE) {
      outliers <- data.frame(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier.colour %||% data$colour[1],
        fill = outlier.fill %||% data$fill[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = NA,
        alpha = outlier.alpha %||% data$alpha[1],
        stringsAsFactors = FALSE
      )
      outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    } else {
      outliers_grob <- NULL
    }
    ggname("geom_eboxplot", grobTree(
      outliers_grob,
      polygon_glob,
      shade_glob,
      polygon_redraw_glob,
      GeomSegment$draw_panel(segments, panel_params, coord),
      percent_glob,
      median_glob
    ))

  },

  draw_key = draw_key_eboxplot,

  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
    alpha = NA, shape = 19, linetype = "solid"),

  required_aes = c("x", "ymin", "ymax")
)
