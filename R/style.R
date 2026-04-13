#' ggplot theme for IdEst
#'
#' @description
#' This theme is used by Adrien Taudiere [IdEst](https://adrientaudiere.com/).
#'  Based on [hrbrthemes](https://github.com/hrbrmstr/hrbrthemes/tree/master)
#' `hrbrthemes::theme_ipsum()` by boB Rudis.
#'
#' @param sans_family Font family for sans serif text (default is
#'  "Roboto Condensed" on Windows and "Roboto Condensed Light" on other OS).
#' @param serif_family Font family for serif text (default is "Linux Libertine G").
#' @param mono_family Font family for monospaced text (default is "Fira Code").
#' @param base_size Base font size (default is 11.5).
#' @param plot_title_family Font family for title (default is serif_family).
#' @param plot_title_size Font size for title (default is 18).
#' @param plot_title_face Font face for title (default is "bold").
#' @param plot_title_margin Margin below title (default is 10).
#' @param subtitle_family Font family for subtitle (default is serif_family).
#' @param subtitle_size Font size for subtitle (default is 13).
#' @param subtitle_face Font face for subtitle (default is "plain").
#' @param subtitle_margin Margin below subtitle (default is 15).
#' @param subtitle_color Font color for subtitle (default is "grey30").
#' @param strip_text_family Font family for facet strip text (default is mono_family).
#' @param strip_text_size Font size for facet strip text (default is 13).
#' @param strip_text_face Font face for facet strip text (default is "plain").
#' @param strip_back_grey Logical, whether to use grey background for facet strips
#'   (default is FALSE).
#' @param caption_family Font family for caption (default is sans_family).
#' @param caption_size Font size for caption (default is 9).
#' @param caption_face Font face for caption (default is "plain").
#' @param caption_margin Margin above caption (default is 10).
#' @param axis_text_size Font size for axis text (default is 80\% of base_size).
#' @param axis_text_family  Font family for axis text (default is sans_family).
#' @param axis_title_family Font family for axis titles (default is mono_family).
#' @param axis_title_size Font size for axis titles (default is 12).
#' @param axis_title_face Font face for axis titles (default is "plain").
#' @param axis_title_just Justification for axis titles (default is "c" for center).
#' @param plot_margin Margin around the plot (default is margin(12, 12, 12, 12)).
#' @param panel_spacing Spacing between panels (default is unit(1.2, "lines")).
#' @param grid_col Color for grid lines (default is "#cccccc").
#' @param grid Logical or character, whether to show grid lines (default is TRUE).
#' @param axis_col Color for axis lines (default is "#cccccc").
#' @param axis Logical or character, whether to show axis lines (default is FALSE).
#' @param ticks Logical, whether to show axis ticks (default is FALSE).
#' @param x_is_species_name Logical (default FALSE). If TRUE, automatically
#'   apply [scale_x_italic_species()] so that binomial species names on the
#'   x-axis are rendered in italic.  When set, the function returns a list
#'   (theme + scale) rather than a bare theme; ggplot2 unpacks the list
#'   automatically when added with `+`.
#' @param y_is_species_name Logical (default FALSE). Same as
#'   `x_is_species_name` but for the y-axis.
#'
#' @return When both `x_is_species_name` and `y_is_species_name` are `FALSE`
#'   (the default), a ggplot2 theme object.  When either flag is `TRUE`, a
#'   list containing the theme object and one or two
#'   `scale_*_discrete(labels = label_italic_species)` objects, which ggplot2
#'   automatically unpacks when added with `+`.
#' @author Adrien Taudiere
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_idest()
#'
#' # Italic species names on y-axis (e.g. after coord_flip())
#' df <- data.frame(
#'   sp = c("Russula nigricans", "Amanita", "Boletus edulis"),
#'   n  = c(10, 5, 8)
#' )
#' ggplot(df, aes(x = n, y = reorder(sp, n))) +
#'   geom_col() +
#'   theme_idest(y_is_species_name = TRUE)
theme_idest <- function(
  sans_family = if (.Platform$OS.type == "windows") {
    "Roboto Condensed"
  } else {
    "Roboto Condensed Light"
  },
  serif_family = "Linux Libertine G",
  mono_family = "Fira Code",
  base_size = 11.5,
  plot_title_family = serif_family,
  plot_title_size = 18,
  plot_title_face = "bold",
  plot_title_margin = 10,
  subtitle_family = serif_family,
  subtitle_size = 13,
  subtitle_face = "plain",
  subtitle_margin = 15,
  subtitle_color = "grey30",
  strip_text_family = mono_family,
  strip_text_size = 13,
  strip_text_face = "plain",
  strip_back_grey = FALSE,
  caption_family = sans_family,
  caption_size = 9,
  caption_face = "plain",
  caption_margin = 10,
  axis_text_size = base_size * 0.8,
  axis_text_family = sans_family,
  axis_title_family = mono_family,
  axis_title_size = 12,
  axis_title_face = "plain",
  axis_title_just = "c",
  plot_margin = margin(12, 12, 12, 12),
  panel_spacing = grid::unit(1.2, "lines"),
  grid_col = "#cccccc",
  grid = TRUE,
  axis_col = "#cccccc",
  axis = FALSE,
  ticks = FALSE,
  x_is_species_name = FALSE,
  y_is_species_name = FALSE
) {
  ret <- theme_minimal(base_family = sans_family, base_size = base_size)

  ret <- ret + theme(legend.background = element_blank())
  ret <- ret + theme(legend.key = element_blank())

  ret <- ret + theme(plot.margin = plot_margin)
  ret <- ret + theme(panel.spacing = panel_spacing)

  if (inherits(grid, "character") || grid) {
    ret <- ret +
      theme(panel.grid = element_line(color = grid_col, linewidth = 0.2))
    ret <- ret +
      theme(panel.grid.major = element_line(color = grid_col, linewidth = 0.2))
    ret <- ret +
      theme(panel.grid.minor = element_line(color = grid_col, linewidth = 0.15))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) {
        ret <- ret + theme(panel.grid.major.x = element_blank())
      }
      if (regexpr("Y", grid)[1] < 0) {
        ret <- ret + theme(panel.grid.major.y = element_blank())
      }
      if (regexpr("x", grid)[1] < 0) {
        ret <- ret + theme(panel.grid.minor.x = element_blank())
      }
      if (regexpr("y", grid)[1] < 0) {
        ret <- ret + theme(panel.grid.minor.y = element_blank())
      }
    }
  } else {
    ret <- ret + theme(panel.grid = element_blank())
    ret <- ret + theme(panel.grid.major = element_blank())
    ret <- ret + theme(panel.grid.major.x = element_blank())
    ret <- ret + theme(panel.grid.major.y = element_blank())
    ret <- ret + theme(panel.grid.minor = element_blank())
    ret <- ret + theme(panel.grid.minor.x = element_blank())
    ret <- ret + theme(panel.grid.minor.y = element_blank())
  }

  if (inherits(axis, "character") || axis) {
    ret <- ret +
      theme(axis.line = element_line(color = axis_col, linewidth = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x = element_blank())
      } else {
        ret <- ret +
          theme(axis.line.x = element_line(color = axis_col, linewidth = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y = element_blank())
      } else {
        ret <- ret +
          theme(axis.line.y = element_line(color = axis_col, linewidth = 0.15))
      }
    } else {
      ret <- ret +
        theme(axis.line.x = element_line(color = axis_col, linewidth = 0.15))
      ret <- ret +
        theme(axis.line.y = element_line(color = axis_col, linewidth = 0.15))
    }
  } else {
    ret <- ret + theme(axis.line = element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(linewidth = 0.15))
    ret <- ret + theme(axis.ticks.x = element_line(linewidth = 0.15))
    ret <- ret + theme(axis.ticks.y = element_line(linewidth = 0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(
    tolower(substr(axis_title_just, 1, 1)),
    b = 0,
    l = 0,
    m = 0.5,
    c = 0.5,
    r = 1,
    t = 1
  )
  yj <- switch(
    tolower(substr(axis_title_just, 2, 2)),
    b = 0,
    l = 0,
    m = 0.5,
    c = 0.5,
    r = 1,
    t = 1
  )

  ret <- ret +
    theme(
      axis.text = element_text(
        size = axis_text_size,
        family = axis_text_family,
        margin = margin(t = 0, r = 0)
      )
    )

  ret <- ret +
    theme(
      axis.title = element_text(
        size = axis_title_size,
        family = axis_title_family
      )
    )
  ret <- ret +
    theme(
      axis.title.x = element_text(
        hjust = xj,
        size = axis_title_size,
        family = axis_title_family,
        face = axis_title_face
      )
    )
  ret <- ret +
    theme(
      axis.title.y = element_text(
        hjust = yj,
        size = axis_title_size,
        family = axis_title_family,
        face = axis_title_face
      )
    )
  ret <- ret +
    theme(
      axis.title.y.right = element_text(
        hjust = yj,
        size = axis_title_size,
        angle = 90,
        family = axis_title_family,
        face = axis_title_face
      )
    )

  ret <- ret +
    theme(
      strip.text = element_text(
        hjust = 0,
        size = strip_text_size,
        face = strip_text_face,
        family = strip_text_family
      )
    )

  ret <- ret +
    theme(
      plot.title = element_text(
        hjust = 0,
        size = plot_title_size,
        margin = margin(b = plot_title_margin),
        family = plot_title_family,
        face = plot_title_face
      )
    )
  ret <- ret +
    theme(
      plot.subtitle = element_text(
        hjust = 0,
        size = subtitle_size,
        margin = margin(b = subtitle_margin),
        family = subtitle_family,
        face = subtitle_face,
        color = subtitle_color
      )
    )
  ret <- ret +
    theme(
      plot.caption = element_text(
        hjust = 1,
        size = caption_size,
        margin = margin(t = caption_margin),
        family = caption_family,
        face = caption_face
      )
    )

  if (strip_back_grey) {
    ret <- ret +
      theme(
        strip.background = element_rect(fill = "grey90", color = NA),
        panel.border = element_rect(color = "grey90", fill = NA)
      )
  }

  if (!x_is_species_name && !y_is_species_name) {
    return(ret)
  }
  scales <- list()
  if (x_is_species_name) {
    message(
      "theme_idest: applying italic species labels to the x-axis. ",
      "If species names are on the y-axis (e.g. horizontal bar charts), ",
      "use `y_is_species_name = TRUE` instead."
    )
    scales <- c(scales, list(scale_x_italic_species()))
  }
  if (y_is_species_name) {
    message(
      "theme_idest: applying italic species labels to the y-axis. ",
      "If species names are on the x-axis, use `x_is_species_name = TRUE` instead."
    )
    scales <- c(scales, list(scale_y_italic_species()))
  }
  c(list(ret), scales)
}


#' IdEst color palettes
#'
#' @description
#'  Palettes of color for IdEst including also
#'  some palettes from MoMAColors
#'   <https://github.com/BlakeRMills/MoMAColors/blob/main/R/Functions.R>
#'
#'  The available palette are c("all_color_idest", "ligth_color_idest",
#'  "dark_color_idest", "Picabia", "Picasso", "Levine2", "Rattner", "Sidhu",
#'  "Hokusai2", "Hokusai3")
#' @export
#' @author Adrien Taudiere
idest_pal <- list(
  all_color_idest = list(
    c(
      "#dc863b",
      "#faefd1",
      "#2e7891",
      "#8a9da4",
      "#aa4c26",
      "#c8a734",
      "#a6d3e3",
      "#003f5f",
      "#ba63b6",
      "#f20040",
      "#774fa0",
      "#b4dfa7"
    ),
    c(1:11),
    colorblind = FALSE
  ),
  ligth_color_idest = list(
    c("#faefd1", "#a6d3e3", "#b4dfa7", "#8a9da4"),
    c(1:4),
    colorblind = FALSE
  ),
  dark_color_idest = list(
    c(
      "#dc863b",
      "#2e7891",
      "#aa4c26",
      "#c8a734",
      "#003f5f",
      "#ba63b6",
      "#f20040",
      "#774fa0"
    ),
    c(1:8),
    colorblind = FALSE
  ),
  Picabia = list(
    c(
      "#53362e",
      "#744940",
      "#9f7064",
      "#c99582",
      "#e6bcac",
      "#e2d8d6",
      "#a5a6ae",
      "#858794",
      "#666879",
      "#515260",
      "#3d3d47"
    ),
    c(10, 4, 8, 1, 6, 3, 7, 2, 9, 5, 11),
    colorblind = TRUE
  ),
  Picasso = list(
    c(
      "#d5968c",
      "#c2676d",
      "#5c363a",
      "#995041",
      "#45939c",
      "#0f6a81"
    ),
    c(6, 3, 4, 2, 1, 5),
    colorblind = TRUE
  ),
  Levine2 = list(
    c(
      "#E3C1CB",
      "#AD5A6B",
      "#C993A2",
      "#365C83",
      "#384351",
      "#4D8F8B",
      "#CDD6AD"
    ),
    c(7, 1, 5, 3, 6, 2, 4),
    colorblind = TRUE
  ),
  Rattner = list(
    c(
      "#de8e69",
      "#f1be99",
      "#c1bd38",
      "#7a9132",
      "#4c849a",
      "#184363",
      "#5d5686",
      "#a39fc9"
    ),
    c(1, 5, 6, 2, 3, 7, 8, 4),
    colorblind = TRUE
  ),
  Sidhu = list(
    c(
      "#af4646",
      "#762b35",
      "#005187",
      "#251c4a",
      "#78adb7",
      "#4c9a77",
      "#1b7975"
    ),
    c(5, 2, 6, 7, 3, 4, 1),
    colorblind = TRUE
  ),
  Hokusai2 = list(
    c("#abc9c8", "#72aeb6", "#4692b0", "#2f70a1", "#134b73", "#0a3351"),
    c(5, 2, 4, 1, 6, 3),
    colorblind = TRUE
  ), # copy from https://github.com/BlakeRMills/MetBrewer/blob/main/R/PaletteCode.R
  Hokusai3 = list(
    c("#d8d97a", "#95c36e", "#74c8c3", "#5a97c1", "#295384", "#0a2e57"),
    c(4, 2, 5, 3, 1, 6),
    colorblind = TRUE
  ) # copy from https://github.com/BlakeRMills/MetBrewer/blob/main/R/PaletteCode.R
)

#' IdEst continuous color scales for ggplot2
#'
#' @param palette_name The name of the palette to use.
#'  The available palette are c("all_color_idest", "ligth_color_idest",
#'  "dark_color_idest", "Picabia", "Picasso", "Levine2", "Rattner", "Sidhu",
#'  "Hokusai2", "Hokusai3"). See [idest_pal] for more details.
#' @param direction Direction of the palette. 1 for standard, -1 for reversed.
#' @param ... Additional arguments passed to [ggplot2::scale_color_gradientn()].
#'
#' @returns A ggplot2 scale object.
#' @export
#' @author Adrien Taudiere
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = disp)) +
#'   geom_point() +
#'   scale_color_idest_c()
scale_color_idest_c <- function(
  palette_name = "all_color_idest",
  direction = 1,
  ...
) {
  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)) {
    stop(
      "Direction not valid. Please use 1 for standard palette or -1 for reversed palette."
    )
  }

  scale_color_gradientn(
    colors = idest_colors(
      palette_name = palette_name,
      direction = direction,
      override_order = FALSE
    ),
    ...
  )
}

#' IdEst continuous fill scales for ggplot2
#' @inheritParams scale_color_idest_c
#' @returns A ggplot2 scale object.
#' @export
#' @author Adrien Taudiere
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_idest_c("Picasso")
scale_fill_idest_c <- function(
  palette_name = "all_color_idest",
  direction = 1,
  ...
) {
  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)) {
    stop(
      "Direction not valid. Please use 1 for standard palette or -1 for reversed palette."
    )
  }

  scale_color_gradientn(
    colors = idest_colors(
      palette_name = palette_name,
      direction = direction,
      override_order = FALSE
    ),
    ...
  )
}

#' IdEst discrete color scales for ggplot2
#'
#' @param palette_name The name of the palette to use.
#'  The available palette are c("all_color_idest", "ligth_color_idest",
#'  "dark_color_idest", "Picabia", "Picasso", "Levine2", "Rattner", "Sidhu",
#'  "Hokusai2", "Hokusai3"). See [idest_pal] for more details.
#' @param direction Direction of the palette. 1 for standard, -1 for reversed.
#' @param override_order Logical (default FALSE),
#'   whether to override the order of the palette.
#' @param ... Additional arguments passed to [ggplot2::scale_color_gradientn()].
#' @export
#' @returns A ggplot2 scale object.
#' @author Adrien Taudiere
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_idest_d("dark_color_idest")
scale_color_idest_d <- function(
  palette_name = "all_color_idest",
  direction = 1,
  override_order = FALSE,
  ...
) {
  discrete_scale(
    aesthetics = "colour",
    scale_name = "moma_d",
    palette = function(n) {
      idest_colors(
        palette_name = palette_name,
        n = n,
        direction = direction,
        override_order = override_order
      )
    },
    ...
  )
}

#' IdEst discrete fill scales for ggplot2
#' @inheritParams scale_color_idest_d
#' @returns A ggplot2 scale object.
#' @export
#' @author Adrien Taudiere
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(class, fill = drv)) +
#'   geom_bar() +
#'   scale_fill_idest_d()
scale_fill_idest_d <- function(
  palette_name = "all_color_idest",
  direction = 1,
  override_order = FALSE,
  ...
) {
  discrete_scale(
    aesthetics = "fill",
    scale_name = "moma_d",
    palette = function(n) {
      idest_colors(
        palette_name = palette_name,
        n = n,
        direction = direction,
        override_order = override_order
      )
    },
    ...
  )
}


#' IdEst colors for ggplot theme_idest
#'
#' @param palette_name The name of the palette to use.
#' The available palette are c("all_color_idest", "ligth_color_idest",
#' "dark_color_idest", "Picabia", "Picasso", "Levine2", "Rattner", "Sidhu",
#' "Hokusai2", "Hokusai3"). See [idest_pal] for more details.
#' @param n Number of colors to return.
#' @param type Type of palette. Either "discrete" or "continuous".
#' @param direction Direction of the palette. 1 for standard, -1 for reversed.
#' @param override_order Logical, whether to override the order of the palette.
#'
#' @returns A vector of colors.
#' @author Adrien Taudiere
#' @export
#' @examples
#' idest_colors("Picasso", n = 3)
#' barplot(rep(1, 6), col = idest_colors("Hokusai2"), border = NA)
idest_colors <- function(
  palette_name = "all_color_idest",
  n,
  type = c("discrete", "continuous"),
  direction = c(1, -1),
  override_order = FALSE
) {
  `%notin%` <- Negate(`%in%`)

  palette <- idest_pal[[palette_name]]

  if (is.null(palette) || is.numeric(palette_name)) {
    stop("Palette does not exist.")
  }

  if (missing(n)) {
    n <- length(palette[[1]])
  }

  if (missing(direction)) {
    direction <- 1
  }

  if (direction %notin% c(1, -1)) {
    stop(
      "Direction not valid. Please use 1 for standard palette or -1 for reversed palette."
    )
  }

  if (missing(type)) {
    if (n > length(palette[[1]])) {
      type <- "continuous"
    } else {
      type <- "discrete"
    }
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(palette[[1]])) {
    stop(
      "Number of requested colors greater than what discrete palette can offer, \n use continuous instead."
    )
  }

  continuous <- if (direction == 1) {
    grDevices::colorRampPalette(palette[[1]])(n)
  } else {
    grDevices::colorRampPalette(rev(palette[[1]]))(n)
  }

  discrete <- if (direction == 1 && !override_order) {
    palette[[1]][which(palette[[2]] %in% c(1:n))]
  } else if (direction == -1 && !override_order) {
    rev(palette[[1]][which(palette[[2]] %in% c(1:n))])
  } else if (direction == 1 && override_order) {
    palette[[1]][1:n]
  } else {
    rev(palette[[1]])[1:n]
  }

  out <- switch(type, continuous = continuous, discrete = discrete)
  structure(out, class = "palette", name = palette_name)
}


#' Format taxon labels with species names in italic
#'
#' @description
#' Returns plotmath expressions that render binomial or trinomial species names
#' (labels containing a space) in italic, leaving single-word labels (genus,
#' family, etc.) unchanged.  Intended as a `labels` formatter for discrete
#' ggplot2 scales, or as a standalone helper.
#'
#' Uses plotmath `italic()` expressions instead of markdown, so no
#' `ggtext::element_markdown()` theme element is required.  This makes the
#' function compatible with any ggplot2 theme, including complete themes such
#' as [theme_idest()].
#'
#' @param x A character vector of taxon labels.
#'
#' @returns A list of plotmath expressions (for species names) and plain
#'   character strings (for non-species labels), suitable for use as `labels`
#'   in [ggplot2::scale_x_discrete()] or [ggplot2::scale_y_discrete()].
#' @export
#' @author Adrien Taudiere
#' @seealso [scale_x_italic_species()], [scale_y_italic_species()]
#' @examples
#' label_italic_species(c("Russula nigricans", "Amanita", "Boletus edulis"))
label_italic_species <- function(x) {
  lapply(x, function(lab) {
    if (grepl(" ", lab)) {
      parse(text = paste0("italic(\"", lab, "\")"))[[1]]
    } else {
      lab
    }
  })
}


#' Discrete x-axis scale with species names in italic
#'
#' @description
#' A drop-in replacement for [ggplot2::scale_x_discrete()] that automatically
#' renders binomial species names (labels containing a space) in italic using
#' plotmath expressions, while leaving single-word labels unchanged.
#' Works with any theme, including complete themes like [theme_idest()].
#'
#' @param ... Arguments passed to [ggplot2::scale_x_discrete()].
#'
#' @returns A `scale_x_discrete` ggplot2 scale object.
#' @export
#' @author Adrien Taudiere
#' @seealso [scale_y_italic_species()], [label_italic_species()]
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   sp = c("Russula nigricans", "Amanita", "Boletus edulis"),
#'   n  = c(10, 5, 8)
#' )
#' ggplot(df, aes(x = sp, y = n)) +
#'   geom_col() +
#'   theme_minimal() +
#'   scale_x_italic_species()
scale_x_italic_species <- function(...) {
  ggplot2::scale_x_discrete(labels = label_italic_species, ...)
}


#' Discrete y-axis scale with species names in italic
#'
#' @description
#' A drop-in replacement for [ggplot2::scale_y_discrete()] that automatically
#' renders binomial species names (labels containing a space) in italic using
#' plotmath expressions, while leaving single-word labels unchanged.
#' Works with any theme, including complete themes like [theme_idest()].
#'
#' @param ... Arguments passed to [ggplot2::scale_y_discrete()].
#'
#' @returns A `scale_y_discrete` ggplot2 scale object.
#' @export
#' @author Adrien Taudiere
#' @seealso [scale_x_italic_species()], [label_italic_species()]
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   sp = c("Russula nigricans", "Amanita", "Boletus edulis"),
#'   n  = c(10, 5, 8)
#' )
#' ggplot(df, aes(y = sp, x = n)) +
#'   geom_col() +
#'   theme_minimal() +
#'   scale_y_italic_species()
scale_y_italic_species <- function(...) {
  ggplot2::scale_y_discrete(labels = label_italic_species, ...)
}
