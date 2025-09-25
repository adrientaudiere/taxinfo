#' Plot the taxa occurence using gbif.range package
#'
#' @description
#' A wrapper of [gbif.range::get_gbif()] function to plot the range of taxa
#'   using ggplot2. The function can take either a phyloseq object or a vector of
#'   taxonomic names. If a phyloseq object is provided, the taxonomic names are
#'   extracted from the specified taxonomic rank.
#'   The occurrences are plotted either as points or using hexagonal binning.
#'   The function can also filter the occurrences by country.
#' @param physeq A phyloseq object
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'   The column(s) present in the @tax_table slot of the phyloseq object. Can
#'   be a vector of two columns (e.g. c("Genus", "Species")).
#' @param taxnames (A character vector of taxonomic names. If provided, physeq is ignored.)
#' @param interactive_plot (logical, default FALSE) If TRUE, an interactive map is created using the package mapview.
#' @param zcol (character vector, default c("year", "taxonomicStatus")) Only used if interactive_plot is TRUE. The column(s) of the occurrences to use for coloring the points in the interactive map. See ?mapview::mapview() for more details.
#' @param hexagons (logical, default FALSE) Only used if interactive_plot is
#'  FALSE. If TRUE, use hexagonal binning to plot the occurrences.
#'  If FALSE, plot the occurrences as points.
#' @param bins (Number of bins for hexagonal binning, default 100) Only used if hexagons is TRUE and interactive_plot is FALSE.
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param countries A character vector of country names to filter the occurrences. If NULL (default), all countries are used (no filter).
#' @param info_names (Character vector)
#'   The information to retrieve from GBIF for each occurrence. See
#'   [gbif.range::get_gbif()] for more details.
#' @param ... Additional arguments to pass to [gbif.range::get_gbif()].
#'
#' @returns A list of ggplot2 objects, one for each taxon.
#' @export
#'
#' @author Adrien Taudière
#'
#' @examples
#'
#' p <- plot_tax_gbif_pq(
#'   subset_taxa_pq(
#'     data_fungi_mini_cleanNames,
#'     taxa_sums(data_fungi_mini) > 20000
#'   ),
#'   hexagons = TRUE,
#'   verbose = TRUE, bins = 50, occ_samp = 100, grain = 10000
#' )
#'
#' p <- plot_tax_gbif_pq(taxnames = c("Xylobolus subpileatus", "Stereum #'  subpileatus"))
#'
#' p <- plot_tax_gbif_pq(taxnames = c("Stereum ostrea", "Mycena renati"))
#' requireNamespace(patchwork)
#' p[[1]] / p[[2]] & no_legend()
#'
#' p <- plot_tax_gbif_pq(taxnames = c("Stereum ostrea", "Mycena renati"), interactive_plot = TRUE)
#' p[[1]]
#'
#' p <- plot_tax_gbif_pq(taxnames = c("Xylobolus subpileatus", "Stereum  subpileatus"), hexagons = TRUE, verbose = F)
#'
#' p <- plot_tax_gbif_pq(taxnames = c("Xylobolus subpileatus", "Stereum #'  subpileatus"), hexagons = TRUE, verbose = F, countries = c("france", "spain"))
#'
#' p[[1]] + coord_fixed(ylim = c(30, 50), xlim = c(-5, 25)) + no_legend()
#'
#' p <- plot_tax_gbif_pq(
#'   taxnames = c(
#'     "Ossicaulis lachnopus",
#'     "Antrodiella brasiliensis", "Stereum ostrea", "Xylobolus subpileatus"
#'   ),
#'   hexagons = TRUE,
#'   verbose = F, bins = 50, occ_samp = 100, grain = 10000
#' )
#'
#' requireNamespace("patchwork")
#' (p[[1]] + p[[2]]) /
#'   (p[[3]] + p[[4]]) & no_legend()
#'
plot_tax_gbif_pq <- function(physeq = NULL,
                             taxonomic_rank = "currentCanonicalSimple",
                             taxnames = NULL,
                             interactive_plot = FALSE,
                             zcol = c("year", "taxonomicStatus"),
                             hexagons = FALSE,
                             bins = 100,
                             verbose = TRUE,
                             countries = NULL,
                             info_names = c(
                               "country", "country code",
                               "acceptedScientificName",
                               "ScientificName"
                             ), ...) {
  if (!is.null(taxnames) && !is.null(physeq)) {
    stop("You must specify either physeq or taxnames, not both")
  }
  if (is.null(taxnames)) {
    if (is.null(physeq)) {
      stop("You must specify either physeq or taxnames")
    }

    taxnames <- taxonomic_rank_to_taxnames(
      physeq = physeq,
      taxonomic_rank = taxonomic_rank,
      discard_genus_alone = TRUE
    )
  }

  p <- vector("list", length(taxnames))
  check_package("maps")
  world <- map_data("world")

  if(!is.null(countries)){
    check_package("rnaturalearth")
    countries_sv <- rnaturalearth::ne_states(countries, returnclass = "sv")
  }

  for (i in seq_along(taxnames)) {
    if (verbose) {
      print(paste0("Start the computation of ", taxnames[i]))
    }
    if (is.null(countries)) {
      tax_gbif <- gbif.range::get_gbif(taxnames[i], ...)
    } else {
       tax_gbif <- gbif.range::get_gbif(taxnames[i], geo = countries_sv, info_names = info_names, ...)
    }

    if (interactive_plot) {
      check_package("mapview")
      check_package("htmltools")

      tax_sf <- sf::st_as_sf(tax_gbif, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
      p[[i]] <- mapview::mapview(tax_sf,
        zcol = zcol,
        map.types = c("CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery", "OpenTopoMap", "Stadia.StamenWatercolor", "Esri.NatGeoWorldMap"), popup = leafpop::popupTable(tax_sf)
      )

      title_p_text <- taxa_summary_text(physeq,
        taxonomic_rank = taxonomic_rank,
        taxnames = taxnames[i], verbose = FALSE
      )

      title_p <- htmltools::tags$div(
        htmltools::tags$h3(taxnames[i],
          style = "color: black;
                   text-align: center;
                   font-family: Arial;
                   font-size: 16px;
                   margin: 0;
                   padding: 2px 15px;
    font-style: italic;"
        ),
        htmltools::tags$p(gsub(paste0(taxnames[i], ": "), "", title_p_text),
          style = "color: black;
                   text-align: center;
                   font-family: Arial;
                   font-size: 12px;
                   margin: 0;
                   padding: 2px 15px;
                   "
        ),
        style = "position: fixed;
           top: 10px;
           left: 50%;
           transform: translateX(-50%);
           z-index: 1000;
           border-radius: 5px;
                     background-color: rgba(255, 255, 255, 0.9);
  box-shadow: 0 2px 4px rgba(0,0,0,0.2);"
      )
      check_package("leaflet")
      p[[i]]@map <- p[[i]]@map |>
        leaflet::addControl(html = as.character(title_p), position = "topleft")
    } else {
      p[[i]] <- ggplot(data = world, aes(x = long, y = lat)) +
        geom_polygon(
          aes(group = group),
          fill = "grey80",
          color = "black",
          linewidth = 0.1
        ) +
        labs(
          title = bquote(italic(.(taxnames[i]))),
          subtitle = bquote("Accepted name:" * " " * italic(.(
            unique(tax_gbif$acceptedScientificName)
          )))
        )

      if (hexagons) {
        p[[i]] <- p[[i]] +
          geom_hex(
            data = tax_gbif,
            bins = bins,
            aes(x = decimalLongitude, y = decimalLatitude, color = basisOfRecord),
            alpha = 0.8,
            linewidth = 0.4
          )
      } else {
        p[[i]] <- p[[i]] + geom_point(
          data = tax_gbif,
          aes(x = decimalLongitude, y = decimalLatitude, color = basisOfRecord),
          alpha = 0.8
        )
      }
    }
  }
  return(p)
}
