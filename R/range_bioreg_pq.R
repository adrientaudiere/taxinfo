#' Get and plot the range of taxa within a bioregion using gbif.range package
#' @description
#' A wrapper of [gbif.range::get_gbif()] and [gbif.range::get_range()] functions
#'   to get and plot the range of taxa using ggplot2.
#'   The function takes a phyloseq object as input and  extracts the taxonomic
#'   names from the specified  taxonomic rank.
#'   The occurrences are plotted as points on a map, along with the range of
#'   the taxon within a specified bioregion. The bioregion used is "eco_terra",
#'   which corresponds to terrestrial ecoregions defined by the World Wildlife
#'  Fund (WWF).
#'
#' @aliases plot_range_bioreg_pq
#' @param physeq (required) A phyloseq object
#' @param taxonomic_rank (Character, default "currentCanonicalSimple")
#'  The column(s) present in the @tax_table slot of the phyloseq object. Can
#'  be a vector of two columns (e.g. c("Genus", "Species")).
#' @param occ_samp (Numeric, default 5000) Number of occurrences to sample from GBIF.
#'   See [gbif.range::get_gbif()] for more details.
#' @param verbose (logical, default TRUE) If TRUE, prompt some messages.
#' @param verbose_gbif_range (logical, default TRUE) If TRUE, prompt some
#'    messages from gbif.range functions.
#' @param make_plot (logical, default TRUE) If TRUE, return a list of ggplot objects. Else return a list of range outputs from [gbif.range::get_range()].
#' @param crop_plot (logical, default TRUE) If TRUE, crop the plot to the extent of the bioregion.
#' @param remove_legend (logical, default TRUE) If TRUE, remove the legend from the plot.
#' @param ... Additional arguments to pass to [gbif.range::get_gbif()].
#'
#' @details
#' [plot_range_bioreg_pq()] is a wrapper of just a shortcut for
#'  `range_bioreg_pq(..., make_plot = TRUE)`.
#'
#' @export
#' @return If make_plot = TRUE (default), a list of ggplot objects, one for each taxon.
#' If make_plot = FALSE, a list of range outputs from [gbif.range::get_range()].
#' @author Adrien Taudière
#' @examples
#'
#' res_range <- range_bioreg_pq(subset_taxa(
#'   data_fungi_mini_cleanNames,
#'   currentCanonicalSimple %in% c("Xylodon raduloides", "Basidiodendron eyrei")
#' ), occ_samp = 100)
#'
#' p <- plot_range_bioreg_pq(subset_taxa(
#'   data_fungi_mini_cleanNames,
#'   currentCanonicalSimple %in% c("Xylodon raduloides", "Basidiodendron eyrei")
#' ), occ_samp = 100)
#' p[[1]] / p[[2]]
#'
#' p <- plot_range_bioreg_pq(subset_taxa_pq(
#'   data_fungi_mini_cleanNames,
#'   taxa_sums(data_fungi_mini) > 20000
#' ), occ_samp = 500)
#'
#' requireNamespace(patchwork)
#' p[[1]] / p[[2]]
range_bioreg_pq <- function(physeq,
                            taxonomic_rank = "currentCanonicalSimple",
                            occ_samp = 5000,
                            verbose = TRUE,
                            verbose_gbif_range = FALSE,
                            make_plot = FALSE,
                            crop_plot = TRUE,
                            remove_legend = TRUE,
                            ...) {
  taxnames <- taxonomic_rank_to_taxnames(
    physeq = physeq,
    taxonomic_rank = taxonomic_rank,
    discard_genus_alone = TRUE,
    discard_NA = TRUE
  )

  gbif_taxa <- rgbif::name_backbone_checklist(taxnames) |>
    filter(matchType %in% c("EXACT", "HIGHERRANK")) |>
    select(-verbatim_index) |> # in order to duplicate
    distinct()

  eco.terra <- gbif.range::read_bioreg(bioreg_name = "eco_terra", save_dir = NULL)

  range_taxa_i_bioreg <- vector(mode = "list", length = length(taxnames))
  names(range_taxa_i_bioreg) <- taxnames
  p <- vector(mode = "list", length = length(taxnames))
  names(p) <- taxnames

  for (tax_i in taxnames) {
    if (verbose) {
      message("Find gbif occurence ", tax_i)
    }
    range_taxa_i <- gbif.range::get_gbif(tax_i, occ_samp = occ_samp, ...)
    if (verbose) {
      message("Start the computation of range of ", tax_i)
    }

    range_taxa_i_bioreg[[tax_i]] <- tryCatch(
      gbif.range::get_range(range_taxa_i,
        bioreg = eco.terra,
        bioreg_name = "ECO_NAME",
        verbose = verbose_gbif_range,
        raster = FALSE
      ),
      error = function(e) {
        if (verbose) {
          message(paste("Not enough occurence for", tax_i))
        }
        return(NULL)
      }
    )

    if (make_plot) {
      if (is.null(range_taxa_i_bioreg[[tax_i]])) {
        message(paste("Not enough occurence to plot", tax_i))
      } else {
        check_package("rnaturalearth")
        check_package("terra")

        countries <-
          rnaturalearth::ne_countries(type = "countries", returnclass = "sf")
        bb_bioreg <-
          as.vector(terra::ext(range_taxa_i_bioreg[[tax_i]]$rangeOutput))[]

        range_i_bioreg <- range_taxa_i_bioreg[[tax_i]]$rangeOutput |>
          sf::st_as_sf()

        p[[tax_i]] <- ggplot() +
          geom_sf(
            data = countries,
            fill = "#bcbddc",
            color = "white",
            size = 0.2
          ) +
          geom_sf(data = range_i_bioreg, fill = "slateblue", col = NA) +
          geom_point(
            data = range_taxa_i,
            aes(x = decimalLongitude, y = decimalLatitude),
            color = "#99340470",
            size = 1.5,
            alpha = 0.2
          ) +
          theme_void() +
          labs(
            title = bquote(italic(.(tax_i))),
            subtitle = taxa_summary_text(physeq,
              taxonomic_rank = taxonomic_rank,
              taxnames = tax_i, verbose = FALSE
            )
          )

        if (remove_legend) {
          p[[tax_i]] <- p[[tax_i]] +
            theme(
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.position = "none"
            )

          if (crop_plot) {
            p[[tax_i]] <- p[[tax_i]] +
              coord_sf(
                xlim = c(bb_bioreg[1], bb_bioreg[2]),
                ylim = c(bb_bioreg[3], bb_bioreg[4]), expand = FALSE
              )
          }
        }
      }
    }
  }

  if (make_plot) {
    return(p)
  } else {
    return(range_taxa_i_bioreg)
  }
}
#' @rdname range_bioreg_pq
#' @export
plot_range_bioreg_pq <- function(...) {
  range_bioreg_pq(..., make_plot = TRUE)
}
