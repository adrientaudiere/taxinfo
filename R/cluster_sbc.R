#' Create Species-Bound Clusters using SWARM algorithm
#' @description
#'
#' This function creates Species-Bound Clusters (SBC) from a phyloseq object
#' containing taxa (ASV/OTU) sequences and taxonomy based on a proposition
#' by Riley *et al.* 2025 (<https://doi.org/10.1186/s12915-025-02284-x>).
#'
#'  SBC (Species bound cluster) are defined as "clusters that include all and only
#'   ESVs assigned to one species, the sequence similarity threshold can vary
#'   between these clusters" (Riley *et al.* 2025
#'   <https://doi.org/10.1186/s12915-025-02284-x>).
#'
#'  It uses the SWARM algorithm to cluster
#' taxa within each taxnames (e.g. Genus species) based on sequence similarity,
#' allowing for variable d values to optimize clustering.
#'
#' Run swarm with d=1 to d=max_d, then for each taxnames (e.g. Species binomial name),
#'  find the lowest d that clusters all taxa assigned to this taxnames into one
#'  cluster. If a taxnames is represented by only one taxa, it is not clustered.
#'  Taxnames containig "NA" are considered as unassigned. By default, unassigned
#'  ASVs are clustered into other cluster without counting for their own taxnames.
#'  Set include_unassigned = FALSE to force cluster to included all taxa with a
#'  given taxnames but none of the unassigned ones.
#'
#'  If the maximum d is reached, keep the clustering at this d and print a warning.
#'  If strict_sbc = TRUE, only taxa corresponding to strict SBC will be clustered
#'  and return in the phyloseq object, in that cases, taxa whose taxnames is
#'  clustered into multiple clusters or whose cluster contains multiple taxnames
#'  will have NA as cluster_ID and will be removed from phyloseq object.
#'
#'  The function returns a data.frame with the cluster assignments and
#'  the optimal d value for each taxnames, as well as a modified phyloseq object
#'  with the cluster information added to the taxonomy table.
#'
#' @param physeq A phyloseq object containing ASV/OTU sequences and taxonomy
#' @param taxonomic_rank Character. Name of the taxonomy column(s) containing
#'   taxonomic assignments to build SBC. Can be a vector of two columns
#'   (e.g. c("Genus", "Species"), the default).
#' @param max_d Integer. Maximum d value to test for SWARM (default: 20)
#' @param include_unassigned Logical. Whether to cluster unassigned taxa
#'  separately (default: TRUE)
#' @param allow_multiple_taxa Logical. If TRUE, allow clusters to contain
#'  multiple taxnames (default: FALSE)
#' @param regroup_cluster Logical. If TRUE, regroup taxa in the phyloseq object
#'  based on their cluster_ID using [merge_taxa_vec()] (default: TRUE)
#' @param tax_adjust Character vector. See ?[MiscMetabar::merge_taxa_vec()]
#'  0: no adjustment; 1: phyloseq-compatible adjustment; 2: conservative adjustment
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @author Adrien Taudiere
#' @return A list containing:
#'   - clusters: data.frame with taxa_id, taxnames, cluster_ID, optimal_d
#'   - summary: data.frame with summary statistics
#'     - n_taxa: total number of taxa
#'     - n_unassigned: number of unassigned taxa
#'     - n_taxa: number of unique taxnames
#'     - n_already_SBC: number of taxnames already represented by a single taxa
#'     - n_taxa_to_cluster: number of taxnames with multiple taxa to cluster
#'     - n_SBC: number of SBC clusters created
#'   - d_per_taxnames: data.frame with taxnames, n_taxa, optimal_d, n_clusters,
#'     other_taxnames, unassigned_taxa
#'   - physeq_with_info: modified phyloseq object with cluster info added to tax_table
#'     - cluster_ID: The id of the SBC cluster
#'     - cluster_d: The optimal d value used to create the SBC cluster
#'     - other_taxnames_in_cluster (logical)
#'     - unassigned_taxa_in_cluster (logical)
#'   - physeq_SBC: modified phyloseq object with cluster info added to tax_table
#'
#' @seealso [MiscMetabar::swarm_clustering()], [MiscMetabar::postcluster_pq()]
#' @export
#' @examples
#' res <- cluster_sbc(data_fungi_mini)
#'
#' track_wkflow(list(data_fungi_mini, res$physeq_SBC))
#'
#' ggplot(
#'   res$d_per_taxnames,
#'   aes(x = reorder(taxnames, n_taxa), y = n_taxa, fill = optimal_d)
#' ) +
#'   geom_bar(stat = "identity") +
#'   geom_text(aes(label = paste0(other_taxnames)),
#'     hjust = -0.1, size = 3,
#'     fontface = "italic"
#'   ) +
#'   coord_flip() +
#'   scale_fill_viridis_c(option = "plasma") +
#'   labs(
#'     title = "Species-Bound Clusters with Optimal d Values",
#'     subtitle = "Labels depict taxonomic names clustered into SBC",
#'     x = "Species",
#'     y = "Number of Taxa",
#'     fill = "Optimal d"
#'   ) +
#'   theme(axis.text.y = element_text(size = 10, face = "italic"))
cluster_sbc <- function(
  physeq,
  taxonomic_rank = c("Genus", "Species"),
  max_d = 20,
  include_unassigned = TRUE,
  allow_multiple_taxa = FALSE,
  regroup_cluster = TRUE,
  tax_adjust = 1L,
  verbose = TRUE
) {
  taxnames <- apply(
    physeq@tax_table[, taxonomic_rank],
    1,
    paste,
    collapse = " "
  )
  taxnames[grepl("NA", taxnames)] <- "unassigned"

  taxtab <- as.data.frame(tax_table(physeq))
  if (sum(!taxonomic_rank %in% colnames(taxtab)) > 0) {
    stop("Column ", taxonomic_rank, " not found in taxonomy table")
  }

  taxa_ids <- taxa_names(physeq)

  cluster_results <- data.frame(
    taxa_id = taxa_ids,
    taxnames = taxnames,
    cluster_ID = NA_character_,
    optimal_d = NA_integer_,
    other_taxnames_in_cluster = FALSE,
    unassigned_taxa_in_cluster = FALSE,
    stringsAsFactors = FALSE
  )

  d_per_taxnames <- data.frame(
    taxnames = character(),
    n_sbc = integer(),
    optimal_d = integer(),
    n_clusters = integer(),
    other_taxnames = character(),
    unassigned_taxa = logical(),
    stringsAsFactors = TRUE
  )

  unique_taxnames <- unique(taxnames)
  unique_taxnames <- unique_taxnames[unique_taxnames != "unassigned"]

  # Pre-compute SWARM clustering for all d values
  if (verbose) {
    cli::cli_alert_info(
      "Computing SWARM clustering for d = 1 to {.val {max_d}} ..."
    )
    cli::cli_progress_bar("Computing SWARM", total = max_d)
    options(cli.progress_show_after = 0)
    options(cli.progress_clear = FALSE)
  }
  swarm_results_list <- list()

  for (d in 1:max_d) {
    if (verbose) {
      cli::cli_progress_update()
    }
    swarm_results_list[[d]] <-
      MiscMetabar::swarm_clustering(
        physeq = physeq,
        d = d,
        return_swarm_df = TRUE
      ) |>
      mutate(query_name = stringr::str_remove(query, "_\\d+$"))
  }
  if (verbose) {
    cli::cli_progress_done()
  }
  if (verbose) {
    cli::cli_alert_info("Finding optimal d values for each taxonomic names...")
  }

  for (taxn in unique_taxnames) {
    taxn_label <- taxn
    taxn_mask <- taxnames == taxn

    taxn_id <- taxa_ids[taxn_mask]
    n_taxn_id <- length(taxn_id)

    if (n_taxn_id == 1) {
      if (verbose) {
        cli::cli_alert_success(
          " {.emph {taxn_label}} is already represented by only one taxa"
        )
      }
      cluster_results$cluster_ID[cluster_results$taxa_id %in% taxn_id] <-
        swarm_results_list[[d]] |>
        filter(type != "C") |>
        filter(query_name %in% taxn_id) |>
        pull(cluster) |>
        paste0("_1")
      cluster_results$optimal_d[cluster_results$taxa_id %in% taxn_id] <- 0

      next
    }

    if (verbose) {
      cli::cli_alert_info(
        "Processing {.emph {taxn_label}} - {.val {n_taxn_id}} taxa"
      )
    }

    optimal_d <- NA_integer_
    for (d in 1:max_d) {
      swarm_tax <- as.data.frame(swarm_results_list[[d]])

      sbc_swarm_clusters <- swarm_tax |>
        filter(type != "C") |>
        filter(query_name %in% taxn_id) |>
        pull(cluster) |>
        unique()

      taxa_in_cluster <- swarm_tax |>
        filter(type != "C") |>
        filter(cluster %in% unique(sbc_swarm_clusters)) |>
        pull(query_name)

      taxnames_with_unassigned <- unique(taxnames[taxa_in_cluster])

      if (include_unassigned) {
        taxnames_in_cluster <- taxnames_with_unassigned[
          taxnames_with_unassigned != "unassigned"
        ]
      } else {
        taxnames_in_cluster <- taxnames_with_unassigned
      }

      if (d == max_d) {
        optimal_d <- max_d
        if (verbose) {
          cli::cli_alert_warning(
            "Max d reached. Using d = {.val {max_d}}, output {.val {length(sbc_swarm_clusters)}} clusters for {.emph {taxn_label}} emcompassing {.val {length(taxnames_in_cluster)}} taxa"
          )
        }
      }

      if (length(sbc_swarm_clusters) > 1) {
        next
      } else {
        if (length(taxnames_in_cluster) > 1) {
          optimal_d <- d
          if (verbose) {
            cli::cli_alert_warning(
              "Multiple taxa clustered into a single group at optimal d = {.val {d}} for {.emph {taxn_label}} with {.val {taxnames_in_cluster}} taxnames inside"
            )
          }
          break
        } else {
          optimal_d <- d

          if (verbose) {
            cli::cli_alert_success(
              "Found optimal d = {.val {d}} for {.emph {taxn_label}}."
            )
          }
          break
        }
      }
    }

    if (optimal_d < max_d) {
      cl_id <- cluster_results$taxa_id %in% taxa_in_cluster
      if (length(taxnames_in_cluster) == 1) {
        final_tax <- swarm_results_list[[optimal_d]] |>
          filter(type != "C") |>
          filter(cluster %in% sbc_swarm_clusters)

        cluster_results$cluster_ID[cl_id] <- paste(
          final_tax$cluster,
          optimal_d,
          sep = "_"
        )
        cluster_results$optimal_d[cl_id] <- optimal_d
        cluster_results$other_taxnames_in_cluster[cl_id] <- FALSE
        cluster_results$unassigned_taxa_in_cluster[cl_id] <-
          "unassigned" %in% taxnames_with_unassigned
      } else {
        final_tax <- swarm_results_list[[optimal_d]] |>
          filter(type != "C") |>
          filter(cluster %in% sbc_swarm_clusters)

        cluster_results$cluster_ID[cl_id] <- paste(
          final_tax$cluster,
          optimal_d,
          sep = "_"
        )
        cluster_results$optimal_d[cl_id] <- optimal_d
        cluster_results$other_taxnames_in_cluster[cl_id] <- TRUE
        cluster_results$unassigned_taxa_in_cluster[cl_id] <-
          "unassigned" %in% taxnames_with_unassigned
      }
      d_per_taxnames <- rbind(
        d_per_taxnames,
        data.frame(
          taxnames = taxn,
          n_taxa = n_taxn_id,
          optimal_d = optimal_d,
          n_clusters = length(unique(final_tax$cluster)),
          other_taxnames = paste(
            taxnames_in_cluster[
              !taxnames_in_cluster %in% c(taxn, "unassigned")
            ],
            collapse = ";"
          ),
          unassigned_taxa = "unassigned" %in% taxnames_with_unassigned,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # Add cluster information to phyloseq object
  cluster_tax <- data.frame(
    cluster_ID = cluster_results$cluster_ID,
    cluster_d = cluster_results$optimal_d,
    row.names = cluster_results$taxa_id,
    other_taxnames_in_cluster = cluster_results$other_taxnames_in_cluster,
    unassigned_taxa_in_cluster = cluster_results$unassigned_taxa_in_cluster
  )

  # Merge with existing taxonomy
  new_physeq <- physeq
  new_tax <- cbind(taxtab, cluster_tax)
  tax_table(new_physeq) <- as.matrix(new_tax)

  # Merge with existing taxonomy
  new_physeq_sbc <- subset_taxa(new_physeq, !is.na(cluster_d))

  if (regroup_cluster) {
    clusters <- as.character(new_physeq_sbc@tax_table[, "cluster_ID"])
    names(clusters) <- taxa_names(new_physeq_sbc)
    new_physeq_sbc_clust <-
      merge_taxa_vec(
        new_physeq_sbc,
        clusters,
        tax_adjust = tax_adjust,
        rank_propagation = FALSE
      ) |>
      clean_pq(silent = TRUE)
    if (verbose) {
      cli::cli_alert_success(
        "Taxa merged into {.val {ntaxa(new_physeq_sbc_clust)}} SBC clusters"
      )
    }
  } else {
    new_physeq_sbc_clust <- new_physeq_sbc
  }

  # Summary statistics
  n_new_sbc_clusters <- sum(!is.na(unique(cluster_tax$cluster_ID)))
  n_taxa <- nrow(cluster_results)
  n_unassigned <- sum(taxnames == "unassigned")
  n_already_SBC <- sum(cluster_results$optimal_d == 0, na.rm = TRUE)

  sum_df <- data.frame(
    n_taxnames = length(unique_taxnames),
    n_taxa = n_taxa,
    n_unassigned = n_unassigned,
    n_already_SBC = n_already_SBC,
    n_taxa_to_cluster = n_taxa - n_unassigned - n_already_SBC,
    n_new_SBC = n_new_sbc_clusters,
    n_SBC = n_new_sbc_clusters + n_already_SBC,
    mean_d = mean(d_per_taxnames$optimal_d, na.rm = TRUE),
    median_d = median(d_per_taxnames$optimal_d, na.rm = TRUE)
  ) |>
    mutate(
      avg_cluster_size = (n_taxa - n_unassigned) /
        (n_new_sbc_clusters + n_already_SBC),
      avg_cluster_size_excluding_singleton = (n_taxa -
        n_unassigned -
        n_already_SBC) /
        (n_new_sbc_clusters),
      .before = n_new_SBC
    )

  final_res <- list(
    clusters = cluster_results,
    d_per_taxnames = d_per_taxnames,
    physeq_with_info = new_physeq,
    physeq_SBC = new_physeq_sbc_clust,
    summary = sum_df
  )

  if (verbose) {
    cli::cli_alert_success("\n=== Clustering complete ===")
    cli::cli_alert_info("Total taxa: {.val {final_res$summary$n_taxa}}")
    cli::cli_alert_info(
      "Unassigned taxa: {.val {final_res$summary$n_unassigned}}"
    )
    cli::cli_alert_info(
      "Unique taxonomic names: {.val {final_res$summary$n_taxa}}"
    )
    cli::cli_alert_info(
      "Already single-taxa taxnames: {.val {final_res$summary$n_already_SBC}}"
    )
    cli::cli_alert_info(
      "Multiple-taxa taxnames clustered: {.val {final_res$summary$n_taxa_to_cluster}}"
    )
    cli::cli_alert_info(
      "Mean swarm d: {.val {round(final_res$summary$mean_d, 2)}}"
    )
    cli::cli_alert_info("Total SBC clusters: {.val {final_res$summary$n_SBC}}")
    cli::cli_alert_info(
      "Average taxa per SBC cluster {.val {round(final_res$summary$avg_cluster_size, 2)}}"
    )
  }

  return(final_res)
}
