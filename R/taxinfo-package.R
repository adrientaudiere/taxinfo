#' \code{taxinfo} package
#'
#' @name taxinfo-package
#' @import MiscMetabar
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "%", "id.", "match", "%>%", ".summary", "Taxa", "name", "URLencode", "across",
    "aes", "all_of", "alt_assign", "any_of", "arrange", "as_tibble", "basisOfRecord",
    "bind_rows", "blast_to_phyloseq", "canonicalName", "clean_pq", "collect",
    "content", "coord_sf", "count", "count_in_radius", "currentCanonicalSimple",
    "currentName", "data_fungi_mini_cleanNames", "dbConnect", "dbDisconnect",
    "decimalLatitude", "decimalLongitude", "desc", "distance_km", "distinct",
    "doi", "download.file", "duckdb", "element_blank", "everything", "filter",
    "full_join", "geom_hex", "geom_point", "geom_polygon", "geom_sf", "get_gbif",
    "get_range", "ggplot", "good_assign", "group", "group_by", "i",
    "interaction_type", "join_by", "labs", "lang", "lat", "left_join", "long",
    "map_data", "matchType", "mutate", "n", "n_doi", "na.omit", "name",
    "name_backbone", "nb", "nsamples", "ntaxa", "occ_count", "occ_search",
    "property_value", "pull", "radius_km", "read.csv", "read_bioreg", "relocate",
    "rename_with", "right_join", "row_number", "sample_data", "sample_name",
    "sample_names", "sample_sums", "select", "setNames", "site", "source_taxon_name",
    "st_as_sf", "st_drop_geometry", "st_intersection", "st_make_valid", "st_point",
    "status_code", "submittedName", "subset_samples_pq", "subset_taxa_pq", "summarise",
    "sym", "tags", "target_taxon_Canonical", "tax_table", "taxa_as_rows", "taxa_id_for_join",
    "taxa_name", "taxa_names", "Taxa name", "taxa_names<-", "", "", "taxa_summary_info", "taxa_sums",
    "taxnames_species", "tbl", "theme", "theme_void", "tib_list_page", "tibble", "type",
    "ungroup", "value", "verbatim_index", "verify_pq"
  ))
}

#' @keywords internal
#' @noRd
"_PACKAGE"
