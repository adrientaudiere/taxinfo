# Check for taxa occurrences within a radius around samples using GBIF data

This function performs a species range check for taxa contained in a
phyloseq object. The result can optionally be added to the phyloseq
object's tax_table as new columns.

## Usage

``` r
tax_occur_check_pq(
  physeq = NULL,
  taxnames = NULL,
  taxonomic_rank = "currentCanonicalSimple",
  longitude = NULL,
  latitude = NULL,
  radius_km = 50,
  n_occur = 1000,
  add_to_phyloseq = NULL,
  col_prefix = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- physeq:

  (optional) phyloseq object. Either \`physeq\` or \`taxnames\` must be
  provided, but not both. The phyloseq object containing the taxa to
  check.

- taxnames:

  (optional) A character vector of taxonomic names.

- taxonomic_rank:

  Character. The taxonomic rank to use for the check. Default is
  "currentCanonicalSimple" which corresponds to the cleaned scientific
  names in the phyloseq object if \[gna_verifier_pq()\] was used with
  default parameter.

- longitude:

  Numeric. Longitude of the test point in decimal degrees.

- latitude:

  Numeric. Latitude of the test point in decimal degrees.

- radius_km:

  Numeric. Search radius in kilometers (default: 50).

- n_occur:

  Numeric. Maximum number of occurrences to retrieve from GBIF for each
  taxon (default: 1000).

- add_to_phyloseq:

  (Logical, default TRUE when physeq is provided, FALSE when taxnames is
  provided). Whether to add the results as new columns in the phyloseq
  object's tax_table. If TRUE, the results will be appended to the
  tax_table with appropriate column names. Automatically set to TRUE
  when a phyloseq object is provided and FALSE when taxnames is
  provided. Cannot be TRUE if \`taxnames\` is provided.

- col_prefix:

  A character string to be added as a prefix to the new columns names
  added to the tax_table slot of the phyloseq object (default: NULL).

- verbose:

  (Logical, default: TRUE). Whether to print progress messages.

- ...:

  Additional parameters passed to \[tax_occur_check()\].

## Value

Either a data frame (if add_to_phyloseq = FALSE) or a new phyloseq
object (if add_to_phyloseq = TRUE).

## See also

\[tax_occur_check()\], \[tax_occur_multi_check_pq()\]

## Author

Adrien Taudiere

## Examples

``` r
check_res <- tax_occur_check_pq(data_fungi_mini_cleanNames,
  longitude = 2.3,
  latitude = 48,
  radius_km = 100,
  n_occur = 50,
  add_to_phyloseq = FALSE
)
#> Error: object 'data_fungi_mini_cleanNames' not found

check_res |>
  mutate(taxa_name = forcats::fct_reorder(taxa_name, count_in_radius)) |>
  ggplot(aes(x = count_in_radius, y = taxa_name, fill = total_count_in_world)) +
  geom_col()
#> Error: object 'check_res' not found

data_fungi_mini_cleanNames_range_verif <-
  tax_occur_check_pq(data_fungi_mini_cleanNames,
    longitude = 2.3,
    latitude = 48,
    radius_km = 50,
    n_occur = 10
  )
#> Error: object 'data_fungi_mini_cleanNames' not found

df <- data_fungi_mini_cleanNames_range_verif@tax_table[, "count_in_radius"] |>
  table(useNA = "always") |>
  data.frame()
#> Error: object 'data_fungi_mini_cleanNames_range_verif' not found

colnames(df) <- c("count_in_radius", "n_taxa")
#> Error in `colnames<-`(`*tmp*`, value = c("count_in_radius", "n_taxa")): attempt to set 'colnames' on an object with less than two dimensions
df
#> function (x, df1, df2, ncp, log = FALSE) 
#> {
#>     if (missing(ncp)) 
#>         .Call(C_df, x, df1, df2, log)
#>     else .Call(C_dnf, x, df1, df2, ncp, log)
#> }
#> <bytecode: 0x5621c379fc40>
#> <environment: namespace:stats>

# Subset taxa with at least one occurrence in the radius
cond_count_sup_0 <-
  data_fungi_mini_cleanNames_range_verif@tax_table[, "count_in_radius"] |>
    as.numeric() > 0
#> Error: object 'data_fungi_mini_cleanNames_range_verif' not found
cond_count_sup_0[is.na(cond_count_sup_0)] <- FALSE
#> Error: object 'cond_count_sup_0' not found
names(cond_count_sup_0) <- taxa_names(data_fungi_mini_cleanNames_range_verif)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'physeq' in selecting a method for function 'taxa_names': object 'data_fungi_mini_cleanNames_range_verif' not found

subset_taxa_pq(data_fungi_mini_cleanNames_range_verif, cond_count_sup_0) |>
  summary_plot_pq()
#> Error: object 'cond_count_sup_0' not found
```
