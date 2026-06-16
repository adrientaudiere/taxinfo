# Retrieve information about wikipedia pages for a given taxon id

\<a
href="https://adrientaudiere.github.io/MiscMetabar/articles/Rules.html#lifecycle"\>
\<img src="https://img.shields.io/badge/lifecycle-experimental-orange"
alt="lifecycle-experimental"\>\</a\>

Input can be either a taxon_id (Wikidata taxon identifier) or a tibble
as returned by \[tax_get_wk_lang()\].

## Usage

``` r
tax_get_wk_pages_info(
  taxon_id = NULL,
  tib_list = NULL,
  languages_pages = NULL,
  time_to_sleep = 0.3,
  summarize_function_length = "mean",
  summarize_function_views = "sum",
  n_days = 30,
  start_date = NULL,
  end_date = NULL,
  verbose = FALSE
)
```

## Arguments

- taxon_id:

  (Character string, required) The Wikidata taxon identifier (e.g.
  "Q10723171" for Xylobolus subpileatus)

- tib_list:

  A tibble as returned by \[tax_get_wk_lang()\] with columns "title",
  "site" and "lang".

- languages_pages:

  (Character vector) If not NULL, only the languages present in this
  vector will be queried. The language codes are the two- or
  three-letter codes defined by ISO 639-1. For example, c("en", "fr",
  "de") will query only the English, French and German Wikipedia pages.

- time_to_sleep:

  (numeric, default 0.3) Time to sleep between two calls to wikipedia
  API.

- summarize_function_length:

  A function to summarize the page length across languages. Default is
  "mean". Other options can be "sum", "median", "max", "min", etc.

- summarize_function_views:

  A function to summarize the page views across languages. Default is
  "sum". Other options can be "mean", "median", "max", "min", etc.

- n_days:

  (numeric, default 30) Number of days to consider for the page views.

- start_date:

  The start date for the page views. If NULL (default), the start date
  is set to 'n_days' before the end date.

- end_date:

  The end date for the page views. If NULL (default), the end date is
  set to yesterday's date.

- verbose:

  (logical, default TRUE) If TRUE, prompt some messages.

## Value

A list with two elements: - \`page_length\`: Mean length of the
wikipedia pages (in characters) - \`page_views\`: Total number of page
views over the last 'n_days' days

## See also

\[tax_get_wk_lang()\], \[tax_get_wk_info_pq()\], \[tax_photo_pq()\]

## Author

Adrien Taudiere

## Examples

``` r
if (FALSE) { # \dontrun{
tax_get_wk_pages_info("Q10723171")
tax_get_wk_pages_info("Q10723171", languages_pages = c("fr", "en"))
tax_get_wk_pages_info("Q10723171", languages_pages = c("fr"))

pages_Q10723171 <- tax_get_wk_lang("Q10723171")
tax_get_wk_pages_info(tib_list = pages_Q10723171)
tax_get_wk_pages_info(
  tib_list = pages_Q10723171,
  summarize_function_length = "sum"
)
tax_get_wk_pages_info(
  tib_list = pages_Q10723171,
  summarize_function_length = "sum",
  n_days = 365
)

tax_get_wk_pages_info(
  tib_list = pages_Q10723171,
  start_date = "2023-01-01",
  end_date = "2023-12-31"
)
} # }
```
