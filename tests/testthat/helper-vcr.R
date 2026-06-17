# vcr records each live HTTP response into a cassette (`tests/testthat/fixtures/
# *.yml`) on first run, then replays it offline on every subsequent run. This
# keeps the synchronous-API tests (GBIF name_backbone / occ_count / occ_search,
# and later GloBI / OpenAlex / Wikipedia) fast and network-free while still
# exercising the real response contract. Credentials are redacted from the
# recorded cassettes. See <https://docs.ropensci.org/vcr/>.
#
# To (re)record a cassette, delete its .yml file and run the test with network
# access (and, for credentialed endpoints, GBIF_USER/GBIF_PWD/GBIF_EMAIL set).
#
# Note: the asynchronous GBIF Download API (occ_download) is not recorded with
# vcr (async polling + binary zip); its import/attribution path is covered
# offline by the bundled SIMPLE_CSV fixture instead (see
# fixtures/gbif_simple_csv_sample.rds and test-gbif_download_fixture.R).
if (requireNamespace("vcr", quietly = TRUE)) {
  invisible(vcr::vcr_configure(
    dir = vcr::vcr_test_path("fixtures"),
    filter_sensitive_data = list(
      "<gbif_user>" = Sys.getenv("GBIF_USER"),
      "<gbif_pwd>" = Sys.getenv("GBIF_PWD"),
      "<gbif_email>" = Sys.getenv("GBIF_EMAIL")
    ),
    filter_request_headers = c("User-Agent", "X-USER-AGENT", "Authorization")
  ))
}
