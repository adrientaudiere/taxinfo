## R CMD check results

0 errors | 0 warnings | 0 notes

## First submission

This is the first submission of taxinfo to CRAN.

## Method References

There are no published references describing the methods in this package.
The package implements original functionality for augmenting 'phyloseq'
objects with taxonomy-based information from external data sources
(GBIF, Wikipedia, GLOBI, OpenAlex, GNA).

## External Data Sources

Several functions in this package make requests to external APIs
(GBIF, Wikipedia, GLOBI, OpenAlex, GNA). Examples that call these APIs
are wrapped in `\dontrun{}` or `\donttest{}` to avoid network access
during CRAN checks.
