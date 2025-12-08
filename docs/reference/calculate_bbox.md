# Calculate Bounding Box Around a Point

This function calculates a geographic bounding box around a given point
with a specified radius in kilometers.

## Usage

``` r
calculate_bbox(longitude = NULL, latitude = NULL, radius_km = 1)
```

## Arguments

- longitude:

  Numeric. Longitude of the center point in decimal degrees.

- latitude:

  Numeric. Latitude of the center point in decimal degrees.

- radius_km:

  Numeric. Radius in kilometers for the bounding box.

## Value

A list containing xmin, xmax, ymin, ymax coordinates in decimal degrees.

## Details

The function uses an approximation where 1 degree ~= 111.32 km and
adjusts for latitude distortion where longitude degrees get closer at
the poles.

## Author

Adrien Taudiere
