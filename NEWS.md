# terrainr 0.5.1
* New features:
    * A new endpoint, `ecosystems`, has been added to `get_tiles` and 
      `hit_national_map_api`. 
* Improvements and bug fixes:
    * `merge_rasters` gains an argument, `overwrite`, which allows you to 
      specify whether or not to overwrite `output_raster` if it exists. Previous
      versions expected you to pass "-overwrite" to `options`. If a file exists
      at `output_raster` and `overwrite` is FALSE, `merge_rasters` will throw an
      error.
* Dependency changes:
    * `sf` now has a minimum dependency of 1.0-5, to take advantage of an 
      upstream bug fix (relating to `merge_rasters` overwrite)

# terrainr 0.5.0.
* New features:
    * A new function, `make_manifest`, now helps automate the import of terrain
      and imagery to Unity. It fully replaces `raster_to_raw_tiles` (see 
      Deprecations below). Documentation updates are forthcoming.
* Deprecations:
    * `raster_to_raw_tiles` is now deprecated and will be removed in a future
      release (no earlier than 2022). Use `make_manifest` instead.
    * The method `get_tiles.list` is now deprecated and will be removed in a 
      future release (unexported in Fall 2021, removed no earlier than 2022).
      Convert your list to an `sf` object instead.
    * The `bbox` argument to `hit_national_map_api` is now documented as 
      "An object from [sf::st_bbox]." This is a change from the earlier options 
      of a length 2 list or terrainr_bounding_box object. Those methods are 
      currently still supported, but undocumented; they will be removed in a 
      future release (no earlier than 2022).
* Improvements and bug fixes:
    * `get_tiles` no longer mangles data with projected coordinates (via a 
      fix to the internal function `split_bbox`). If for some reason you want 
      the old behavior back, set the new argument `projected` to `FALSE` while
      providing projected data.
    * The documentation for `add_bbox_buffer` and `set_bbox_side_length` now 
      specifies that they should only be used with geographic coordinate 
      systems. If you use these functions with projected data, they will warn;
      this may be upgraded to an error in future versions.
    * The README images are now of beautiful Hyampom, California, a somewhat 
      more appealing vista than the original Mt Marcy scene.
    * The "Import to Unity" vignette has been rewritten to use make_manifest, as
      has the overview vignette and other documentation.
    * Typos in the message `merge_rasters` gives when using the fallback method
      have been fixed.
    * `merge_rasters` gains an argument `force_fallback` which, if TRUE, will 
      use the older, slower method for merging tiles. This is not recommended, 
      but is useful for testing.
* Internal changes:
    * The slow removal of all `terrainr_*` custom classes marches on! These 
      classes should no longer be present in any user-facing, non-deprecated 
      code; the only functions still relying on custom classes are internal 
      utilities and the `split_bbox` function responsible for tiling `get_tiles`
      requests.
    * `split_bbox` should now run faster, particularly for large tile sets, as
      some nested loops have been vectorized.
    * Improvements to test coverage and CI.

# terrainr 0.4.1
* Improvements and bug fixes:
    * `get_tiles` now displays a bulleted list of endpoints (again?), rather 
      than a jumble of raw markdown
    * `add_bbox_buffer` properly sets the CRS of the output when attempting to 
      buffer geodesic coordinates.
    * Typo fixes to an error message in `combine_overlays`
* Internal changes:
    * Added `importFrom` tag to `terrainr-package.R` to silence R CMD CHECK NOTE.

# terrainr 0.4.0
* Breaking changes:
    * Three changes in how `vector_to_overlay` deals with missing CRS in 
      `vector_data`:
        * A new argument, `error_crs`, behaves just like `error_crs` in 
          `add_bbox`: if `NULL`, the function will give a warning when assuming
          CRS; if `FALSE`, the function will assume a CRS silently, and if 
          `TRUE`, the function will error if `vector_data` is missing a CRS.
        * `target_crs` has been removed. `vector_data` will be given the CRS of 
          `reference_raster` if it doesn't have its own CRS, and will always be
          projected to the CRS of `reference_raster`.
        * `error_crs` has been added to mirror `add_bbox_buffer`: if `NULL` and
          your input data has no CRS, `vector_to_overlay` will warn about 
          assuming the raster CRS. Set to `TRUE` to error or `FALSE` to ignore
          the warning.
    * NAIP imagery is now downloaded with `transparent = "false"` to
      minimize the number of times the backup method to `merge_rasters` (see 
      below) is called. To restore the old behavior, set `transparent = "true"` 
      in either `get_tiles` or `hit_national_map_api`.
    * `get_tiles` will now infer `bboxSR` and `imageSR` from provided `sf` or
      `Raster` objects if not otherwise specified. To restore the old behavior,
      set `bboxSR` and `imageSR` to `4326` in `get_tiles` (or set your data's 
      CRS to 4326 before calling `get_tiles`).
* Improvements and bug fixes:
    * `merge_rasters` can once again handle merging mixed-band rasters (such as
      NAIP images with and without alpha bands). At the moment this is using the
      older, slower implementation and will raise a warning. (#30, #32).
* Internal changes:
    * Removed code to check for `ggplot2` from `vector_to_overlay` now that
      `ggplot2` is required
    * `calc_haversine_distance` (not exported) now assumes it's been provided
      with degrees. `coord_units` has been removed as an argument.
    * `get_tiles.terrainr_bounding_box` has been removed; it should no longer be
      possible for users to have `terrainr_bounding_box` objects unless they 
      were using non-exported functionality.

# terrainr 0.3.1
* First CRAN submission!
* This is the smallest of patch releases, with almost no user-facing changes.
* Internal changes:
    * Added rOpenSci reviewers to DESCRIPTION.
    * Changed USGS API link to new website.
    * Added rOpenSci badge to README.
    * Changed most PNG images to _slightly_ smaller JPGs.
    * Edited URLs for new rOpenSci website.
    * Moved lifecycle badge href to new site.
    * Some small spelling issues have been fixed.
    * Added \value tags to non-exported point_to_distance and 
      terrainr_bounding_box functions
    * Added single quotes around Unity in the DESCRIPTION

# terrainr 0.3.0
* Breaking changes:
    * `terrainr_*` classes have been effectively removed and are no longer 
      exported. Functions which previously expected these objects now generally 
      accept `sf` and `Raster` class objects instead. Functions which previously
      returned these objects now generally return `sf` objects instead (#24).
    * The list returned by `get_tiles` now uses the service names provided by
      the user, not the endpoint names. This means that 
      `get_tiles(..., services = "elevation")` will now use the name `elevation`
      instead of `3DEPElevation`, and remain standard across versions (#12).
    * `get_bbox` and `get_coordinate_bbox` have been removed. Functions that 
      used to expect `terrainr_bounding_box` objects now accept objects of class
      `sf` or `raster` (#24).
    * `add_bbox_buffer` loses the `divisible` argument. For precise control over
      side length, use `set_bbox_side_length` (which should be more accurate, if
      slightly more conservative, than the `divisible` system ever was) (#17).
    * `convert_distance` has been removed (internally replaced by the 
      `units` package) (#7).
    * `merge_rasters` loses the `input_images` and `output_image` function, as
      most downloaded files are now already georeferenced. To recreate this 
      functionality, georeference image tiles directly via 
      `output <- georeference_overlay(img_tiles, ref_tiles, tempfile(fileext = ".tif"))`
      and then provide `output` to `merge_rasters`.
    * A handful of utility functions are no longer exported:
        * `calc_haversine_distance`
        * `point_from_distance`
        * `rad_to_deg`
        * `deg_to_rad`
* New features:
    * Two new functions, `geom_spatial_rgb` and `stat_spatial_rgb`, allow you to 
      use RGB map tiles as backgrounds for further plotting.
    * `calc_haversine_distance` gains an argument `coord_units` allowing it to 
      handle coordinates in radians as well as degrees.
* Improvements and bug fixes:
    * `georeference_overlay` provides `tempfile(fileext = ".tif")` as a default
      output location if no `output_file` is provided.
    * `get_tiles` now tells you what tiles it's retrieving, not retriving.
* Internal changes:
    * `calc_haversine_distance` has been internally simplified somewhat to 
      reduce code duplication.
    * All `services` arguments to `hit_national_map_api` and `get_tiles` can 
      now handle both base64 and binary returns, removing the need to manually
      categorize endpoints (54ad9fb).
        * `hit_national_map_api` auto-detects whether API endpoints are 
          returning base64 or binary and handles them appropriately
        * `get_tiles` now auto-detects whether `hit_national_map_api` is 
          returning base64 or binary and writes to file appropriately.
    * `hit_national_map_api` is now more likely to fail with a human-friendly
      error message if API endpoints return a non-200 status (54ad9fb).
    * `hit_national_map_api` (and by extension `get_tiles`) now register a user
      agent.
* Changes in dependencies:
    * `gdalUtilities` has been removed, with functionality replaced by `sf`.
    * `rlang` has been removed, with functionality removed.
    * `units` has been added.
    * `ggplot2` has been moved to Imports (was previously in Suggests) due to 
      the new `geom_spatial_rgb` and `stat_spatial_rgb` functions.

# terrainr 0.2.1
* Improvements and bug fixes:
    * The `transportation` endpoint has moved servers, and is now handled by the
      same function that handles DEMs and orthoimages
* Internal changes:
    * The main branch of `terrainr` is now `main`
    * Tests run on a schedule on Monday/Wednesday/Friday mornings, to alert to 
      endpoint changes
    * Restyled code

# terrainr 0.2.0
* Breaking changes:
    * `merge_rasters` loses the argument `merge_raster`. For the "georeference 
      a single image" use case, see the new `georeference_overlay` function.
    * `get_tiles` gains an argument `resolution` (details below) between 
      `side_length` and `services`. No functionality should be changed, but code 
      with unnamed arguments to `services`, `verbose`, or `georeference` may be 
      impacted. 
* New features:
    * A new family of functions for dealing with overlay creation:
        * `vector_to_overlay` lets users quickly produce image overlays from 
          vector data.
        * `georeference_overlay` replaces the use of merge_raster for creating 
          single-file georeferenced overlay files.
        * `combine_overlays` lets users, well, combine overlays into a single 
          image
    * `get_tiles` gains an argument, `resolution`, specifying the number of 
      meters each pixel should represent (so higher images result in smaller 
      downloads). 
    * `get_bbox` provides an S3 generic to create `terrainr_bounding_box` 
      objects. In this version, that means users can use `get_bbox` to get 
      bounding boxes from `sf` and `RasterLayer` objects, and it means adding 
      methods will be easier going forward. The generic `get_bbox` method 
      is equivalent to `get_coord_bbox`
    * `raster_to_raw_tiles` handles rectangles appropriately
* Improvements and bug fixes:
    * `get_tiles`, `raster_to_raw_tiles`, and `merge_rasters` are now much more 
      conscientious about deleting tempfiles when they're done with them.
    * `merge_rasters` no longer fails when handed a mix of 3- and 4-band raster
      files. The current implementation will cast all 4 band rasters to 3 band
      images and then return a 3 band raster image.
    * The `output_image` argument to `merge_rasters` now has a default value of 
      `tempfile(fileext = ".tif")` to be a little more friendly to users.
    * Arguments `lat` and `lng` to `get_bbox` (and `get_coord_bbox`) no longer 
      need to be quoted -- either the tidyverse-feeling NSE approach or the 
      more standard quoted argument approach will work.
* Internal changes:
    * All terrainr-provided functions now explicitly use the terrainr:: 
      namespace.
* Changes in dependencies:
    * `sf` has been added as an explicit import due to `vector_to_overlay`. `sf`
      is required by `gdalUtilities`, also imported by this package, so this 
      change should have no impact on users.
    * `rlang` is added as a dependency to allow `lat` and `lng` be unquoted in 
      `get_bbox`.
    * `ggplot2` has been added to `Suggests` due to `vector_to_overlay`. 
    * `jpeg` and `tiff` have been added to `Suggests` due to 
      `georeference_overlay`. I'd expect more image libraries to join this list
      over time. 

# terrainr 0.1.0
* New features:
    * set_bbox_side_length wraps add_bbox_buffer to set each side of the 
      bounding box to an equal length (within ~1% accuracy)
* First version released on GitHub 

# terrainr 0.0.0.9001

* First development version
* Supports retrieval from 3DEP and NAIP data sources
* Supports export to Unity-friendly format
* Functions in this version:
    * Utility functions: 
        * add_bbox_buffer
        * calc_haversine_distance
        * convert_distance
        * deg_to_rad
        * get_bbox_centroid
        * get_coord_bbox
        * point_from_distance
        * rad_to_deg
    * Data retrieval functions:
      * get_tiles 
      * hit_national_map_api
    * Data processing functions:
      * merge_rasters
      * raster_to_raw_tiles
    * Classes and class utility functions:
      * terrainr_bounding_box (class)
      * terrainr_coordinate_pair (class)
      * terrainr_bounding_box (creation utility)
      * terrainr_coordinate_pair (creation utility)
      * export_bounding_box
      * export_coord_pair
