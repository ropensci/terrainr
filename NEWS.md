# terrainr 0.2.1.9000
* Improvements and bug fixes:
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
