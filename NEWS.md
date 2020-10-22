# terrainr 0.1.0.9001
* Breaking changes:
    * `merge_rasters` loses the argument `merge_raster`. For the "georeference 
      a single image" use case, see the new `georeference_overlay` function.
* New features:
    * A new family of functions for dealing with overlay creation:
        * `vector_to_overlay` lets users quickly produce image overlays from 
          vector data.
        * `georeference_overlay` replaces the use of merge_raster for creating 
          single-file georeferenced overlay files.
        * `combine_overlays` lets users, well, combine overlays into a single 
          image
* Internal changes:
    * All terrainr-provided functions now explicitly use the terrainr:: 
      namespace.

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
