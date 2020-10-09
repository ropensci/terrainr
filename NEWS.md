# terrainr 0.0.0.9001

* First development version
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
      * Supports 3DEP and NAIP data sources
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
