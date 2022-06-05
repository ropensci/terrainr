# set_bbox_side_length works within 1%

    Code
      set_bbox_side_length(df_sf, 8000)
    Warning <rlang_warning>
      No CRS associated with input data.
      i Assuming EPSG:4326.
    Output
      Geometry set for 1 feature 
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -73.97357 ymin: 44.07662 xmax: -73.87337 ymax: 44.14857
      Geodetic CRS:  WGS 84
    Message <simpleMessage>
      POLYGON ((-73.97357 44.07662, -73.87337 44.0766...

---

    Code
      set_bbox_side_length(df_sf, 8000, error_crs = TRUE)
    Error <rlang_error>
      No CRS associated with input data.

