# set_bbox_side_length works within 1%

    Code
      set_bbox_side_length(df_sf, 8000)
    Condition
      Warning:
      No CRS associated with input data.
      i Assuming EPSG:4326.
    Output
      Geometry set for 1 feature 
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -73.97357 ymin: 44.07662 xmax: -73.87337 ymax: 44.14857
      Geodetic CRS:  WGS 84
    Message
      POLYGON ((-73.97357 44.07662, -73.87337 44.0766...

---

    Code
      set_bbox_side_length(df_sf, 8000, error_crs = TRUE)
    Condition
      Error in `add_bbox_buffer()`:
      ! No CRS associated with input data.

