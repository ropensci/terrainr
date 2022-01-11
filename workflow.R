

DiagrammeR::grViz('
      digraph workflow {
  graph [layout = dot,
         rankdir = TB]
  node [shape = rectangle,
        style = filled,
        color = "#b4bfb4",
        width = 0.9,
        fontsize = 10]


      node [label = "Manually download data from web portal", fillcolor = "#b8ffb8"]
      A

      node [label = "Convert data (if needed) into TIFF format", fillcolor = "#ff9494"]
      B

      node [label = "Import data into image-editing software"]
      C

      node [label = "Convert image into greyscale"]
      D

      node [label = "Rescale image to Unity-acceptable size"]
      E

      node [label = "Convert image to RAW format"]
      F

      node [label = "Import raw file into Unity", fillcolor = "#FAFAD2"]
      G

      node [label = "Repeat for each terrain tile to import"]
      H

      A->B->C->D->E->F->G->H

      }

')

DiagrammeR::grViz('
      digraph workflow {
  graph [layout = dot,
         rankdir = TB]
  node [shape = rectangle,
        style = filled,
        color = "#b4bfb4",
        width = 0.9,
        fontsize = 10]

      node [label = "Download data via get_tiles", fillcolor = "#b8ffb8"]
      A

      node [label = "Process data for Unity via make_manifest", fillcolor = "#ff9494"]
      B

      node [label = "Import all terrain tiles into Unity via the Terrain Import wizard", fillcolor = "#FAFAD2"]
      C

      A->B->C

      }
')
