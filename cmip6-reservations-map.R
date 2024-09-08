# install.packages("pak")

packages <- c("magrittr",
              "terra",
              "tidyverse",
              "multidplyr",
              "openxlsx",
              "png",
              "RcppRoll",
              "mgcv",
              "tigris",
              "weathermetrics",
              "gamlss",
              "ragg",
              "furrr",
              "future.callr",
              "parallelly",
              "mapdeck"
)

pak::pak(packages, ask = FALSE)
# install.packages("arrow", type = "source")
# purrr::walk(packages, devtools::install_cran)
purrr::walk(packages,
            library,
            character.only = TRUE)
library(arrow)
library(mapview)
mapviewOptions(
  # platform = "mapdeck"
  fgb = TRUE
  )
options(future.rng.onMisuse="ignore")
terra::gdalCache(32000)


tribal_land <- 
  sf::read_sf("cmip6-reservations.parquet") %>%
  dplyr::mutate(`Native Land` = factor(`Native Land`, ordered = TRUE)) %>%
  cmip6:::st_rotate()

mapview::mapview(tribal_land)

library(leaflet.extras)
tribal_land %>%
  dplyr::mutate(
    under_name = stringr::str_replace_all(`Native Land`, " ", "_"),
    pop = 
      glue::glue(
        "<h2 style='text-align: center'>&emsp;{`Native Land`}&emsp;</h2>
<p style='text-align: center'>
<a href='https://data.climate.umt.edu/projections/native-climate/{`Native Land`}.zip' download>
  Download CMIP6 Projections Here
</a>
        </p>"
        )
    ) %>%
mapview::mapview(x = .,
                 popup = .$pop,
                 label = "Native Land",
                 zcol = "Native Land",
                 legend = FALSE, 
                 layer.name = "area",
                 map.types = "CartoDB.Positron",
                 homebutton = FALSE) %>%
    mapview::removeMapJunk(junk = c("zoomControl",
                                    "layersControl",
                                    "scaleBar")) %>%
  leaflet.extras::addSearchFeatures(
    targetGroups = "area",
    options = 
      searchFeaturesOptions(
        propertyName = "Native Land", 
        openPopup = TRUE,
        zoom = 7,
        collapsed = FALSE,
        hideMarkerOnCollapse = TRUE,
        marker = 
          list(
            icon = NULL, 
            animate = TRUE, 
            circle = 
              list(
                radius = 10, 
                weight = 3, 
                color = "#e03", 
                stroke = TRUE, 
                fill = FALSE
              )
            )
        )
  ) %>%
  leafem::removeMouseCoordinates() %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }") %>%
  htmlwidgets::saveWidget(file = "docs/index.html",
                           title = "Native Climate CMIP6 Agricultural Climate Projections")

