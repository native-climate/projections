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
              "mapdeck",
              "mt-climate-office/cmip6",
              "leaflet.extras",
              "metathis"
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
  sf::read_sf("native-land.parquet") %>%
  dplyr::mutate(`Native Land` = factor(`Native Land`, ordered = TRUE)) %>%
  cmip6:::st_rotate()

library(leaflet.extras)
tribal_land %>%
  dplyr::mutate(
    under_name = stringr::str_replace_all(`Native Land`, " ", "_") %>%
      stringr::str_replace_all("'", "&apos;"),
    over_name =  stringr::str_replace_all(`Native Land`, "'", "&apos;"),
    pop = 
      glue::glue(
        "<h2 style='text-align: center'>&emsp;{`Native Land`}&emsp;</h2>
        
        <p style='text-align: center'>
<a href='https://data.climate.umt.edu/native-climate/projections/pdf/{over_name}_CMIP6-climate-projections.pdf' target='_blank' style='color: #000000; text-decoration: none;'>
  <img src = 'pdf.svg' alt='Download CMIP6 data for {over_name}' width='50' height='50'/><br>VIEW
</a>
        </p>
        
<p style='text-align: center'>
<a href='https://data.climate.umt.edu/native-climate/projections/{under_name}.zip' download style='color: #000000; text-decoration: none;'>
  <img src = 'download.svg' alt='Download CMIP6 data for {over_name}' width='50' height='50'/><br>DOWNLOAD
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
                          selfcontained = FALSE,
                          title = "Native Climate CMIP6 Agricultural Climate Projections")

library(metathis)

meta() %>%
  meta_description(
    "Native Climate partners at the Montana Climate Office have extracted
place-based climate data for Native American, Alaska Native, and Native
Hawaiian lands located in the United States. Climate data and
projections for temperature, precipitation, and other metrics related to
crop, livestock and forestry agriculture are shown in the accompanying
graphs. The data derive from eight Coupled Model Intercomparison Project
Phase 6 (CMIP6) global climate models and four socioeconomic scenarios
for the period from 2015 to 2100, as well as the historical simulation
for each model for the period 1950 to 2014. Raw data are extracted for
the location of the reservation from the NASA Earth Exchange (NEX)
Global Daily Downscaled Projections (GDDP) dataset (NEX-GDDP-CMIP6)."
  ) %>% 
  meta_name("github-repo" = "native-climate/projections") %>% 
  meta_viewport() %>% 
  meta_social(
    title = "Native Climate CMIP6 Agricultural Climate Projections",
    url = "https://native-climate.github.io/projections/",
    image = "https://native-climate.com/wp-content/uploads/2022/08/NC-logo-web.png",
    image_alt = "Native Climate Logo",
    og_type = "website",
    og_author = c("Kyle Bocinsky")
  )

# ?metathis::include_meta()

# 
# widget <- htmlwidgets:: read_html("docs/index.html")
