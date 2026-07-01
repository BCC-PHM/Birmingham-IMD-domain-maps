library(dplyr)
library(BSol.mapR)
library(sf)
library(tmap)
library(janitor)

data <- read.csv(
  "data/IoD-2025-custom_data_download-LSOA.csv",
  check.names = F
) 

domains <- colnames(data)[grepl("Decile", colnames(data))]

LSOA_shp <- st_as_sf(LSOA21) %>%
  filter(Area == "Birmingham") %>%
  left_join(
    data,
    by = join_by(
      "LSOA21" == "LSOA code (2021)"
    )
  )

ward_shp <- st_as_sf(Ward) %>%
  filter(Area == "Birmingham")
  
brum_shp <- st_union(ward_shp)

palette <- ggpubr::get_palette((c("#84329B", "#FFFFFF")), 20)

for (domain_i in domains) {
  map_i <- tm_shape(LSOA_shp) +
    tm_fill(
      domain_i,
      fill.scale = tm_scale_continuous(
        values = palette,
        ticks = c(1,5,10),
        labels = c("\n1\n(Most deprived)", "\n5\n", "\n10\n(Least deprived)")
      ),
      fill.legend = tm_legend(
        orientation = "landscape",
        margins = c(1.5, 0.5, 0.5, 0.5),
        title = paste(domain_i, "(2025)")
      )
    ) +
    tm_shape(ward_shp) +
    tm_borders(lwd = 1) +
    tm_shape(brum_shp) +
    tm_borders(lwd = 2) +
    tmap::tm_layout(
      legend.position = c("LEFT", "TOP"),
      scale = 1,
      legend.frame.alpha = 0,
      legend.frame.lwd = 0,
      legend.frame = FALSE,
      inner.margins = 0.08,
      frame = FALSE
    ) +
    tm_credits(
      paste("Contains OS data \u00A9 Crown copyright and database right",
            # Get current year
            format(Sys.Date(), "%Y"),
            ". Source:\nOffice for National Statistics licensed under the Open Government Licence v.3.0."
      ), 
      size = 0.8,
      position = c(0, 0.05)) +
    tm_compass(
      type = "8star",
      size = 6,
      position = c(0.8, 0.2),
      color.light = "white"
    )
  
  
  
  tmap_save(
    map_i, 
    paste0(
      "output/",
      gsub(" ", "_", tolower(domain_i)),
      ".png"
      )
  )
}