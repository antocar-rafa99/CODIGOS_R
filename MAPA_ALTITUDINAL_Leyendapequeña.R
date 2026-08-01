#########################################################
# MAPA ALTITUDINAL DEL PERÚ - LEYENDA ABAJO Y COMPACTA
#########################################################
# Tiempo estimado de 20 min
library(terra)
library(sf)

# 1. Cargar archivos
ruta_dem <- "C:/Users/user/Desktop/DEMS/DEMPERU_UNION/DEM_PERU1.tif"
ruta_geojson <- "C:/Users/user/Desktop/DEMS/region natural_geogpsperu.geojson"

dem <- rast(ruta_dem)
regiones <- read_sf(ruta_geojson)

# 2. Preparar datos
dem_peru <- dem
dem_peru[dem_peru <= 0] <- NA
regiones <- st_transform(regiones, crs(dem_peru))

# 3. Sombras 3D
slope <- terrain(dem_peru, v = "slope", unit = "radians")
aspect <- terrain(dem_peru, v = "aspect", unit = "radians")
hillshade <- shade(slope = slope, aspect = aspect, angle = 45, direction = 315)

# 4. Recortes por región
dem_costa  <- mask(crop(dem_peru, regiones[regiones$Nm_RegNat == "Costa", ]),  regiones[regiones$Nm_RegNat == "Costa", ])
dem_sierra <- mask(crop(dem_peru, regiones[regiones$Nm_RegNat == "Sierra", ]), regiones[regiones$Nm_RegNat == "Sierra", ])
dem_selva  <- mask(crop(dem_peru, regiones[regiones$Nm_RegNat == "Selva", ]),  regiones[regiones$Nm_RegNat == "Selva", ])

# 5. Colores y cortes
cortes_sierra <- c(0, 1500, 3000, 4500, 5500, 6744)
colores_sierra <- c("#a7c957", "#b07d62", "#6c584c", "#d6ccc2", "#ffffff")

# ======================================================
# 6. DIBUJAR MAPA (CON MARGEN ABAJO AJUSTADO)
# ======================================================
# Aumentamos el margen inferior (5 en lugar de 3) para dar aire a la leyenda
par(mar = c(5, 4, 3, 2), bg = "white")

# Dibujar mapa con encuadre original (sin deformar con xlim)
plot(hillshade, 
     col = gray(0:100 / 100), 
     legend = FALSE, 
     main = "Mapa Altitudinal del Perú", 
     axes = TRUE)

# Superponer capas
plot(dem_costa > -Inf, col = "#e9c46a", alpha = 0.55, add = TRUE, legend = FALSE)
plot(dem_selva > -Inf, col = "#2d6a4f", alpha = 0.55, add = TRUE, legend = FALSE)
plot(dem_sierra, breaks = cortes_sierra, col = colores_sierra, alpha = 0.60, add = TRUE, legend = FALSE)
plot(st_geometry(regiones), border = "black", lwd = 0.5, add = TRUE)

# ======================================================
# 7. LEYENDA SÚPER PEQUEÑA Y COMPACTA (SIN TAPAR EL MAPA)
# ======================================================

legend(x = -81.2, y = -13.5, 
       legend = c("Costa (Desértica)", 
                  "Selva (Amazonía)", 
                  "Sierra: Valles (<3000m)", 
                  "Sierra: Puna (3000-4500m)", 
                  "Sierra: Alta Montaña (4500-5500m)", 
                  "Sierra: Nevados (>5500m)"),
       fill = c("#e9c46a", "#2d6a4f", "#b07d62", "#6c584c", "#d6ccc2", "#ffffff"),
       border = "black", 
       title = "Leyenda Altitudinal", 
       bg = "white",       # Fondo blanco
       box.col = "black",  # Borde negro
       cex = 0.32,         # <--- Letra y cuadritos súper pequeños
       x.intersp = 0.4,    # Espacio horizontal mínimo
       y.intersp = 0.5)    # Espacio vertical mínimo (caja compacta)

