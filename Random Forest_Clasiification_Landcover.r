library(terra)
library(sf)
library(randomForest)
library(ggplot2)
library(tidyterra)
library(ggspatial)
library(scales)

NIR <- rast("D:/SPATIAL DATA/RASTER/B8.tif")
SWIR <- rast("D:/SPATIAL DATA/RASTER/B11.tif")
RED <- rast("D:/SPATIAL DATA/RASTER/B4.tif")

cat("Menyelaraskan resolusi dan extent raster...\n")
SWIR_resampled <- terra::project(SWIR, NIR, method = "bilinear")
RED_aligned <- terra::project(RED, NIR, method = "bilinear")
cat("Selesai.\n")

NDVI <- (NIR - RED_aligned) / (NIR + RED_aligned)


base_img <- c(NIR, SWIR_resampled, RED_aligned, NDVI)
names(base_img) <- c("NIR", "SWIR", "RED", "NDVI")

train_shp <- st_read("D:/SPATIAL DATA/TRAINING_SAMPLE.shp")
train_shp <- st_transform(train_shp, crs(base_img))

if (any(sf::st_geometry_type(train_shp) == "MULTIPOLYGON")) {
  train_shp <- sf::st_cast(train_shp, "POLYGON")
}

train_pts <- st_centroid(train_shp)
train_data <- terra::extract(base_img, vect(train_pts))
train_data <- cbind(train_data, Classname = train_pts$Classname)
train_data <- na.omit(train_data)

predictors <- train_data[, -c(1, ncol(train_data))]
response <- as.factor(train_data$Classname)

set.seed(123)
cat("\nMemulai training model Random Forest...\n")
rf_model <- randomForest(x = predictors, y = response, ntree = 500, importance = TRUE)
cat("Training model selesai.\n\n")

cat("\nMelakukan klasifikasi pada seluruh citra...\n")
rf_class <- predict(base_img, rf_model, na.rm = TRUE, type = "response")
cat("Klasifikasi selesai.\n")

model_classes <- levels(response)
palette <- c("yellow", "brown", "pink", "#228B22", "green", "blue")
cols <- setNames(palette[1:length(model_classes)], model_classes)

levels(rf_class) <- data.frame(ID = 1:length(model_classes), class = model_classes)

peta_final_lengkap <- ggplot() +
  geom_spatraster(data = rf_class, aes(fill = class)) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  annotation_scale(
    location = "bl",
    style = "ticks",
    width_hint = 0.25
  ) +
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.2, "in"),
    height = unit(0.8, "cm"),
    width = unit(0.8, "cm")
  ) +
  scale_fill_manual(
    values = cols,
    name = "Class",
    na.value = "white",
    na.translate = FALSE
  ) +
  scale_x_continuous(breaks = c(108.40, 108.42)) +
  labs(
    title = "Land Cover (Random Forest)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(color = "black", size = 8, angle = 0),
    axis.text.y = element_text(color = "black", size = 8, angle = 90, hjust = 0.5),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray70", linetype = "dashed", size = 0.3),
    panel.background = element_rect(fill = "white")
  )

print(peta_final_lengkap)

ggsave("D:/SPATIAL DATA/Peta_Klasifikasi_Final_v2.png", plot = peta_final_lengkap, width = 8, height = 9, dpi = 300)