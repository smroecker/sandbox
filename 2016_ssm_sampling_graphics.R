
# sp version ----
library(sp)


# Create a sixteen square polygon
grd <- GridTopology(c(1, 1), c(1, 1), c(4, 4))
polys <- as.SpatialPolygons.GridTopology(grd)

# graphic paramaters
png(file = "sample_comparison.png", width = 7, height = 2, units = "in", res = 300)
par(mfrow = c(1, 4), mar = c(0, 0, 1, 0))


## Simple ----
plot(polys, main = "Simple", cex.main = 1.5)
test <- spsample(polys, n = 16, type = "random")
points(test, pch = 16, cex = 2)


## Stratified ----
plot(polys, main = "Stratified", cex.main = 1.5)
# Generate a spatially stratified random sample
# test <- spsample(polys, n = 16, type = "stratified")
# points(test, pch = 16, cex = 2)
s <- sapply(slot(polys, 'polygons'), function(x) spsample(x, n = 1, type = "random"))
s <- do.call("rbind", s)
points(s, pch = 16, cex = 2) # randomly select 1 square and plot


## Two-stage ----
plot(polys, main = "Two-stage", cex.main = 1.5)
# Select 8 samples from each square
s <- sapply(slot(polys, 'polygons'), function(x) spsample(x, n = 8, type = "random"))
points(sample(s, 1)[[1]], pch = 16, cex = 2) # randomly select 1 square and plot
points(sample(s, 1)[[1]], pch = 16, cex = 2) # randomly select 1 square and plot


## Systematic ----
plot(polys, main = "Systematic", cex.main = 1.5)
# Generate systematic random sample
test <- spsample(polys, n = 16, type = "regular")
points(test, pch = 16, cex = 2)


dev.off()


polys_sdf <- SpatialPolygonsDataFrame(
  polys,
  data = data.frame(
    z = rnorm(length(polys)),
    # x = coordinates(polys)[, 1],
    # y = coordinates(polys)[, 2],
    row.names = row.names(polys)
    )
  )

pts_sdf <- SpatialPointsDataFrame(
  data = data.frame(z = polys_sdf$z),
  coords = cbind(polys_sdf$x, polys_sdf$y)
)


polys_sf <- st_as_sf(polys_sdf)
pts_sf   <- st_centroid(polys_sf)


# sf version ----
library(sf)
library(ggplot2)


# Create a sixteen square polygon
set.seed(4)

bb <- st_make_grid(st_bbox(c(xmin = 0, xmax = 4, ymin = 0, ymax = 4)), n = 4)
grd <- cbind(
  ID = as.character(1:length(bb)),
  st_coordinates(st_centroid(bb))[, 1:2],
  data.frame(
    Z = rnorm(length(bb), mean = 15, sd = 5),
    S = rlnorm(length(bb), mean = 2, sd = 1)
  ), 
  bb
)
grd <- st_as_sf(grd, sf_column_name = "geometry")
pts <- st_centroid(grd)


## Simple ----
test <- st_sample(grd, size = 10, type = "random")

gg_srs <- ggplot() + 
  geom_sf(data = grd) + 
  geom_sf(data = test) + 
  scale_x_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  scale_y_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  # theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  ggtitle("Simple")
gg_srs


## Stratified ----
test <- sapply(1:nrow(grd), function(i) {
  st_sample(grd[i, ], size = 1, type = "random")
})
test <- st_sfc(test)

gg_strs <- ggplot() + 
  geom_sf(data = grd) + 
  geom_sf(data = test) + 
  scale_x_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  scale_y_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  # theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  ggtitle("Stratified")
gg_strs


## Two-stage ----
# Select 8 samples from each square
idx <- sample(1:nrow(grd), size = 2, replace = FALSE)
grd_sub <- grd[idx, ]
test <- sapply(1:2, function(i) {
  st_coordinates(st_sample(grd_sub[i, ], size = 8, type = "random"))
})
test <- st_as_sf(as.data.frame(test), coords = 1:2)

gg_2srs <- ggplot() + 
  geom_sf(data = grd) + 
  geom_sf(data = test) + 
  scale_x_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  scale_y_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  # theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  ggtitle("Two-stage")
gg_2srs


## Systematic ----
test <- st_sample(grd, size = 16, type = "regular")
gg_syrs <- ggplot() + 
  geom_sf(data = grd) + 
  geom_sf(data = test) + 
  scale_x_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  scale_y_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  # theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  ggtitle("Systematic")
gg_syrs


## cLHS ----
library(clhs)

idx1 <- clhs(st_drop_geometry(pts)[c("X", "Y")], size = 4)
idx2 <- clhs(st_drop_geometry(grd),      size = 4)

gg_clhs <- ggplot() + 
  geom_sf(data = grd) + 
  # geom_sf(data = grd, aes(fill = z)) + 
  geom_sf(data = pts[idx1, ]) + 
  scale_x_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  scale_y_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  # scale_fill_viridis_c() +
  # theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  ggtitle("cLHS")
gg_clhs


## spcosa ----
library(spcosa)

grd_sp <- as(grd, "Spatial")

strata <- stratify(grd_sp, nStrata = 5) 
pts    <- spsample(strata)
gg_spcosa <- plot(strata, pts) + 
  scale_x_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  scale_y_continuous(breaks = (0:3) + 0.5, labels = 1:4) +
  theme(axis.title = element_blank()) +
  ggtitle("Spatial Coverage")


gg <- gridExtra::grid.arrange(gg_srs, gg_strs, gg_2srs, gg_syrs, gg_clhs, gg_spcosa, nrow = 2)
ggsave(gg, file = "sample_comparison_sf2.png", width = 6, height = 4, units = "in", dpi = 300)

## spsurvey ----
library(spsurvey)

st_crs(grd) <- 5070
grd$prob <- runif(16)

test <- grts(sframe = grd, n_base = 4, aux_var = "prob", seltype = "proportional")
gg_grts <- ggplot() + 
  geom_sf(data = grd, aes(fill = S)) + 
  geom_sf(data = test$sites_base) + 
  scale_fill_gradient(low = "black", high = "white") +
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  ggtitle("GRTS")
gg_grts



  # raster examples ----
library(raster)

data(volcano) # details at http://geomorphometry.org/content/volcano-maungawhau
volcano_r <- raster(as.matrix(volcano[87:1, 61:1]), crs = CRS("+init=epsg:27200"), xmn = 2667405, xmx = 2667405 + 61*10, ymn = 6478705, ymx = 6478705 + 87*10) # import volcano DEM
test <- as(extent(volcano_r), "SpatialPolygons") # create a polygon from the spatial extent of the volcano dataset
names(volcano_r) <- "elev"
slope_r <- terrain(volcano_r, opt = "slope", unit = "radians") # calculate slope from the DEM
rs <- stack(volcano_r, slope_r)

sr400 <- spsample(test, n = 400, type = "random") # take a large random sample
sr <- spsample(test, n = 20, type = "random") # take a small random sample
str <- spsample(test, n = 23, type = "stratified", iter = 1000) # take a small stratified random sample
str <- str[1:20]
cs <- clhs(rs, size = 20, progress = FALSE, simple = FALSE) # take a cLHS sample

s <- rbind(data.frame(method = "Simple Random",extract(rs, sr)),
                data.frame(method = "Stratified Random", extract(rs, str)),
                data.frame(method = "cLHS", cs$sampled_data@data)
)
s$slope <- as.numeric(s$slope)

lty <- 1:3
col <- c("orange", "blue", "black")

densityplot(~ slope, group = method, data = s, 
            xlab = "Slope Gradient (%)", main = "Comparison of Different Sampling Methods",
            lty = lty, lwd = 3, col = "black", type = c("g", "n"),
            key = list(columns = 3, lines = list(lty = lty, lwd = 3), text = list(levels(as.factor(s$method))))
            )

            
par(mfrow = c(1, 3))
plot(volcano_r, main = "Simple random", cex.main = 2)
points(sr, pch = 3, cex = 1.2)

plot(volcano_r, main = "Stratified random", cex.main = 2)
points(str, pch = 3, cex = 1.2)

plot(volcano_r, main = "cLHS", cex.main = 2)
points(cs$sampled_data, pch = 3, cex = 1.2)


