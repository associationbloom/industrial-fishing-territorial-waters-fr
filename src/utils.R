library(raster)

create_grid <- function(data, name_res = "none", resolution = 1, xmin = -180, xmax = 180, ymin = -90, ymax = 90) {
  ext <- extent(xmin, xmax, ymin, ymax)
  grid <- raster(ext)

  if (name_res == "CESM_LR") {
    res(grid) <- c(1.1250000, 0.5342287)
  } else {
    res(grid) <- resolution
  }

  extent_box <- data %>%
    ungroup() %>%
    reframe(minLon = min(lon),
            maxLon = max(lon),
            minLat = min(lat),
            maxLat = max(lat))

  e <- extent(extent_box$minLon, extent_box$maxLon,
              extent_box$minLat, extent_box$maxLat) #___box containing all the dives

  grid <- crop(grid, e)

  return(grid)
}

df_to_raster <- function(data, grid, FUN, quantity, layers = "") {
  coordinates(data) <- c("lon", "lat")
  if (layers != "") {
    layer_names <- unique(data[[layers]])
    rr <- NULL
    for (i in 1:length(layer_names)) {
      data_lyr <- data[data[[layers]] == layer_names[i],]
      r = rasterize(data_lyr, grid, quantity, fun = FUN)

      rr[[i]] <- r
    }
    rr_brick <- do.call(brick, c(rr))
    names(rr_brick) <- layer_names
    return(rr_brick)
  } else {
    r = rasterize(data, grid, quantity, fun = FUN)
    rr <-  rasterize(data, grid, quantity, fun = FUN)
    return(rr)
  }
}
