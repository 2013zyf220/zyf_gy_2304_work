buf_set <- 100

strs_1 <- shapefile(paste0('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/LINES/streets_5_buf', buf_set, '.shp'))
tc_1 <- raster('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/TC/TC_4.tif')

ii <- 4
tc_2 <- crop(tc_1, extent(strs_1[ii, ]))
tc_3 <- mask(tc_2, strs_1[ii, ])

plot(tc_2)
plot(tc_3)

tc_3v <- getValues(tc_3)
tc_4 <- na.omit(tc_3v)
tc_4_len <- length(tc_4)
tc_5 <- tc_4[tc_4 != 0]
tc_5_len <- length(tc_5)
tc_cover <- tc_5_len/tc_4_len

plot(tc_1)
plot(strs_1[3,], add = T)
