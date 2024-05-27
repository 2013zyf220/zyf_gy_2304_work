
src_dir <- 'E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES1'
dest_dir <- 'E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES2'

if (!dir.exists(dest_dir)) {
  dir.create(dest_dir, recursive = TRUE)
}

csv_files <- list.files(src_dir, pattern = '\\.csv$', full.names = TRUE)

# 遍历csv文件并复制到目标目录，同时修改文件名
for (file in csv_files) {
  filename <- basename(file)
  new_filename <- sub('^data', 'rec', filename)
  dest_file <- file.path(dest_dir, new_filename)
  file.copy(file, dest_file)
  cat('File: ', file, 'was copied to', dest_file, '\n')
}


