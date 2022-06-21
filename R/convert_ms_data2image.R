#' @title Convert raw mass spectrometry data to pseudoMS image
#' @description Convert raw mass spectrometry data to pseudoMS image
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param file_name File name (mzXML or mzML)
#' @param mz_range mz_range
#' @param rt_range rt_range, second.
#' @param mz_pixel mz pixel
#' @param rt_pixel rt pixel
#' @param noise_threshold noise_threshold
#' @param output_path output_path, default is .
#' @param threads threads, default is 3
#' @param mz_shift mz shift or not.
#' @param mean If shift mz, mean value.
#' @param sd If shift mz, sd value.
#' @param rt_shift Shift RT or not.
#' @param rt_diff Rt shift.
#' @param int_shift Shift intensity or not.
#' @param int_times Time for intensity shift.
#' @return A png image.
#' @export

convert_ms_data2image <-
  function(file_name,
           mz_range = c(70, 1000),
           rt_range = c(50, 1000),
           mz_pixel = 224,
           rt_pixel = 224,
           noise_threshold = 500,
           output_path = ".",
           threads = 3,
           mz_shift = FALSE,
           mean = 1.12361,
           sd = 4.076444,
           rt_shift = FALSE,
           rt_diff = 10,
           int_shift = FALSE,
           int_times = 1.1) {
    # library(tidyverse)
    dir.create(output_path, showWarnings = FALSE)
    message("Reading data...")
    file1 <- mzR::openMSfile(file_name)
    file2 <-
      MSnbase::readMSData(
        files = file_name,
        msLevel. = 1,
        mode = "onDisk",
        verbose = TRUE
      )
    
    peaks <- ProtGenerics::peaks(object = file1)
    rt <- ProtGenerics::rtime(object = file2)
    
    file_name <-
      file_name %>%
      stringr::str_replace(pattern = ".mz[X]{0,1}ML",
                           replacement = "") %>%
      stringr::str_replace(pattern = ".mz[x]{0,1}ml",
                           replacement = "")
    
    rm(list = c("file1", "file2"))
    gc()
    
    ####remove spectra according to RT
    message(crayon::green("RT shift and removing some spectra according to rt..."))
    
    ##RT shift
    if (rt_shift) {
      rt <- rt + rt_diff
    }
    
    ###according RT to remove same spectra
    remain_idx <- which(rt > rt_range[1] & rt < rt_range[2])
    peaks <- peaks[remain_idx]
    rt <- rt[remain_idx]
    
    rm(list = c("remain_idx"))
    gc()
    
    length(peaks)
    
    message("New feature list...")
    new_peaks <-
      purrr::map2(
        .x = peaks,
        .y = rt,
        .f = function(x, y) {
          temp <-
            data.frame(x, y)
          colnames(temp) <- c("mz", "intensity", "rt")
          temp
        }
      ) %>%
      dplyr::bind_rows()
    
    rm(list = "peaks")
    gc()
    
    if (mz_shift) {
      message("Shifting mz...")
      new_peaks$mz <- get_mz_shift(new_peaks$mz, mean, sd)
    }
    
    if (int_shift) {
      message("Shifting intensity...")
      new_peaks$intensity <- new_peaks$intensity * int_times
    }
    
    ##remove some spectra
    new_peaks <-
      new_peaks %>%
      dplyr::filter(mz > mz_range[1] &
                      mz < mz_range[2] &
                      intensity > noise_threshold)
    
    if (nrow(new_peaks) == 0) {
      stop("All mz are not in the mz_rang you set.")
    }
    
    new_peaks$intensity <-
      log(new_peaks$intensity, 10)
    
    ###bin mz
    message("Binning mz and RT...")
    # system.time(
    mz_cell <- bin_x2(
      x = new_peaks$mz,
      x_min = mz_range[1],
      x_max = mz_range[2],
      pixel = mz_pixel
    )
    # )
    
    # system.time(
    # cell2 <- bin_x(
    #   x = new_peaks$mz,
    #   x_min = mz_range[1],
    #   x_max = mz_range[2],
    #   pixel = mz_pixel
    # )
    # )
    #
    # sum(cell == cell2)
      # system.time(
        rt_cell <- bin_x2(
        x = unique(new_peaks$rt),
        x_min = rt_range[1],
        x_max = rt_range[2],
        pixel = rt_pixel
      )
        # )
    
    new_peaks <-
      data.frame(new_peaks,
                 mz_cell = mz_cell) %>%
      dplyr::left_join(data.frame(rt_cell, rt = unique(new_peaks$rt)),
                       by = "rt")
    
    new_peaks <-
      new_peaks %>%
      dplyr::select(rt_cell, mz_cell, intensity) %>%
      dplyr::group_by(mz_cell, rt_cell) %>%
      dplyr::summarise(intensity = sum(intensity)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = "rt_cell",
                         values_from = "intensity") %>%
      tibble::column_to_rownames(var = "mz_cell") %>%
      as.matrix()
    
    # save(peaks, file = file.path(output_path, file_name))
    png(
      filename = file.path(output_path, paste(file_name, ".png", sep = "")),
      width = rt_pixel,
      height = mz_pixel
    )
    par(mar = c(0, 0, 0, 0))
    image(t(new_peaks),
          axes = FALSE,
          col = colorRampPalette(colors = c("white", "black"))(256))
    dev.off()
  }
