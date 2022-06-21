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

convert_raw_data2pseudoms_image <-
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
    
    file1 <- mzR::openMSfile(file_name)
    file2 <-
      MSnbase::readMSData(
        files = file_name,
        msLevel. = 1,
        mode = "onDisk",
        verbose = TRUE
      )
    
    file_name <-
      file_name %>% 
      stringr::str_replace(pattern = ".mz[X]{0,1}ML",
                           replacement = "") %>% 
      stringr::str_replace(pattern = ".mz[x]{0,1}ml",
                           replacement = "")
    
    peaks <- ProtGenerics::peaks(object = file1)
    
    rt <- ProtGenerics::rtime(object = file2)
    
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
    
    ###mz int shift, bin mz
    temp_fun1 <- function(idx,
                          peaks,
                          mz_shift,
                          mean,
                          sd,
                          int_shift,
                          int_times,
                          get_mz_shift,
                          mz_range,
                          noise_threshold,
                          mz_pixel,
                          bin_x) {
      # library(tidyverse, warn.conflicts = FALSE)
      x <- peaks[[idx]]
      if (mz_shift) {
        x[, 1] <- get_mz_shift(x[, 1], mean, sd)
      }
      
      if (int_shift) {
        x[, 2] <- x[, 2] * int_times
      }
      
      ##remove some spectra
      x <- x[x[, 1] >= mz_range[1] &
               x[, 1] <= mz_range[2], , drop = FALSE]
      if (nrow(x) == 0) {
        x <- NULL
      }
      x <- x[x[, 2] > noise_threshold, , drop = FALSE]
      x[, 2] <- log(x[, 2], 10)
      
      ###bin mz
      cell <- bin_x(
        x = x[, 1],
        x_min = mz_range[1],
        x_max = mz_range[2],
        pixel = mz_pixel
      )
      
      x <- data.frame(x, cell, stringsAsFactors = FALSE)
      
      colnames(x)[c(1, 2)] <- c("mz", "intensity")
      
      # x <-
      #   x %>%
      #   dplyr::group_by(cell) %>%
      #   dplyr::mutate(intensity = sum(intensity)) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::select(cell, intensity) %>%
      #   dplyr::distinct(cell, .keep_all = TRUE)
      
      x <- dplyr::group_by(x, cell)
      x <- dplyr::mutate(x, intensity = sum(intensity))
      x <- dplyr::ungroup(x)
      x <- dplyr::select(x, cell, intensity)
      x <- dplyr::distinct(x, cell, .keep_all = TRUE)
      
      all_cell <-
        data.frame(cell = seq_len(mz_pixel),
                   stringsAsFactors = FALSE)
      
      dplyr::select(dplyr::left_join(all_cell, x, by = "cell"), -cell)
    }
    
    message(
      crayon::green(
        "m/z and RT shift and removing some spectra according to m/z and bining m/z..."
      )
    )
    
    # system.time(
    peaks <-
      BiocParallel::bplapply(
        X = seq_len(length(peaks)),
        FUN = temp_fun1,
        BPPARAM = BiocParallel::SnowParam(workers = threads,
                                          progressbar = TRUE),
        peaks = peaks,
        mz_shift = mz_shift,
        mean = mean,
        sd = sd,
        int_shift = int_shift,
        int_times = int_times,
        get_mz_shift = get_mz_shift,
        mz_range = mz_range,
        noise_threshold = noise_threshold,
        mz_pixel = mz_pixel,
        bin_x = bin_x
      )
    # )
    
    peaks <-
      do.call(cbind, peaks) %>%
      as.data.frame()
    
    colnames(peaks) <- rt
    
    peaks[is.na(peaks)] <- 0
    
    ##bin rt
    message('Binning rt...')
    
    cell <- bin_x(x = unname(rt),
                  x_min = rt_range[1],
                  rt_range[2],
                  pixel = rt_pixel)
    
    peaks <-
      data.frame(cell, t(peaks), stringsAsFactors = FALSE) %>%
      dplyr::group_by(cell) %>%
      dplyr::mutate_if(is.numeric, sum) %>%
      dplyr::sample_n(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-cell) %>%
      t() %>%
      as.data.frame()
    
    # save(peaks, file = file.path(output_path, file_name))
    png(
      filename = file.path(output_path, paste(file_name, ".png", sep = "")),
      width = rt_pixel,
      height = mz_pixel
    )
    par(mar = c(0, 0, 0, 0))
    image(t(as.matrix(peaks)),
          axes = FALSE,
          col = colorRampPalette(colors = c("white", "black"))(256))
    dev.off()
  }
