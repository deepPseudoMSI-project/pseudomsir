% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_ms_data2image.R
\name{convert_ms_data2image}
\alias{convert_ms_data2image}
\title{Convert raw mass spectrometry data to pseudoMS image}
\usage{
convert_ms_data2image(
  file_name,
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
  int_times = 1.1
)
}
\arguments{
\item{file_name}{File name (mzXML or mzML)}

\item{mz_range}{mz_range}

\item{rt_range}{rt_range, second.}

\item{mz_pixel}{mz pixel}

\item{rt_pixel}{rt pixel}

\item{noise_threshold}{noise_threshold}

\item{output_path}{output_path, default is .}

\item{threads}{threads, default is 3}

\item{mz_shift}{mz shift or not.}

\item{mean}{If shift mz, mean value.}

\item{sd}{If shift mz, sd value.}

\item{rt_shift}{Shift RT or not.}

\item{rt_diff}{Rt shift.}

\item{int_shift}{Shift intensity or not.}

\item{int_times}{Time for intensity shift.}
}
\value{
A png image.
}
\description{
Convert raw mass spectrometry data to pseudoMS image
}
\author{
Xiaotao Shen
\email{shenxt1990@outlook.com}
}
