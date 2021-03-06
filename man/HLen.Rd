% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HLen.R
\name{HLen}
\alias{HLen}
\title{HLen}
\usage{
HLen(
  Disc_1 = -5,
  Disc_2 = -33,
  Rad_1 = 1.4375,
  Rad_2 = 3,
  Fing_1 = 4.8125,
  Knuc_1 = 0.610865,
  Rot_1 = 0.785398
)
}
\arguments{
\item{Disc_1}{relative angle (in degrees) of larger disc (disc 1) in chamber door vs. gravity}

\item{Disc_2}{relative angle (in degrees) of smaller disc (disc 2) in chamber door vs. gravity}

\item{Rad_1}{fixed value describing  the distance between the center of larger disc (disc 1) and the center of the smaller disc (disc 2)}

\item{Rad_2}{fixed value describing the distance between the center of disc 2 and the position of each cathode (cathodes are symmetric)}

\item{Fing_1}{distance from the knuckle to the gun tip}

\item{Knuc_1}{angle (in radians) at which the gun is set at the knuckle.  Min is 0, max is 1.18}

\item{Rot_1}{angle (in radians) at which the gun is rotated, min is 0, max is 2*pi}
}
\description{
distance between the chamber origin and the platen (height of platen relative to gun)
}
\author{
Greg Pilgrim \email{gpilgrim@vergason.com}
}
