% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transforms.R
\name{bark_to_hz}
\alias{bark_to_hz}
\title{Bark to Hz}
\usage{
bark_to_hz(bark)
}
\arguments{
\item{bark}{Frequency in Bark}
}
\value{
A vector of Hz scaled values
}
\description{
Converts bark to Hz
}
\details{
\deqn{
\hat{b} = \begin{cases}
\frac{b - 0.3}{0.85} & \text{if} ~ b < 2\\
\frac{b + 4.422}{1.22} & \text{if} ~ b > 20.1\\
b & \text{otherwise}
\end{cases}
}

\deqn{
hz = 1960\frac{\hat{b} + 0.53}{26.28 - \hat{b}}
}
}
\examples{
bark <- seq(1.5, 13, length = 100)
hz <- bark_to_hz(bark)
plot(bark, hz)
}
\references{
Traunmüller, H. (1990). Analytical expressions for the tonotopic
sensory scale. The Journal of the Acoustical Society of America,
88(1), 97–100. \doi{10.1121/1.399849}
}
