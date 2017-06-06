\name{gen.data}
\alias{gen.data}
\title{Generate simulated data}
\description{
   Generate survival data for simulations.
}
\usage{
  gen.data(n=100, p=10, c = 4, beta = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{n}{The number of observations.}
 \item{p}{The number of predictors of interest.}
 \item{c}{The censoring paramter to control the censoring rate. Default is 4.}
 \item{beta}{The coefficient values in the underlying regression model.}
}

\value{
A list with the following components: data, beta.
 \item{data}{The simulated dataset.}
  \item{beta}{The coefficients.}
}

\references{
 Zhang, H., Legro, R. S., Zhang, J., Zhang, L., Chen, X., Huang, H., ... & Eisenberg, E. (2010). Decision trees for identifying predictors of treatment effectiveness in clinical trials and its application to ovulation in a study of women with polycystic ovary syndrome.. Human Reproduction, 25(10), 2612-2621.
}

\author{Yewei Li}

\seealso{
\code{\link{rpst}}, \code{\link{predict.rpst}}, \code{\link{plot.rpst}}.
}

\examples{
#require(survival)
data <- gen.data(n = 20, p = 5, c = 2)
tree <- rpst(data$data, minsize = 2)
print.rpst(tree)
}

