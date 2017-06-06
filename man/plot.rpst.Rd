\name{plot.rpst}
\alias{plot.rpst}
\title{Produces a tree plot of a "rpst" object}
\description{
Produces a tree plot of a "rpst" object.
}
\usage{
  \method{plot}{rpst}(x, xlength = 1, ylength = 3, xshift = 0.5 , ysegment = 6,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a "\code{rpst}" project.}
  \item{xlength}{xlength of the plot.}
  \item{ylength}{ylength of the plot.}
  \item{xshift}{xshift of the plot.}
  \item{ysegment}{ysegment of the plot.}
  \item{\dots}{additional summary arguments.}
}


\references{
 Zhang, H., Legro, R. S., Zhang, J., Zhang, L., Chen, X., Huang, H., ... & Eisenberg, E. (2010). Decision trees for identifying predictors of treatment effectiveness in clinical trials and its application to ovulation in a study of women with polycystic ovary syndrome.. Human Reproduction, 25(10), 2612-2621.
}

\author{Yewei Li}

\seealso{
\code{\link{rpst}}, \code{\link{print.rpst}}, \code{\link{predict.rpst}}.
}

\examples{
#require(survival)
data <- gen.data(n = 20, p = 5, c = 2)
tree <- rpst(data$data, minsize = 2)
print.rpst(tree)

plot.rpst(tree)
}