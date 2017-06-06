\name{rpst.km}
\alias{rpst.km}
\title{Produces KM-curve plot of a "rpst" object}
\description{
Produces a KM-curve plot of a "rpst" object.
}
\usage{
  rpst.km(object, newdata, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a "\code{rpst}" project.}
  \item{newdata}{New data used for prediction and plot.}
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

rpst.km(tree,data$data)

##not run##
#data <- gen.data(n = 100, p = 10, c = 2)
#tree <- rpst(data$data, minsize = 2)
#print.rpst(tree)

#rpst.km(tree,data$data)
}