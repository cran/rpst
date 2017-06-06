\name{predict.rpst}
\alias{predict.rpst}
\title{Make predictions from a "rpst" object}
\description{
 Similar to other predict methods, which returns predictions from a fitted "\code{rpst}" object.
}
\usage{
\method{predict}{rpst}(object, newdata,...)
}

\arguments{
  \item{object}{Output from the \code{rpst} function.}
  \item{newdata}{New data used for prediction.}
  \item{\dots}{additional summary arguments.}
}

\value{\code{predict.rpst} returns a dataframe that contain a new column "class", which contains the class labels of each sample. }

\references{
 Zhang, H., Legro, R. S., Zhang, J., Zhang, L., Chen, X., Huang, H., ... & Eisenberg, E. (2010). Decision trees for identifying predictors of treatment effectiveness in clinical trials and its application to ovulation in a study of women with polycystic ovary syndrome.. Human Reproduction, 25(10), 2612-2621.
}

\author{Yewei Li}

\seealso{
\code{\link{print.rpst}}, \code{\link{predict.rpst}}, \code{\link{plot.rpst}}.
}

\examples{
#require(survival)
##not run##
#data <- gen.data(n = 100, p = 10, c = 2)
#tree <- rpst(data$data, minsize = 2)
#class = predict.rpst(tree,data$data)$class

data <- gen.data(n = 20, p = 5, c = 2)
tree <- rpst(data$data, minsize = 2)
class = predict.rpst(tree,data$data)$class
}