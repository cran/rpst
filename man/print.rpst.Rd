\name{print.rpst}
\alias{print.rpst}
\title{Print out a "rpst" object}
\description{
Print node and node information details of "\code{rpst}".
}
\usage{
\method{print}{rpst}(x, ...)
}

\arguments{
  \item{x}{a "\code{rpst}" project.}
  \item{\dots}{additional summary arguments.}
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