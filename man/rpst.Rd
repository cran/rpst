\name{rpst}
\alias{rpst}
\title{Recursive Partitioning Survival Trees}
\description{
   Build recursive partitioning survival trees via a node-splitting rule that builds decision tree models that reflected within-node and within-treatment responses.
}
\usage{
rpst(data, datapath, maxlay = 12, minsize = 5)
}
\arguments{
  \item{data}{Input dataframe, it should be set in the particular way showed in detail.}
  \item{datapath}{The file path of the input dataframe.}
  \item{maxlay}{The maximum layer of the survival tree.}
  \item{minsize}{The minimum sample size in the each leaf node of the survival tree.}
}
\details{
The input dataframe should be set in the following way:
  (i) The variable names are specified in the first line in the data file. 
  (ii) The variable types are specified in the second line in the data file. 
  The variable types can include treatment (1), survival time and censor status (2), ordered (3), binary (4), nominal (5), and unwanted (6) variables. 
 
  For potential predictors, they are categorized in three types, including ordered, binary, and nominal ones. 
  
  Specifically, ordered covariates include continuous and ordinal data types, binary covariates must have only two factors (they are typically coded as 0 and 1), and nominal covariates must have more than two factors. The number in the parenthesis represents each variable type.
}

\references{
 Zhang, H., Legro, R. S., Zhang, J., Zhang, L., Chen, X., Huang, H., ... & Eisenberg, E. (2010). Decision trees for identifying predictors of treatment effectiveness in clinical trials and its application to ovulation in a study of women with polycystic ovary syndrome.. Human Reproduction, 25(10), 2612-2621.
}

\author{Yewei Li}

\seealso{
\code{\link{print.rpst}}, \code{\link{predict.rpst}}, \code{\link{plot.rpst}}.
}

\examples{
#require(survival)
data <- gen.data(n = 20, p = 5, c = 2)
tree <- rpst(data$data, minsize = 2)
print.rpst(tree)
}

