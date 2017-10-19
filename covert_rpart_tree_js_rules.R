rules.rpart.tree.js <- function(model)
{
      if (!inherits(model, "rpart")) stop("Not a valid rpart tree")
      # Assign a frame object , get rule names, get the total size of tree ( get the 1st one)
      tframe     <- model$frame
      tnames   <- row.names(frm)
      for (i in 1:nrow(tframe))
      {
            if (tframe[i,1] == "<leaf>")
            {
                  cat("\n")
                  cat(sprintf("## Rule number: %s \n", tnames[i]))
                  pth <- path.rpart(model, nodes=as.numeric(tnames[i]), print.it=FALSE)
                  cat("if ( ")
                  cat(sprintf("   %s ", unlist(pth)[-1]), sep="&")
                  cat(" )", "{ ",sprintf("yval=%s", tframe[i,]$yval),";}  ## Obs = ",tframe[i,]$n, " - Coverage : ",round(100*tframe[i,]$n/(tframe[1,]$n)),"%")
            }
      }
}
