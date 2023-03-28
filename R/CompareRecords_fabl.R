CompareRecords_fabl <- function(df1, df2, flds=NULL, flds1=NULL, flds2=NULL, types=NULL, breaks=c(0,.25,.5)){

  warn <- FALSE

  # control the input

  if(!is.data.frame(df1)) stop("'df1' is not a data frame")
  if(!is.data.frame(df2)) stop("'df2' is not a data frame")

  if(!is.null(flds)){ # user provides 'flds' fields in 'df1' and 'df2' for linkage
    if( !(class(flds) %in% c("character", "numeric")) )
      stop("'flds' should be of class character or numeric")

    if(!is.null(flds1)) warning("Argument 'flds' was provided, 'flds1' is ignored")
    if(!is.null(flds2)) warning("Argument 'flds' was provided, 'flds2' is ignored")

    flds1 <- flds2 <- flds

    if(is.numeric(flds)){
      fldsCheck1 <- flds %in% seq_len(ncol(df1))
      if(!all(fldsCheck1)) stop("Some numbers in 'flds' are out of the column range for 'df1'")
      fldsCheck2 <- flds %in% seq_len(ncol(df2))
      if(!all(fldsCheck2)) stop("Some numbers in 'flds' are out of the column range for 'df2'")
      warn <- TRUE
    }
    if(is.character(flds)){
      fldsCheck1 <- flds %in% colnames(df1)
      if(!all(fldsCheck1)) stop("Some names in 'flds' are not columns of 'df1'")
      fldsCheck2 <- flds %in% colnames(df2)
      if(!all(fldsCheck2)) stop("Some names in 'flds' are not columns of 'df2'")
      flds1 <- match(flds1, colnames(df1))
      flds2 <- match(flds2, colnames(df2))
    }
  }else{
    if(is.null(flds1) & !is.null(flds2)) stop("Argument 'flds1' also needs to be specified")
    if(is.null(flds2) & !is.null(flds1)) stop("Argument 'flds2' also needs to be specified")
    if(is.null(flds2) & is.null(flds2)){
      flds1 <- flds2 <- intersect(colnames(df1), colnames(df2))
      if(length(flds1)==0)
        stop("'df1' and 'df2' do not have fields with the same names")
      flds1 <- match(flds1, colnames(df1))
      flds2 <- match(flds2, colnames(df2))
    }else{ # user provides 'flds1' and 'flds2' fields in 'df1' and 'df2' for linkage
      if( !(class(flds1) %in% c("character", "numeric")) )
        stop("'flds1' should be of class character or numeric")
      if( !(class(flds2) %in% c("character", "numeric")) )
        stop("'flds2' should be of class character or numeric")
      if(is.numeric(flds1)){
        flds1Check <- flds1 %in% seq_len(ncol(df1))
        if(!all(flds1Check)) stop("Some numbers in 'flds1' are out of the column range for 'df1'")
      }
      if(is.numeric(flds2)){
        flds2Check <- flds2 %in% seq_len(ncol(df2))
        if(!all(flds2Check)) stop("Some numbers in 'flds2' are out of the column range for 'df2'")
      }
      if(is.character(flds1)){
        flds1Check <- flds1 %in% colnames(df1)
        if(!all(flds1Check)) stop("Some names in 'flds1' are not columns of 'df1'")
        flds1 <- match(flds1, colnames(df1))
      }
      if(is.character(flds2)){
        flds2Check <- flds2 %in% colnames(df2)
        if(!all(flds2Check)) stop("Some names in 'flds2' are not columns of 'df2'")
        flds2 <- match(flds2, colnames(df2))
      }
      if(length(flds1)!=length(flds2)) stop("'flds1' and 'flds2' should have the same length")
    }
  }

  n1 <- nrow(df1)
  n2 <- nrow(df2)

  if(n2 > n1) stop("'df2' has more records than 'df1'")

  F <- length(flds1)

  if(is.null(types)) types <- rep("lv",F)

  if(F != length(types))
    stop("Length of 'types' does not equal number of fields used for comparison")

  if( (!is.character(types)) | any( !(types %in% c("lv","bi","nu")) ) )
    stop("'types' should be a character vector of 'lv', 'bi', and/or 'nu' indicating Levenshtein-based,
		binary, or numeric comparisons")

  c1 <- sapply(df1[,flds1], class)
  c2 <- sapply(df2[,flds2], class)

  if( any(types=="nu") & any(c1[types=="nu", drop=FALSE] != "numeric") )
    stop(paste("Numeric comparison requested for columns '", paste(colnames(df1)[flds1[types=="nu"]],collapse="' '"),
               "' in 'df1', but at least one of these is not numeric", sep=""))

  if( any(types=="nu") & any(c2[types=="nu", drop=FALSE] != "numeric") )
    stop(paste("Numeric comparison requested for columns '", paste(colnames(df2)[flds2[types=="nu"]],collapse="' '"),
               "' in 'df2', but at least one of these is not numeric", sep=""))

  if( !(class(breaks) %in% c("list", "numeric")) )
    stop("'breaks' should be of class list or numeric")

  if( class(breaks) == "list" ){ # if user specifies list with breaks
    if(length(breaks) == F){ # breaks for each comparison field
      for(fld in 1:F){ # check the breaks provided for each field
        if(types[fld]=="lv"){
          if( (!is.numeric(breaks[[fld]])) | any(breaks[[fld]] < 0 | breaks[[fld]] > 1) )
            stop(paste("'types[",fld,"]' specified as 'lv', so 'breaks[[",fld,"]]' should be a vector of numbers in [0,1]",
                       sep=""))
          breaks[[fld]] <- unique(c(-Inf,breaks[[fld]],Inf))
        }
        if(types[fld]=="nu"){
          if( (!is.numeric(breaks[[fld]])) | any(breaks[[fld]] < 0) )
            stop(paste("'types[",fld,"]' specified as 'nu', so 'breaks[[",fld,"]]' should be a vector of non-negative numbers",
                       sep=""))
          breaks[[fld]] <- unique(c(-Inf,breaks[[fld]],Inf))
        }
      }
    }else{ # if length of list is not F, then list needs to be named with elements 'lv' and 'nu'
      if( (length(breaks) != 2) | (!all(names(breaks) %in% c("lv","nu"))) )
        stop("When 'breaks' is specified as a list, it should either have length equal to the number of comparison fields, or be a named list with elements 'lv' and 'nu'")
      if( (!is.numeric(breaks$lv)) | any(breaks$lv < 0 | breaks$lv > 1) )
        stop("'breaks$lv should be a vector of numbers in [0,1]")
      breaks$lv <- unique(c(-Inf,breaks$lv,Inf))
      if( (!is.numeric(breaks$nu)) | any(breaks$nu < 0) )
        stop("'breaks$nu should be a vector of non-negative numbers")
      breaks$nu <- unique(c(-Inf,breaks$nu,Inf))

      breaks1 <- list()
      breaks1[1:F] <- list(NULL)
      breaks1[types=="nu"] <- list(breaks$nu)
      breaks1[types=="lv"] <- list(breaks$lv)
      breaks <- breaks1
    }
  }

  if( class(breaks) == "numeric" ){ # if user specifies only one vector with breaks for all non-binary comparison fields
    if( any(breaks < 0) )
      stop("When 'breaks' is specified as a numeric vector, it should contain non-negative numbers")
    if( all(types %in% c("lv","bi")) & any(breaks > 1) )
      stop("When 'breaks' is specified as a numeric vector, it should contain numbers in [0,1] if all the non-binary comparisons in 'types' are 'lv'")

    breaks <- unique(c(-Inf,breaks,Inf))
    breaks1 <- list()
    breaks1[1:F] <- list(breaks)
    breaks <- breaks1
  }

  pairInds1 <- rep(1:n1, n2)
  pairInds2 <- rep(1:n2, each=n1)

  comparisons <- list()

  for(fld in 1:F){
    if(types[fld]=="bi"){
      # computing agreement levels for binary comparisons
      if( c1[fld] != c2[fld] )
        warning(paste("Columns '", colnames(df1)[flds1[fld]], "' in 'df1' and '" ,
                      colnames(df2)[flds2[fld]], "' in 'df2' are of different classes", sep=""))
      if( (c1[fld] == "factor")&(c2[fld] == "factor") ){
        levs1 <- sort(levels(df1[,flds1[fld]]))
        levs2 <- sort(levels(df2[,flds2[fld]]))
        if(!identical(levs1,levs2)){
          warning(paste("Columns '", colnames(df1)[flds1[fld]], "' in 'df1' and '" ,
                        colnames(df2)[flds2[fld]], "' in 'df2' are factors with different levels", sep=""))
          unilevs <- union(levs1,levs2)
          df1[,flds1[fld]] <- factor(df1[,flds1[fld]], levels = unilevs)
          df2[,flds2[fld]] <- factor(df2[,flds2[fld]], levels = unilevs)
        }
      }
      same <- df1[pairInds1,flds1[fld]] == df2[pairInds2,flds2[fld]]
      AgrLev <- 1*same
      AgrLev[!same] <- 2
      comparisons[[fld]] <- as.factor(AgrLev, levels = c(1, 2))
    }
    if(types[fld]=="nu"){
      absDiff <- abs(df1[pairInds1,flds1[fld]] - df2[pairInds2,flds2[fld]])
      AgrLev <- cut(absDiff, breaks=breaks[[fld]], labels=seq_len(length(breaks[[fld]])-1))
      comparisons[[fld]] <- as.factor(AgrLev, levels = seq_len(length(breaks[[fld]]) + 1))
    }
    if(types[fld]=="lv"){
      # computing agreement levels for Levenshtein-based comparisons
      df1[,flds1[fld]] <- as.character(df1[,flds1[fld]])
      df2[,flds2[fld]] <- as.character(df2[,flds2[fld]])

      # adist in the utils package returns the matrix of Levenshtein distances
      lvd <- as.numeric(utils::adist(df1[,flds1[fld]], df2[,flds2[fld]]))/
        pmax(nchar(df1[,flds1[fld]])[pairInds1], nchar(df2[,flds2[fld]])[pairInds2])

      AgrLev <- cut(lvd, breaks=breaks[[fld]], labels=seq_len(length(breaks[[fld]])-1))
      comparisons[[fld]] <- as.factor(AgrLev, levels = seq_len(length(breaks[[fld]]) + 1))
    }
  }

  nDisagLevs <- sapply(comparisons, FUN=nlevels)

  # next four lines compute the binary indicators from the agreement levels
  xfun <- function(f) paste("(comparisons[[",f,"]]==",seq_len(nDisagLevs[f]),")")
  expr1 <- paste(sapply(sapply(seq_len(F),xfun),FUN=paste,collapse=","),collapse=",")
  expr2 <- paste("cbind(",expr1,")")
  comparisons <- eval(parse(text=expr2))

  # replacing NAs by FALSE or zeroes is justified by ignorability and CI assumptions (see Sadinle 2017)
  comparisons[is.na(comparisons)] <- FALSE

  df1Fields <- colnames(df1)[flds1]
  df2Fields <- colnames(df2)[flds2]
  compFields <- data.frame(file1=df1Fields, file2=df2Fields, types=types)

  # if(any(df1Fields != df2Fields) | warn){
  #   warning(paste("The fields '", paste(df1Fields,collapse="' '"),
  #                 "' in 'df1' are being compared with the fields '",
  #                 paste(df2Fields,collapse="' '"), "' in 'df2'",
  #                 sep=""))
  #}

  out <- list(comparisons=comparisons, n1=n1, n2=n2, nDisagLevs=nDisagLevs,
              compFields=compFields)

  return(out)
}
