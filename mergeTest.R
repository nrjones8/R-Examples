# Nick Jones -- demonstrates the difference in merging two data frames (with some overlap in row names, but not all)
# when using by = 'row.names' vs. a column storing 'Name'


# Added in this one line
slowMerging <- function(totalRows = 8000, subsetRows = 7000)
{
  getRandStr <- function(length = 8)
  {
    return(paste(sample(c(0:9, letters, LETTERS),
                      length, replace=TRUE), collapse=""))
  }
  # Create two random matrices of equal size
  totalRows <- 8000
  # Number of rows to keep in subset
  subsetRows <- 7000
  mat1 <- matrix(rnorm(totalRows*2), ncol = 2)
  mat2 <- matrix(rnorm(totalRows*2), ncol = 2)
  # Give two matricies same rownames
  randRownames <- replicate(totalRows, getRandStr())
  rownames(mat1) <- randRownames
  rownames(mat2) <- randRownames
  # Keep only a subset of each (NOT the same subset)
  mat1 <- mat1[sample(nrow(mat1), subsetRows), ]
  mat2 <- mat2[sample(nrow(mat2), subsetRows), ]

  # Merge by row names, keep everything -- takes forever!
  # Takes same amount of time for matrix vs. data frame
  #merge.mat <- system.time(mergedMat <- merge(mat1, mat2, by = 'row.names', all = T))
  df1 <- data.frame(mat1)
  df2 <- data.frame(mat2)
  merge.df <- system.time(mergedDF <- merge(df1, df2, by = 'row.names', all = T))

  # Now create separate column for 'Name' in the two data frames;
  # Merge based on column 'Name', not row.names
  df1[, 'Name'] <- rownames(df1)
  df2[, 'Name'] <- rownames(df2)

  merge.df.byCol <- system.time(mergedDF.byCol <- merge(df1, df2, by = 'Name', all = T))
  print('Using row.names:')
  print(merge.df)
  print('Using column "Name"')
  print(merge.df.byCol)
}
