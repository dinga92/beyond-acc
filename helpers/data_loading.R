
load_oasis <- function(y='gender', modality='wm'){
  # load data
  oasis_pheno <- read.csv('../data/oasis_cross-sectional.csv')
  if (modality == 'wm'){
    oasis_100pc <- read.csv('../data/oasis_wm_100pc.csv', header = F)
  }
  else if (modality == 'gm') {
    oasis_100pc <- read.csv('../data/oasis_gm_100pc.csv', header = F)
  }
  # get ID's strings in order
  oasis_100pc[,1] <- as.factor(unlist(lapply(strsplit(as.character(oasis_100pc[,1]),
                                                      split = "'"),
                                             function(x){x[2]})))
  names(oasis_100pc)[1] <- 'ID'
  # throw out irrelevant collumns
  if (y=='gender'){
    oasis_pheno <- oasis_pheno[, c('ID', 'M.F')]
    # rename the target column
    names(oasis_pheno) <- c('ID', 'y')
    # transofrm target values to 0-1
    oasis_pheno$y <- as.numeric(oasis_pheno$y) - 1
  } else if (y == 'diagnosis') {
    oasis_pheno <- oasis_pheno[, c('ID', 'CDR')]
    names(oasis_pheno) <- c('ID', 'y')
    oasis_pheno$y <- oasis_pheno$y > 0
    oasis_pheno$y[is.na(oasis_pheno$y)] <- 0
  }
  # merge predictors and target values
  oasis_data <- merge(oasis_100pc, oasis_pheno)
  # throw out ID column
  oasis_data <- oasis_data[, -1]
}


load_abide <- function(){
  thickness <- read.csv('../data/abide_thickness.csv', sep='\t')
  phenotype <- read.csv('../data/Phenotypic_V1_0b_preprocessed1.csv', sep=',')

  thickness <- thickness[,-2]
  phenotype <- phenotype[,c('FILE_ID', 'SEX')]
  names(phenotype) <- c('File', 'y')
  phenotype$y <- as.numeric(phenotype$y) -1

  # delete mostly invariant features
  thickness <- thickness[, c(TRUE, apply(thickness[, -1], 2, sd) > 0.001)]
  thickness <- thickness[, c(TRUE, apply(thickness[, -1], 2, function(x){
    length(unique(x))}) > 3)]
  abide_data <- merge(thickness, phenotype)
  # delete filename column
  abide_data <- abide_data[, -1]
}


fetch_shopping_data <- function(){
  shopping.data <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/00468/online_shoppers_intention.csv"), header = T)
  shopping.data$Revenue <- as.numeric(shopping.data$Revenue)
  names(shopping.data)[18] <- 'y'
  shopping.data <- fastDummies::dummy_columns(shopping.data)
  shopping.data <- shopping.data[, -c(11, 16)]
  return(shopping.data)
}


fetch_magic_data <- function(){
  magic.data <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/magic/magic04.data"), header=F)
  names(magic.data)[11] <- 'y'
  magic.data$y <- as.numeric(magic.data$y) - 1
  return(magic.data)
}


fetch_adult_data <- function(){
  adult.data <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"), header = F)
  names(adult.data)[15] <- 'y'
  adult.data$y <- as.numeric(adult.data$y) - 1
  adult.data <- fastDummies::dummy_cols(adult.data)
  adult.data <- adult.data[,-c(2,4,6,7,8,9,10,14)]
}


fetch_mushrooms <- function(){
  mushrooms.data <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header = F)
  names(mushrooms.data)[1] <- 'y'
  mushrooms.data$y <- as.numeric(mushrooms.data$y) - 1
  mushrooms.data <- fastDummies::dummy_cols(mushrooms.data)
  mushrooms.data <- mushrooms.data[, -c(2:23)]
  return(mushrooms.data)
}
