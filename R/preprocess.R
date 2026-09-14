processOneFile <- function(file, first_condition) {
  # Read the file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  idx <- c(1,which(!is.na(df$phase.thisRepN)))
  
  # add blocks:
  df$block <- 1
  for (i in 2:length(idx)) {
    df$block[idx[i-1]:idx[i]] <- df$phase.thisRepN[idx[i]] + 1
  }
  
  df$congruence <- c(NA, ifelse(df$correctAnswer[2:dim(df)[1]] == df$correctAnswer[1:(dim(df)[1]-1)], 1, 0))
  
  # skip the first trial of each block:
  df$trial <- df$trials.thisRepN + 1
  df <- df[which(df$trial > 1),]
  
  df$stimprop <- c('[-0.125,0.125]'='shape', '[0.125,0.125]'='shape', '[-0.125,-0.125]'='dots', '[0.125,-0.125]'='dots')[df$gridLocation]
  
  df$music <- df$group
  df$rt    <- df$key_resp.rt
  
  if (first_condition == df$group[1]) {
    df$run <- 1
  } else {
    df$run <- 2
  }
  
  interesting_columns <- c('participant', 
                           'run',
                           'music', 
                           'block', 
                           'trial', 
                           'task',
                           'condition',
                           'accuracy',
                           'rt',      
                           'stimprop',
                           'congruence')
  
  df <- df[, interesting_columns]
  
  return(df)

}


processParticipants <- function() {
  
  df <- read.csv('data/demographics.csv', stringsAsFactors = FALSE)
  IDs <- df$ID
  
  classicRTs <- NA
  classicAccs <- NA
  congruentRTs <- NA
  stimpropRTs <- NA
  orderRTs <- NA
  
  for (ID in IDs) {
    files <- list.files(path = 'data/raw', pattern = sprintf('%s',ID))

    ppdf <- NA
    
    for (file in files) {

      tsw_df <- processOneFile(sprintf('data/raw/%s', file), first_condition = df[which(df$ID == ID),]$first_condition)
      
      if (is.data.frame(ppdf)) {
        ppdf <- rbind(ppdf, tsw_df)
      } else {
        ppdf <- tsw_df
      }
      
    }

    idx <- sort(union(which(ppdf$block > 2), which(ppdf$trial > 8)))
    late_df <- ppdf[idx,]
    
    crt_df <- late_df[which(late_df$accuracy == 1),]
    crt_df <- crt_df[which(crt_df$rt > 0.2 & crt_df$rt < 3),]
    crt_df$condition <- sprintf('%s-%s', crt_df$task, crt_df$condition )
    
    # # #   CLASSIC RTs   # # #
    
    agg_crt_df <- aggregate( rt ~ participant + music + condition, data = crt_df, FUN = median)

    if (is.data.frame(classicRTs)) {
      classicRTs <- rbind(classicRTs, agg_crt_df)
    } else {
      classicRTs <- agg_crt_df
    }
    
    
    # # #   ORDER RTs   # # #
    
    agg_ord_df <- aggregate( rt ~ participant + music + run + condition, data = crt_df, FUN = median)
    
    if (is.data.frame(orderRTs)) {
      orderRTs <- rbind(orderRTs, agg_ord_df)
    } else {
      orderRTs <- agg_ord_df
    }
    
    
    # # #   CONGRUENT RTs   # # #
    
    agg_cng_df <- aggregate( rt ~ participant + music + condition + congruence, data = crt_df, FUN = median)
    
    if (is.data.frame(congruentRTs)) {
      congruentRTs <- rbind(congruentRTs, agg_cng_df)
    } else {
      congruentRTs <- agg_cng_df
    }
    
    # # #   CLASSIC ACCURACIES   # # #
    
    acc_df <- late_df[which(late_df$rt > 0.2 & late_df$rt < 3),]
    acc_df$condition <- sprintf('%s-%s', acc_df$task, acc_df$condition )
    
    agg_acc_df <- aggregate( accuracy ~ participant + music + condition, data = acc_df, FUN = mean)
    
    if (is.data.frame(classicAccs)) {
      classicAccs <- rbind(classicAccs, agg_acc_df)
    } else {
      classicAccs <- agg_acc_df
    }
    
  }
  
  print(str(orderRTs))
  
  return( list( classicRTs   = classicRTs,
                orderRTs     = orderRTs,
                congruentRTs = congruentRTs,
                classicAccs  = classicAccs
                ) )
  
}


