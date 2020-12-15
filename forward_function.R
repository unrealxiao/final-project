

library(dplyr)
onestep_forward_selection = function(respons_e, star_t, en_d) {
  #in this function, respons_e is the response vector, star_t is the data frame that contains all
  #the predictors in the current model,
  #en_d is data.frame contains all the predictors that are not included in the current model
  original = star_t
  mstart = lm(respons_e ~ ., data = star_t)
  AIC_score = rep(NA, dim(en_d)[2])
  AIC_mstart = AIC(mstart)
  for (i in 1:dim(en_d)[2]) {
    newdata = star_t
    newdata$new_data = unlist(en_d[, i])
    names(newdata)[dim(newdata)[2]] = colnames(en_d[i])
    mtest = lm(respons_e ~., data = newdata)
    AIC_score[i] = AIC(mtest)
  }
  if (min(AIC_score) <= AIC_mstart) {
    index = which.min(AIC_score)
    mydata = star_t
    mydata$new_data = en_d[, index]
    names(mydata)[dim(mydata)[2]] = colnames(en_d[index])
    return(list(mydata, "go"))
  }
  else {
    return(list(original, "stop"))
  }
}

selection_function = function(respons, begin_data, full_data) {
  #the respons variable represent the responses in the model, the begin_data is the data frame
  #that includes the predictors in our initial model
  #full_data is the full data frame.
  dro = colnames(begin_data)#we extract the column names of all the predictors in our initial model
  en_dd = full_data %>%
    select(-one_of(dro))
  sta_t = begin_data
  while(TRUE) {
    result = onestep_forward_selection(respons_e = respons, star_t = sta_t, en_d = en_dd)
    if (result[[2]] == "go") {
      sta_t = result[[1]]
      dropp = colnames(sta_t)
      en_dd = full_data %>%
        select(-one_of(dropp))
      if (dim(en_dd)[2] == 0) {
        return(result[[1]])
        break
      }
    }
    else if (result[[2]] == "stop") {
      return(result[[1]])
      break
    }
  }
}

