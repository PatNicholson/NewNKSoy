# TODO: Add comment
# 
# Author: Patrick
###############################################################################

clean_data <- function(X,Checks){
	X = transform(X, EXPERIMENT = as.factor(EXPERIMENT))
	X = transform(X, LOCATION = as.factor(LOCATION))
	X = transform(X, FAMILY = as.factor(FAMILY))
	X = transform(X, VARIETY = as.factor(VARIETY))
	X = transform(X, REPNO = as.factor(REPNO))
	X = transform(X, BAGSOLD = as.numeric(BAGSOLD))
	X = transform(X, GRAD = as.logical(GRAD=="YES"))
	X[is.na(X$GRAD)] <- FALSE
	X = transform(X, CLASS_OF = as.factor(CLASS_OF))
	X[X == "."] = NA
	return(X[X$CHECK==Checks,])
}

find_sig <- function(X,col){
	idx = match(col,colnames(X))
	locs = unique(X[,idx])
	vals = matrix(0,length(locs),4)
	i = 1
	for(l in locs){
		lm.fit = lm(GRAD~YIELD,data=X[X[,idx] == l,])
		lm.sum = summary(lm.fit)
		vals[i,1] = as.numeric(l)
		vals[i,2] = lm.sum$adj.r.squared
		vals[i,3] = dim(X[X[,idx] == l,])[1]
		vals[i,4] = length(unique(X[X[,idx] == l,]$VARIETY))
		i = i + 1;
	}
	return(vals)
}

time_series <- function(X, means){
	#X = clean_data(X)
	varieties = unique(X$VARIETY)
	vals = data.frame(VARIETY=character(),BAGS=integer(),YEAR=integer(),YEAR3=integer(),
			YIELD3=integer(),MAX3=integer(),MIN3=integer(),STDEV3=integer(),YEAR2=integer(),
			YIELD2=integer(),MAX2=integer(),MIN2=integer(),STDEV2=integer(),YEAR1=integer(),
			YIELD1=integer(),MAX1=integer(),MIN1=integer(),STDEV1=integer(),RM=integer(),stringsAsFactors=FALSE)
	i = 1
	prog = 1
	X = transform(X, CLASS_OF = as.numeric(CLASS_OF))
	for(v in varieties){
		exps = X[X$VARIETY == v,]
		min = summary(exps$YEAR)[1]
		if(nrow(exps[exps$BAGSOLD >= 0,]) > 0){
			vals[i,1] = v
			vals[i,2] = exps[1,12]
			if(!is.na(exps[1,10])){
				vals[i,3] = exps[1,10]+2009
			}
			else{
				vals[i,3] = exps[1,1]
			}
			vals[i,4] = 0
			vals[i,5] = 0
			vals[i,6] = 0
			vals[i,7] = 0
			vals[i,8] = 0
			vals[i,9] = 0
			vals[i,10] = 0
			vals[i,11] = 0
			vals[i,12] = 0
			vals[i,13] = 0
			vals[i,14] = 0
			vals[i,15] = 0
			vals[i,16] = 0
			vals[i,17] = 0
			vals[i,18] = 0
			vals[i,19] = exps[1,]$RM
			vec1 = c()
			vec2 = c()
			vec3 = c()
			for(y in 1:nrow(exps)){
				#browser()
				mn = means[means$EXPER==exps[y,2],]
				mn = mn[mn$YEAR==exps[y,1],][,3]
				vecval = (exps[y,9])/mn
				if(as.numeric(exps[y,1]) == (min+2)){
					vec1[length(vec1)+1] = vecval
					vals[i,4] = as.numeric(exps[y,1])
				} else if(as.numeric(exps[y,1]) == (min+1)){
					vec2[length(vec2)+1] = vecval
					vals[i,9] = as.numeric(exps[y,1])
				} else{
					vec3[length(vec3)+1] = vecval
					vals[i,14] = as.numeric(exps[y,1])
				}
			}
			if(length(vec1) > 0){
				vals[i,5] = mean(vec1)
				vals[i,6] = max(vec1)
				vals[i,7] = min(vec1)
				vals[i,8] = sd(vec1)
			}
			if(length(vec2) > 0){
				vals[i,10] = mean(vec2)
				vals[i,11] = max(vec2)
				vals[i,12] = min(vec2)
				vals[i,13] = sd(vec2)
			}
			if(length(vec3) > 0){
				vals[i,15] = mean(vec3)
				vals[i,16] = max(vec3)
				vals[i,17] = min(vec3)
				vals[i,18] = sd(vec3)
			}
			if(!is.na(vals[i,2])){
				if(vals[i,5] == 0 && vals[i,6] == vals[i,3]){
					vals[i,4] = vals[i,9]
					vals[i,5] = vals[i,10]
					vals[i,6] = vals[i,11]
					vals[i,7] = vals[i,12]
					vals[i,8] = vals[i,13]
					vals[i,9] = vals[i,14]
					vals[i,10] = vals[i,15]
					vals[i,11] = vals[i,16]
					vals[i,12] = vals[i,17]
					vals[i,13] = vals[i,18]
					vals[i,14] = 0
					vals[i,15] = 0
					vals[i,16] = 0
					vals[i,17] = 0
					vals[i,18] = 0
				}
				if(vals[i,5] == 0 && vals[i,8] == vals[i,3]){
					vals[i,4] = vals[i,14]
					vals[i,5] = vals[i,15]
					vals[i,6] = vals[i,16]
					vals[i,7] = vals[i,17]
					vals[i,8] = vals[i,18]
					vals[i,14] = 0
					vals[i,15] = 0
					vals[i,16] = 0
					vals[i,17] = 0
					vals[i,18] = 0
				}
			}
			i = i+1
			prog = prog+1
			if(prog > 500){
				print(".")
				prog = 1
			}
		}
	}
	return(vals)
}

aggregate <- function(X, means){
	#X = clean_data(X)
	varieties = unique(X$VARIETY)
	vals = data.frame(VARIETY=character(),BAGS=integer(),YEAR=integer(),YEAR3=integer(),
			YIELD=integer(),MAX=integer(),MIN=integer(),STDEV=integer(),RM=integer(),stringsAsFactors=FALSE)
	i = 1
	prog = 1
	X = transform(X, CLASS_OF = as.numeric(CLASS_OF))
	for(v in varieties){
		exps = X[X$VARIETY == v,]
		min = summary(exps$YEAR)[1]
		if(nrow(exps[exps$BAGSOLD >= 0,]) > 0){
			vals[i,1] = v
			vals[i,2] = exps[1,12]
			if(!is.na(exps[1,10])){
				vals[i,3] = exps[1,10]+2009
			}
			else{
				vals[i,3] = exps[1,1]
			}
			vals[i,4] = 0
			vals[i,5] = 0
			vals[i,6] = 0
			vals[i,7] = 0
			vals[i,8] = 0
			vals[i,9] = exps[1,]$RM
			vec1 = c()
			for(y in 1:nrow(exps)){
				#browser()
				mn = means[means$EXPER==exps[y,2],]
				mn = mn[mn$YEAR==exps[y,1],][,3]
				if(1==1){
					vec1[length(vec1)+1] = (exps[y,9]/mn)
					vals[i,4] = as.numeric(exps[y,1])
				}
			}
			if(length(vec1) > 0){
				vals[i,5] = mean(vec1)
				vals[i,6] = max(vec1)
				vals[i,7] = min(vec1)
				vals[i,8] = sd(vec1)
			}
			i = i+1
			prog = prog+1
			if(prog > 500){
				print(".")
				prog = 1
			}
		}
	}
	return(vals)
}

exp_means <- function(X){
	vals = data.frame(EXPER=character(),YEAR=integer(),MEAN=integer(),stringsAsFactors=FALSE)
	i = 1
	exp = unique(X$EXPERIMENT)
	for(e in exp){
		exp_subset = X[X$EXPERIMENT == e,]
		for(yr in unique(exp_subset$YEAR)){
			vals[i,1] = e
			vals[i,2] = yr
			vals[i,3] = mean(exp_subset[exp_subset$YEAR==yr,]$YIELD)
			i = i+1
		}
	}
	return(vals)
}

crossval = function(formula1,data1,folds,reps){
	total = 0
	for(x in 1:reps){
		interval = round(nrow(data1)/folds)
		err = 0
		for(i in 1:folds){
			test = ((i-1)*interval+1):(i*interval)
			train = (-test)
			fit = lm(formula=formula1,data=data1[train,])
			pred = predict(fit,newdata=data1[test,])
			err = err + (mean((pred - data1[test,]$BAGS)^2))^(1/2)
			if(is.na(err)){
				browser()
			}
		}
		err = err / folds
		total = total + err
	}
	return(total/reps)
}

crossval_glm = function(data1,folds,reps,alpha){
	total = 0
	for(x in 1:reps){
		interval = round(nrow(data1)/folds)
		err = 0
		for(i in 1:folds){
			test = ((i-1)*interval+1):(i*interval)
			train = (-test)
			y = data1[train,]$BAGS
			x = model.matrix(BAGS~.,data1[train,])[,-1]
			cv.out=cv.glmnet(x,y,alpha=alpha)
			bestlam=cv.out$lambda.min
			ridge.mod=glmnet(x,y,alpha=alpha,lambda=bestlam)
			pred = predict(ridge.mod,newx=model.matrix(BAGS~.,data1[test,])[,-1])
			err = err + (mean((pred - data1[test,]$BAGS)^2))^(1/2)
			if(is.na(err)){
				browser()
			}
		}
		err = err / folds
		total = total + err
	}
	return(total/reps)
}

fillIn = function(data1,ts12,ts23){
	fit12 = lm(YIELD2~YIELD1+MAX1+MIN1+STDEV1+RM,data=ts12)
	fit12max = lm(MAX2~YIELD1+MAX1+MIN1+STDEV1+RM,data=ts12)
	fit12min = lm(MIN2~YIELD1+MAX1+MIN1+STDEV1+RM,data=ts12)
	fit12sd = lm(STDEV2~YIELD1+MAX1+MIN1+STDEV1+RM,data=ts12)
	
	fit23 = lm(YIELD3~YIELD2+MAX2+MIN2+STDEV2+RM,data=ts23)
	fit23max = lm(MAX3~YIELD2+MAX2+MIN2+STDEV2+RM,data=ts23)
	fit23min = lm(MIN3~YIELD2+MAX2+MIN2+STDEV2+RM,data=ts23)
	fit23sd = lm(STDEV3~YIELD2+MAX2+MIN2+STDEV2+RM,data=ts23)
	
	fit21 = lm(YIELD1~YIELD2+MAX2+MIN2+STDEV2+RM,data=ts12)
	fit21max = lm(MAX1~YIELD2+MAX2+MIN2+STDEV2+RM,data=ts12)
	fit21min = lm(MIN1~YIELD2+MAX2+MIN2+STDEV2+RM,data=ts12)
	fit21sd = lm(STDEV1~YIELD2+MAX2+MIN2+STDEV2+RM,data=ts12)
	
	fit32 = lm(YIELD2~YIELD3+MAX3+MIN3+STDEV3+RM,data=ts23)
	fit32max = lm(MAX2~YIELD3+MAX3+MIN3+STDEV3+RM,data=ts23)
	fit32min = lm(MIN2~YIELD3+MAX3+MIN3+STDEV3+RM,data=ts23)
	fit32sd = lm(STDEV2~YIELD3+MAX3+MIN3+STDEV3+RM,data=ts23)
	
	for(i in 1:nrow(data1)){
		if(data1[i,]$YIELD2 == 0 && data1[i,]$YIELD1 > 0){
			data1[i,]$YIELD2 = predict(fit12,newdata=data1[i,])
			data1[i,]$MAX2 = predict(fit12max,newdata=data1[i,])
			data1[i,]$MIN2 = predict(fit12min,newdata=data1[i,])
			data1[i,]$STDEV2 = predict(fit12sd,newdata=data1[i,])
			data1[i,]$YEAR2 = data1[i,]$YEAR1 + 1
		}
		if(data1[i,]$YIELD3 == 0 && data1[i,]$YIELD2 > 0){
			data1[i,]$YIELD3 = predict(fit23,newdata=data1[i,])
			data1[i,]$MAX3 = predict(fit23max,newdata=data1[i,])
			data1[i,]$MIN3 = predict(fit23min,newdata=data1[i,])
			data1[i,]$STDEV3 = predict(fit23sd,newdata=data1[i,])
			data1[i,]$YEAR3 = data1[i,]$YEAR2 + 1
		}
		if(data1[i,]$YIELD1 == 0 && data1[i,]$YIELD2 > 0){
			data1[i,]$YIELD1 = predict(fit21,newdata=data1[i,])
			data1[i,]$MAX1 = predict(fit21max,newdata=data1[i,])
			data1[i,]$MIN1 = predict(fit21min,newdata=data1[i,])
			data1[i,]$STDEV1 = predict(fit21sd,newdata=data1[i,])
			data1[i,]$YEAR1 = data1[i,]$YEAR2 - 1
		}
		if(data1[i,]$YIELD2 == 0 && data1[i,]$YIELD3 > 0){
			data1[i,]$YIELD2 = predict(fit32,newdata=data1[i,])
			data1[i,]$MAX2 = predict(fit32max,newdata=data1[i,])
			data1[i,]$MIN2 = predict(fit32min,newdata=data1[i,])
			data1[i,]$STDEV2 = predict(fit32sd,newdata=data1[i,])
			data1[i,]$YEAR2 = data1[i,]$YEAR3 - 1
		}
	}
	return(data1)
}