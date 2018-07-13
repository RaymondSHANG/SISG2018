means <- aggregate(chol ~  as.factor(rs4775401), cholesterol, mean)
ggplot(cholesterol, aes(x = as.factor(rs4775401),
                        y = chol, 
                        fill = as.factor(rs4775401))) + 
	geom_boxplot() + 
	stat_summary(fun.y=mean, colour="darkred", geom="point", 
	shape=18, size=3) 
	# +
	# geom_text(data = means, aes(label = chol, y = chol + 0.08))

means <- aggregate(chol ~  as.factor(APOE), cholesterol, mean)
ggplot(cholesterol, aes(x = as.factor(APOE),
                        y = chol, 
                        fill = as.factor(APOE))) + geom_boxplot() + 
   stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3)
