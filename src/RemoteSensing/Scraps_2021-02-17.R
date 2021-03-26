x = read.csv('./data/processed/rs_annual_2013-19.csv', stringsAsFactors=F)

y = read.csv('./data/processed/rs_annual_dateCorrected_2013-19.csv', stringsAsFactors=F)

x = x[!is.na(x$B2),]

sum(x$ss == y$ss)

dev.off()
plot(as.matrix(x[,16:26]), as.matrix(y[,16:26]))

x=read.csv('./data/processed/BRT_habitat_predictions_best_2013-19_old.csv', stringsAsFactors=F)
y=read.csv('./data/processed/BRT_habitat_predictions_best_2013-19.csv', stringsAsFactors=F)
z=read.csv('./data/processed/BRT_habitat_predictions_best_2013-19b.csv', stringsAsFactors=F)

#x and y are very different!
plot(as.matrix(x[,paste0('pred', 2013:2019)]), as.matrix(y[,paste0('pred', 2013:2019)]))

#But the correlation is stronger when the predictions are averaged.
plot(apply(x[,paste0('pred', 2013:2019)],1,mean), apply(y[,paste0('pred', 2013:2019)],1,mean))


plot(as.matrix(brt[,9:14]), as.matrix(brt2[,9:14]))
plot(as.matrix(z[,paste0('pred', 2013:2019)]), as.matrix(y[,paste0('pred', 2013:2019)])) #Best and rand are very strongly correlated.
cor.test(as.matrix(x[,paste0('pred', 2013:2019)]), as.matrix(y[,paste0('pred', 2013:2019)]))

boxplot(apply(y[,paste0('pred', 2013:2019)],1,mean) ~ x$occupied)
boxplot(apply(x[,paste0('pred', 2013:2019)],1,mean) ~ x$occupied)
