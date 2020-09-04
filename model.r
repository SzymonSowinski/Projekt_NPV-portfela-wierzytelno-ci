library(data.table)
load("final_benchmark.RData") #benchmark

library(h2o)

h2o.init(nthreads = -1) 

h2o.removeAll() ## clean slate - just in case the cluster was already running


benchmark_trn <- as.h2o(benchmark[Set == 1,])
benchmark_val <- as.h2o(benchmark[Set == 2,])
benchmark_test <- as.h2o(benchmark[Set == 3,])

X <- setdiff(colnames(benchmark), c("PortfolioId", "NPV", "Set"))
Y <- "NPV"

#Neural Network for NPV

#hyperparameters

hyper_params <- list(hidden = list(10, 20, 50, 100, c(20,5), c(50, 20, 5)),
                     l1 = seq(1e-6,1e-3,1e-6),
                     l2 = seq(1e-6,1e-3,1e-6),
                     epochs = c(seq(5, 30, by = 5), 100, 500, 1000),
                     input_dropout_ratio = c(0.05, 0.1, 0.2, 0.3)
                     )

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 100,
                        max_runtime_secs = 10*60, #3 min
                        stopping_metric = "MAE",
                        stopping_tolerance = 0.001, #0.0001
                        stopping_rounds = 3,
                        seed = 1234)

#Grid search

NN_grid <- h2o.grid(algorithm = "deeplearning",
                    grid_id = "NN_NPV",
                    x = X,
                    y = Y,
                    training_frame = benchmark_trn,
                    validation_frame = benchmark_val,
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)

model_id <- h2o.getGrid(grid_id = "NN_NPV", sort_by = "MAE")@model_ids[[1]]
model_nn <- h2o.getModel(model_id)

h2o.rm(ids = setdiff(h2o.getGrid(grid_id = "NN_NPV", sort_by = "MAE")@model_ids, model_id))
#as.character(h2o.ls()$key)

#plot(h2o.getModel(model_id), metric = "MAE")

hidden <- h2o.getModel(model_id)@parameters$hidden
epochs <- h2o.getModel(model_id)@parameters$epochs
l1 <- h2o.getModel(model_id)@parameters$l1
l2 <- h2o.getModel(model_id)@parameters$l2
dropout <- h2o.getModel(model_id)@parameters$input_dropout_ratio

h2o.no_progress() #disable progress bar

models_nn <- data.table()
for(i in 1:20)
{#i <- 1
  print(paste("model", i, sep = " "))
  id <- paste("NN", i, sep = "_")
  NN <- h2o.deeplearning(x = X,
                         y = Y,
                         training_frame = benchmark_trn,
                         validation_frame = benchmark_val,
                         model_id = id,
                         hidden = hidden,
                         epochs = epochs, 
                         l1 = l1,
                         l2 = l2,
                         input_dropout_ratio = dropout,
                         stopping_metric = "MAE",
                         stopping_tolerance = 0.001,
                         stopping_rounds = 3,
                         seed = 1234)
  
  model <- h2o.getModel(id)
  
  pred <- h2o.predict(model, newdata = benchmark_val)
  dev <- mean(abs(pred - benchmark_val$NPV)/benchmark_val$NPV)
  
  models_nn <- rbind(models_nn,
                     data.table(model_id = id,
                                MAE_val = model@model$validation_metrics@metrics$mae,
                                dev_val = dev))
  
}

h2o.rm(ids = paste("NN", 1:20, sep = "_"))
h2o.rm(as.character(h2o.ls()$key)[as.character(h2o.ls()$key) %like% "transformation"])

rm(pred, dev, model, NN, id) #deleting

h2o.show_progress() #enable progress bar

library(ggplot2)

ggplot(models_nn, aes(x = dev_val, y = MAE_val)) +
  geom_point(size = 3, shape = 20, color = "navy") +
  labs(title = "Odchylenia na zbiorze walidacyjnym") +
  ylab("MAE") +
  xlab("dev") +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "lightgrey"))


rbind(models_nn[, .(metric = "dev", mean = round(mean(dev_val), 2), sd = round(sd(dev_val), 2))],
      models_nn[, .(metric = "MAE", mean = round(mean(MAE_val), 2), sd = round(sd(MAE_val), 2))])

#Prediction Error on training set

pred_tr <- h2o.predict(model_nn, newdata = benchmark_trn)
dev_tr <- abs(pred_tr - benchmark_trn$NPV)/benchmark_trn$NPV
mae_tr <-  model_nn@model$training_metrics@metrics$mae

#Prediction Error on validation set

pred_val <- h2o.predict(model_nn, newdata = benchmark_val)
dev_val <- abs(pred_val - benchmark_val$NPV)/benchmark_val$NPV
mae_val <-  model_nn@model$validation_metrics@metrics$mae

#Prediction Error on test set

pred_tst <- h2o.predict(model_nn, newdata = benchmark_test)
dev_tst <- abs(pred_tst - benchmark_test$NPV)/benchmark_test$NPV
mae_tst <- h2o.make_metrics(pred_tst, benchmark_test$NPV)@metrics$mae


h2o.rm(as.character(h2o.ls()$key)[as.character(h2o.ls()$key) %like% "transformation"])

#Variables importance

as.data.table(h2o.varimp(model_nn)[,c(1,4)])

#Regression tree for NPV

library(rpart)
library(rpart.plot)

grid <- expand.grid(maxdepth = 2:5, minsplit = c(10, 30, 50, 100))

trees <- data.table(minsplit = NULL, maxdepth = NULL, dev = NULL)

n_grid <- nrow(grid)

for (i in 1:n_grid) 
{
  
  minsplit <- grid$minsplit[i]
  maxdepth <- grid$maxdepth[i]
  
  tree <- rpart(NPV~.,
                data = benchmark[Set == 1, .SD, .SDcols = c(X, Y)],
                method = "anova",
                minsplit = minsplit,
                maxdepth = maxdepth, 
                model = TRUE,
                cp = 0.0001)
  prediction <- sapply(predict(tree,
                               newdata = benchmark[Set == 2, .SD, .SDcols = c(X, Y)],
                               type = "matrix"), function(x) max(0,x))
  
  dev_tree <- mean(abs(prediction - benchmark[Set == 2, NPV])/benchmark[Set == 2, NPV])
  
  trees <- rbind(trees, data.table(minsplit = minsplit,
                                   maxdepth = maxdepth,
                                   dev = dev_tree))
  
  
  
}

minsplit <- trees[order(dev)][1, minsplit]
maxdepth <- trees[order(dev)][1, maxdepth]

tree_NPV <- rpart(NPV~.,
                    data = benchmark[Set == 1, .SD, .SDcols = c(X, Y)],
                    method = "anova",
                    minsplit = minsplit,
                    maxdepth = maxdepth, 
                    model = TRUE,
                    cp = 0.0001)

rpart.plot(tree_NPV, type = 4, fallen.leaves = FALSE, varlen = 5)

#Prediction Error on training set

pred_tr_tree <- predict(tree_NPV, newdata = benchmark[Set == 1,], type = "matrix")
dev_tr_tree <- abs(pred_tr_tree - benchmark[Set == 1, NPV])/benchmark[Set == 1, NPV]
mae_tr_tree <- mean(abs(pred_tr_tree - benchmark[Set == 1, NPV]))

#Prediction Error on validation set

pred_val_tree <- predict(tree_NPV, newdata = benchmark[Set == 2,], type = "matrix")
dev_val_tree <- abs(pred_val_tree - benchmark[Set == 2, NPV])/benchmark[Set == 2, NPV]
mae_val_tree <- mean(abs(pred_val_tree - benchmark[Set == 2, NPV]))

#Prediction Error on test set

pred_tst_tree <- predict(tree_NPV, newdata = benchmark[Set == 3,], type = "matrix")
dev_tst_tree <- abs(pred_tst_tree - benchmark[Set == 3, NPV])/benchmark[Set == 3, NPV]
mae_tst_tree <- mean(abs(pred_tst_tree - benchmark[Set == 3, NPV]))

#Distribution of dev over portfolios for both models

library(ggplot2)

dev_tst <- as.vector(dev_tst)

df_dev_dens <- data.frame(model = c(rep("NN", length(dev_tst)),
                                    rep("RT", length(dev_tst_tree))),
                          dev = c(dev_tst, dev_tst_tree))

p <- ggplot(df_dev_dens, aes(x=dev, color=model)) +
  geom_density() +
  labs(color = "MODEL") +
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "lightgrey")) +
  scale_x_continuous(breaks = seq(0, 1.2, by = 0.2), limits = c(-0.1, 1.2)) +
  scale_y_continuous(breaks = seq(0, 6, by = 1.5))

plot(p)

#After using h2o

h2o.shutdown(prompt=FALSE)

#linear correlation

cor_variables <- setdiff(colnames(benchmark), c("PortfolioId", "Set"))

#for(v in cor_variables)
#{#v<-"Product"
#  eval(parse(text = paste("benchmark[, ", v, " := as.numeric(", v, ")]")))
#}

library(corrplot)
corrplot(cor(benchmark[, .SD, .SDcols = cor_variables],
             use = "pairwise.complete.obs", method = "pearson"),
         order = "original", tl.col = 'black', tl.cex = 0.75, type = "lower")
