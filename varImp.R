# variable importance



a=models[[1]]
b=h2o.getModel(a@model$metalearner$name)
h2o.varimp(b)
h2o.varimp_plot(b)
v=h2o.getModel("GLM_grid_1_AutoML_20190224_091156_model_1")
h2o.varimp(v)

