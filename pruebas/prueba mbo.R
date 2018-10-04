library(mlrMBO)
library(ggplot2)



obj.fun = makeBraninFunction()
# visualize the function
autoplot(obj.fun, render.levels = TRUE, show.optimum = TRUE)

ctrl = makeMBOControl(propose.points = 4)
ctrl = setMBOControlInfill(ctrl, crit = crit.ei)
ctrl = setMBOControlMultiPoint(ctrl, method = "cl", cl.lie = min)

ctrl = setMBOControlTermination(ctrl, iters = 30)
#ctrl = setMBOControlTermination(ctrl, max.evals = 20) # for the choosen settings will result in the same number of evaluations.

# Kriging can create a lot of console output, which we want tu surpress here:
configureMlr(on.learner.warning = "quiet", show.learner.output = FALSE)

#library(parallelMap)
#parallelStartMulticore(cpus = 2, show.info = TRUE)
#parallelStar
res = mbo(obj.fun, control = ctrl, show.info = TRUE)

autoplot(obj.fun, render.levels = TRUE, show.optimum = TRUE) + geom_text(data = as.data.frame(res$opt.path), mapping = aes(label = dob), color = "white")

#parallelStop()