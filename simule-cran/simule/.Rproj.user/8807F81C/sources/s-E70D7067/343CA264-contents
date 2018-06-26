library(simule)

graphics.off()
par(ask=F)
par(mfrow=c(1,1))

data(ABIDE_aal116_timeseries)
label = colnames(ABIDE_aal116_timeseries[[1]])

data(aal116coordinates)
layout = cbind(aal116coordinates$x.mni + 90,aal116coordinates$y.mni+126, aal116coordinates$z.mni+72)

result = simule(ABIDE_aal116_timeseries, 0.2, 1, covType = "cov", TRUE)

### obtain graph for creating layout
#graph = returngraph.simule(result)

### create a fixed layout on cancergraph for plotting
#braintemplate <- neurobase::readnii(system.file("MNI152_T1_1mm_brain.nii.gz", package = "brainR"),reorient = FALSE)

color = as.integer(aal116coordinates$lobe)
simule::plotbrain.simule(result, type = "task", neighbouroption = "task",
                         subID = NULL, index = NULL, layout = layout, color = color, label = label)

simule::plotbrain.simule(result, type = "share", neighbouroption = "task",
                         subID = NULL, index = NULL, layout = layout, color = color, label = label)

simule::plotbrain.simule(result, type = "taskspecific", neighbouroption = "task",
                         subID = 1, index = NULL, layout = layout, color = color, label = label)

simule::plotbrain.simule(result, type = "taskspecific", neighbouroption = "task",
                         subID = 2, index = NULL, layout = layout, color = color, label = label)
