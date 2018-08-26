source("Get_Goal.R")
chosenfile = "../LCAP/xml/BearValleyUnified_LCAP_2015.2016.xml"
myxml = file_to_xml(chosenfile)
nodes = sec2_nodes(myxml)
attrs = lapply(nodes, get_attrs)
info = lapply(attrs, get_info)
rawgoals = lapply(info, get_goal)
goals = rawgoals[unlist(lapply(rawgoals, function(x) length(x) != 0))]
