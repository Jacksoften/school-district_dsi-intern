# lets use xml2 first, then switch to XML if necessary

library(xml2)

# take one xml file as our example
file = "xml/BearValleyUnified_LCAP_2015.2016.xml"

tol = c(5,5)

xml = read_xml(file)

# first = xml_find_all(xml, sprintf("//page[(text[contains(normalize-space(text()), '%s')]))]", "Related State and/or Local"))

match = sprintf("//page[
		(
		 text[contains(normalize-space(text()), '%s')
		      and not(text[contains(normalize-space(text()),'%s')]][1]/@number"
			      ,"Related State and/or Local"
			      , "Identify the state and/or Local"
			      , "Related State and /or Local"
		 )
