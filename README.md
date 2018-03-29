# school-district_dsi-intern
Project at DSI UC Davis  
Extracting target text from PDF files with specific structure.

# Using Packages
XML  
ReadPDF

# Install Packages
Install above two packages from github.
XML: 
```
git clone https://github.com/omegahat/XML.git \
	&& cd XML \
	&& R CMD INSTALL .
```
ReadPDF:
```
git clone https://github.com/dsidavis/ReadPDF.git \
	&& cd ReadPDF \
	&& R CMD INSTALL .
```

# XML
rects are showing as [x0, y0, x1, y1]

# TODO
1. ploting the extracted coordinates (well done)
2. extracting text by subsection names (may be able to do this by locations of characters.
3. One characteristic that our xmls do not share the ExtractTabularData python file is that our pdf parser gets words by sentence instead of characters. We my take an advantage of it that we do not need to group them by ourselves.
