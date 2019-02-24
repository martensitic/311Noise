# 311Noise
Data science project on NYC noise complaints.

## Files included
MLnoise.R:  assembly of data sets and training of the machine learining model.  Saves data and output for report generation.  Be aware that the data download takes some minutes, and the training takes some minutes more.

report.rmd: Generates report from the data sets saved by MLnoise.R.  Will not work if you don't run MLnoise.R first.

report.pdf: the generated report in PDF format.

index.html: the generated report in HTML, which is also hosted at https://martensitic.github.io/311Noise/ and is perhaps nicer looking than the PDF.

/resource: contains supplementary data called by MLnoise.R (downloaded automatically within the .R files, no need to save)
