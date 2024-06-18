# Install pdftools package if not already installed
library(pdftools)

# Paths to your PDF files
pdf_files <- c("report_gen/resources/cover.pdf",
               "report_gen/resources/pre.pdf",
               "report_gen/report_gen.pdf",
               "report_gen/resources/backcover.pdf")

# Output file path
output_pdf <- "report_scenario_final.pdf"

# Combine the PDF files
pdf_combine(pdf_files, output = output_pdf)
