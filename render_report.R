


box::use(pdftools[...])

# Paths to your PDF files
pdf_files = c("02_report_gen/resources/cover.pdf",
              "02_report_gen/resources/pre.pdf",
              "02_report_gen/report_gen.pdf",
              "02_report_gen/resources/backcover.pdf")

# Output file path
output_pdf = paste0("report_scenario", Sys.Date(), ".pdf")

# Combine the PDF files
pdf_combine(pdf_files, output = output_pdf)

