install.packages("pdftools")
install.packages("pagedown")

# Packages
box::use(pagedown[...])
box::use(pdftools[...])

## Path to rmarkdown
rmd_file = file.path('02_report_gen', 'report_gen.rmd')

# Render the R Markdown file to PDF
chrome_print(rmd_file)



# Combine cover-pre-REPORT-backcover
pdf_files = c(file.path('02_report_gen', 'resources', 'cover.pdf'),
              file.path('02_report_gen', 'resources', 'pre.pdf'),
              file.path('02_report_gen', "report_gen.pdf"),
              file.path('02_report_gen', 'resources', 'backcover.pdf')
              )

# Output file path
output_pdf = file.path('03_output', paste0("report_scenario", Sys.Date(), ".pdf"))

# Combine the PDF files
pdf_combine(pdf_files, output = output_pdf)

# PDF READY
