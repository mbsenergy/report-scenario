# Report-scenario

The repository is divided in 3 sections:

-   `01_preparation`: plots and figure preparation
-   `02_report_gen`: text and visuals combination
-   `03_output`: final file

## How to use

In a nutshell the workflow shall go like this:

### Figures preparation

1.  Paste the updated `excel` file called `"Grafici report scenari.xlsx"`\
2.  Open `01_plot_producer.R` located in `01_preparation` and update `report_num = 'IV2023'` with the current version\
3.  Run the file. The figures will be saved in `02_report_gen/figs`
4.  Eventually, adjust the plot size iteratively when need to fit the report

### Report developing

5.  Open `report_gen.rmd` located in `02_report_gen`\

6.  Update the title in the `yaml` header.\

7.  Write the comments and update the text as needed

    \> **Optional**: in you render the file with the **knit** button you can preview the body content as to be the built. The file will be placed in the same directory and called `report_gen.pdf` and also the `.html` version will be available

### Rendering

8.  Paste the new `cover.pdf`in `02_report_gen/resources`
9.  Open in the main directory the file `render_report.R`
10. Run the file. The output will be placed in `03_output` as `report_scenarioYYYY-MM-DD.pdf`

## Memo

Remember to push the changes to the main repository to keep it updated.
