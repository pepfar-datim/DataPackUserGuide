**Repo Owner:** Scott Jackson [@jacksonsj](https://github.com/jacksonsj)

# DataPackUserGuide

A [bookdown](https://bookdown.org/) book about
the PEPFAR COP Data Pack tool, including the Excel tool and the
R Shiny Self-Service App.

View the book online at <https://apps.datim.org/datapack-userguide/>.

# How this bookdown site is assembled
This book is configured using the `_bookdown.yml`, `_output.yml`, and
`_render.R` files, as well as certain information in the `index.Rmd` file.

Each chapter of the book is given a number and a title for the file and should
be ordered appropriately to ensure the book builds properly.

Reference tables in the User Guide are rendered from the Data Pack schema file
found in the `datapackr` package, with files to clean and format these tables in
the `R/schema.R`, `R/tables.R`, and `R/utilities.R` files.

Deployments are done via RStudio Connect and should always be done such that
the "Publish document with source code" option is chosen so that the bookdown site
is assembled on the server. Also ensure that updates via Git trigger
a rebuilding of the site.
See: https://docs.rstudio.com/connect/user/publishing/#publish-source-code.



# TODOs
The following activities should be undertaken to improve the quality
and performance of this bookdown site:
* Bring in more information from the Excel template into the schema
in `unPackSchema` in `datapackr` so that more information is revealed
for usage here. e.g. Green vs. grey columns.
* The `Enter or Modify Data?` column calculation is currently broken. This
could be fixed using the above "Green vs. Grey" column information.
* There is an outline in some commented-out code in the `utilities.R` file
showing how code chunks might be embedded within one another to further
simplify how tables are build in the Rmd files. Worth investigating.
* Rename image files with their chapter, figure number, and a descriptive title for easier maintenance
