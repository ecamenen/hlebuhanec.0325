library(devtools)

name <- "hlebuhanec.0325"

# available::available(name)
path <- paste0("~/bin/", name)
if (!file.exists(path)) {
    dir.create(file.path(path))
}
setwd(path)
create_package(path)

system(paste("git clone  https://github.com/ecamenen/packageStarter.git", name))
# Remove git remote in DESCRIPTION
system("rm -rf .git")
golem::set_golem_name(name)
use_description(
    fields = list(
        Title = "",
        `Authors@R` = c(
            person("Etienne", "Camenen", email = "etienne.camenen@gmail.com", role = c("aut", "cre")) #,
            #person("", "", role = "aut")
        ),
        Description = "",
        License = "GPL-3",
        Version = "0.1.0"
    )
)
document()
use_git()
use_git(message = "Update package configuration")
create_project(path)
system(paste("git clone  https://github.com/ecamenen/packageStarter.git", name))

use_readme_rmd()
use_cran_badge()
use_package_doc()
use_pipe()
# promptPackage(name)
for (f in c(".idea/", "dev/", "inst/extdata/")) {
    use_git_ignore(f)
    use_build_ignore(f)
}
# use_vignette("Tutorial")
use_testthat()
use_spell_check()
for (p in c(
    "GimmeMyPlot",
    "GimmeMyStats"
)) {
    use_package(p)
}
use_coverage()
use_github_actions()
use_github_actions_badge()
use_data_raw()
