# tastypie 0.1.0

* Added the `pie_addimages` function: you can now easily **add images** to the slices of the pie charts created using `pie_bake`!
* Added the `bubble_blow` function: you can now also easily create cool **circular packing charts** with `{tastypie}`!
* Added 10 more templates for the `pie_bake_pro` function related to the **circular barplot chart** and to the **spider** (or **radar**) **chart**: `'cirbar1', 'cirbar2', 'cirbar3', 'cirbar4', 'cirbar5', 'spider1', 'spider2', 'spider3', 'spider4', spider5'` (added to `pie_template_list_pro`, the list of the available templates for this function).
* Added to the package site the article related to the **tasty_tshirts**, the super cool t-shirts with the `{tastypie}` logo (and removed the vignette about the users favorite templates from the R package documentation).

# tastypie 0.0.3

* Now `{tastypie}` is `tibble`-friendly: the user can also provide a `tibble` instead of a `dataframe` for the available functions. (#1)
* Improved the documentations of `pie_bake` and `pie_bake_pro`(added useful information about the labels and the `tibble` input).
* Solved the CRAN issue related to the importing of the `magrittr` package that is not actually used.

# tastypie 0.0.2 - NOW AVAILABLE ON CRAN!

* Added the description of the function results in the documentations of `pie_bake`, `pie_bake_pro`, `pie_templates` and `pie_discover`.
* Solved some issues regarding the examples of the `pie_datacheck` function.

# tastypie 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Fixed package documentation (after spell_check).
* Added a package site.
* Fixed badges in `README`.
