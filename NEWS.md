# makepipe (development version)

* Updated README with a link to a minimal example of a `makepipe` pipeline. 
* Fixed a minor bug in `make_register()` (#22). Previously, an error would be thrown
when using `make_register()` outside of a `makepipe` segment if the user set 
`quiet=TRUE`. This is no longer the case.

# makepipe 0.0.5

* Fixed minor bug having to do with the propagation of out-of-dateness in
Pipeline visualizations.

# makepipe 0.0.4

* Renamed package from `piper` to `makepipe` (#11).

* Both `make_with_recipe()` and `make_with_source()` now return an object of class `makepipe_result` (#2, #14). Along with metadata relating to the execution, this object contains a `result` element which:
  * If using `make_with_recipe()`, is identical with whatever the `recipe`
  returns
  * If using `make_with_source()`, is a list containing any objects registered
  in the source script using `make_register()`

* The execution environment for `make_*()` is now, by default, a fresh
environment whose parent is the calling environment. This can be overridden
using the `envir` argument.


# makepipe 0.0.3

* Added arguments to `save_pipeline()` and `show_pipeline()`, allowing the user to specify e.g. height and width of output widgets. 

* Added `packages` argument to `make_*()` functions, allowing the user to easily add packages as dependencies of their targets. If any of the `packages` specified have been updated since the `targets` were last produced, the `targets` will be registered as out-of-date when `make_*()` is run. 

# makepipe 0.0.2

* Pipeline visualisations are now sorted hierarchically, from left to right across the screen.
* Fixed bug in pipeline invalidation (#4).
* Added a `NEWS.md` file to track changes to the package.

# makepipe 0.0.1

* Initial release.
