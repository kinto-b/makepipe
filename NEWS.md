# makepipe (development version)

# makepipe 0.0.3

* Added arguments to `save_pipeline()` and `show_pipeline()`, allowing the user to specify e.g. height and width of output widgets. 

* Added `packages` argument to `make_*()` functions, allowing the user to easily add packages as dependencies of their targets. If any of the `packages` specified have been updated since the `targets` were last produced, the `targets` will be registered as out-of-date when `make_*()` is run. 

# makepipe 0.0.2

* Pipeline visualisations are now sorted hierarchically, from left to right across the screen.
* Fixed bug in pipeline invalidation (#4).
* Added a `NEWS.md` file to track changes to the package.

# makepipe 0.0.1

* Initial release.
