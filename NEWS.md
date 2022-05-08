# makepipe (development version)

* BREAKING CHANGE: Deprecated `Pipeline$save()` and changed the arguments for `save_pipeline()`, `show_pipeline()`, and `Pipeline$annotate()`. This change was made to support the next change which is:

* Added support for `nomnoml` flowcharts:
  + By default, `show_pipeline()` will now display a `nomnoml` style flowchart. You can reproduce the old `visNetwork` style chart using `show_pipeline(as="visnetwork")`. These are powered by two new methods belonging to the `Pipeline` class: `$nomnoml()` and `$visnetwork()`.
  + By default, `save_pipeline()` will now save an image of the `nomnoml` chart. You can export the `visNetwork` chart as an html file using `save_pipeline(as="visnetwork")`. These are powered by two new methods belonging to the `Pipeline` class, which replace the deprecated `$save()` method: `$save_nomnoml()` and `$save_visnetwork()`.

* Added `note` and `label` arguments to `make_*()`. This allows makepipe code to be even more self documenting, since notes that might be left as comments above the `make_*()` segment can now be incorporated into the pipeline itself. 

* `make_with_*()` no longer throws an error if `dependencies` don't exist. It will be left to the users script/code to handle this situation.

# makepipe 0.1.0

## `make_*()`

* Made `dependencies` argument to `make_*()` optional (#25). 

* Added a `force` argument to `make_*()` (#26). This allows the user to override the default conditional execution logic to ensure that a given segment of the pipeline is run no matter what.

## Segment
* Implemented `Segment` class to serve as the basic building block for the `Pipeline` (#28). This clarifies the link between the fundamental `make_*()` functions and the `Pipeline` object. In particular, each `make_*()` call constructs a `Segment` which is appended to the `Pipeline`.
 + The `Segment` class has an `execute()` method, which replicates the execution behaviour of the `make_*()` functions. Hence `Segment$execute()` can be run to rebuild the targets associated with a given segment without adding a new segment to the `Pipeline` as would be done if we called `make_*()` again.
 + The `Segment` holds all execution metadata (e.g. execution result, executiontime, etc.). This is updated via the `Segment$update_result()` method whenever `Segment$execute()` is called.

* Added a `build()` method to the `Pipeline` class, which sorts the `Segment`s topologically and then calls `execute()` on each in turn. 

* Added a `clean()` method to the `Pipeline` class, which deletes the `targets` associated with each `Segment`.

* Removed the now redundant `makepipe_result` S3 class. All of the information that was held by the `makepipe_result` is now held by the `Segment`.

## Bug fixes

* Fixed a minor bug in `Pipeline$print()` which was causing user-supplied annotations to be overwritten (#23).

* Fixed a minor bug in `make_register()` (#22). Previously, an error would be thrown when using `make_register()` outside of a `makepipe` segment if the user set `quiet=TRUE`. This is no longer the case.

## Documentation

* Added a getting started vignette (#19).

# makepipe 0.0.5

* Fixed minor bug having to do with the propagation of out-of-dateness in Pipeline visualizations.

# makepipe 0.0.4

* Renamed package from `piper` to `makepipe` (#11).

* Both `make_with_recipe()` and `make_with_source()` now return an object of class `makepipe_result` (#2, #14). Along with metadata relating to the execution, this object contains a `result` element which:
  * If using `make_with_recipe()`, is identical with whatever the `recipe` returns
  * If using `make_with_source()`, is a list containing any objects registered in the source script using `make_register()`

* The execution environment for `make_*()` is now, by default, a fresh environment whose parent is the calling environment. This can be overridden using the `envir` argument.


# makepipe 0.0.3

* Added arguments to `save_pipeline()` and `show_pipeline()`, allowing the user to specify e.g. height and width of output widgets. 

* Added `packages` argument to `make_*()` functions, allowing the user to easily add packages as dependencies of their targets. If any of the `packages` specified have been updated since the `targets` were last produced, the `targets` will be registered as out-of-date when `make_*()` is run. 

# makepipe 0.0.2

* Pipeline visualisations are now sorted hierarchically, from left to right across the screen.
* Fixed bug in pipeline invalidation (#4).
* Added a `NEWS.md` file to track changes to the package.

# makepipe 0.0.1

* Initial release.
