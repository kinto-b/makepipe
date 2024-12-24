# makepipe 0.2.2

* Added a `build` argument to `make_*` family to allow a Pipeline to be built without being executed. Once the pipeline has been fully 'registered' it can be built via

```r
p <- get_pipeline()
p$build()
```

* Added a `touch()` method to `Pipeline` which updates the file modification times of all targets to the current time. This is useful when you know your targets are all up-to-date but makepipe doesn't, for example after a negligible change was made to your source code. The new `build` argument described above comes in handy here: setting `build=FALSE` you can construct the make 'graph' without executing it, then call 

```r
p <- get_pipeline()
p$touch()
```

This will update the times for all the targets in the pipeline so that when you call `p$build()` nothing will happen (unless one of your targets is set to `force=TRUE`).


* Upgraded the internal `webshot` dependency to `webshot2`, which doesn't depend on the unmaintained `PhantomJS`. 

# makepipe 0.2.1

* You can now specify the targets and dependencies (and so on) for each source script in your pipeline **within** that script using roxygen tags like this,

```r
#'@title Load
#'@description Clean raw survey data and do derivations
#'@dependencies "data/raw.Rds", "lookup/concordance.csv"
#'@targets "data/1 data.Rds"
#'@makepipe
NULL
```

The entire pipeline can then be executed in one fell-swoop with `make_with_dir()`, simply by passing through the directory in which the scripts are located. Alternatively, you can construct your pipeline piece by piece using `make_with_roxy()`. For example, using the tags above, you would have

```r
# This:
make_with_roxy("load.R")

# Instead of this:
make_with_source(
  "load.R", 
  targets = "data/1 data.Rds", 
  dependencies = c("data/raw.Rds", "lookup/concordance.csv"),
  label = "Load",
  note = "Clean raw survey data and do derivations"
)
```

* You can now produce a plain text summary of your pipeline using `show_pipeline(as = "text")`. You can also save this using, e.g., `save_pipeline(file = "pipeline.md", as = "text")`.

# makepipe 0.2.0

* BREAKING CHANGE: Deprecated `Pipeline$save()` and changed the arguments for `save_pipeline()`, `show_pipeline()`, and `Pipeline$annotate()`. This change was made to support the next change which is:

* Added support for `nomnoml` flowcharts:
  + By default, `show_pipeline()` will now display a `nomnoml` style flowchart. You can reproduce the old `visNetwork` style chart using `show_pipeline(as="visnetwork")`. These are powered by two new methods belonging to the `Pipeline` class: `$nomnoml()` and `$visnetwork()`.
  + By default, `save_pipeline()` will now save an image of the `nomnoml` chart. You can export the `visNetwork` chart as an html file using `save_pipeline(as="visnetwork")`. These are powered by two new methods belonging to the `Pipeline` class, which replace the deprecated `$save()` method: `$save_nomnoml()` and `$save_visnetwork()`.

* Added `note` and `label` arguments to `make_*()`. This allows makepipe code to be even more self documenting, since notes that might be left as comments above the `make_*()` segment can now be incorporated into the pipeline itself. 

* `make_with_*()` no longer throws an error if `dependencies` don't exist. It will be left to the users script/code to handle this situation.

* Fixed a bug which prevented `make_with_recipe()` from accepting long (>10 line) recipes

* Added `reset_pipeline()`, a wrapper for `set_pipeline(Pipeline$new())`, for resetting the active pipeline

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
