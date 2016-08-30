## sbt plugins

### scalastyle-sbt

- is activated default in compile and test

```scala
sbt scalastyle
```

### sbt-scapegoat

- uses scapegoat version 1.2.0 and provides not only style but also static code analysis
- generates report under `src/site/scapegoat` and is so included in `ghpagesPushSite`

```scala
sbt scapegoat
```

### sbt-scoverage

- instrumentation is activated, but the HTML report needs to be manually generated
- the output parameter doesn't seem to be adjustable, so I added a copy task
- I think `coverage` needs to be run before/together with tests

```scala
sbt coverage test
sbt coverageReport
sbt copyCoverage
```

### sbt-site

- Don't use `publishSite` from sbt-site, but the `ghpagesPushSite` from sbt-ghpages.
- sbt-site in version 0.8.1 seems to come pre-packaged with `activator` and gets evicted from the newer version declared in `plugins.sbt`

- preps
```scala
sbt makeSite
sbt previewSite
```

### sbt-ghpages

- pusblish to gh-pages branch of the project (from what was generated from `sbt site` in `src/site` but including ScalaDocs Api)

```scala
sbt ghpagesPushSite
```

### sbt-dependency-check

- OWASP Dependency-Check and Vulnerability is an open source tool performing a best effort analysis of 3rd party dependencies.

```scala
sbt check
```

### sbt-dependency-graph

```scala
sbt
```


### sbt-native-packager

```scala
sbt
```

### sbt-release TODO

```scala
sbt
```
