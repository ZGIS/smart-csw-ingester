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

Don't use `publishSite` from sbt-site, but the `ghpagesPushSite` from sbt-ghpages

```scala
sbt makeSite
sbt previewSite
```

### sbt-ghpages

```scala
sbt ghpagesPushSite
```

### sbt-dependency-graph

```scala
sbt
```

### sbt-dependency-check

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
