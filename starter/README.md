# A separate Datomic Local project

This Cabal package depends on `connector`; it owns its schema, initial identities
and typed `Starter.Persistence` module. Java 21 and either Clojure CLI or prepared
dependency jars are required. It uses the portable installed library entry point.

From this directory:

```sh
cabal build all
cabal run starter -- init /absolute/new/storage notes-system notes
cabal run starter -- demo /absolute/new/storage notes-system notes
cabal test starter-integration --test-show-details=direct
```

Storage, system and database are explicit arguments. `Starter.Config.configure`
selects the runtime using `DATOMIC_CONNECTOR_CLASSPATH`, `DATOMIC_CONNECTOR_JAVA`/
`JAVA_HOME` and `DATOMIC_CONNECTOR_CLOJURE`. Alternatively construct `LocalConfig`
with an explicit `JavaClasspath` or `ClojureCLI` in your configuration loader.
Run `../scripts/package-runtime /absolute/new/runtime` once to prepare offline
jars; set `DATOMIC_CONNECTOR_CLASSPATH=/absolute/new/runtime/lib/*` for deployment.
Dependency preparation needs network; Java-only execution does not.

For a new project, copy this starter package (not the connector sources), rename
its package/modules/schema attributes and choose initial identities. Here,
`cabal.project` selects `..` as the connector dependency. In a separate repository,
replace that entry with a published package, local checkout path, or pinned
source-repository-package. Keep the connector in `build-depends`. Provision once.

For an existing application, add the connector dependency and a persistence
module beside existing code. Resolve your actor and application entity IDs into
`WriteContext`; these example names are project choices. Supply runtime/location
through your existing config. For an existing database, do not run `init`: add
schema and identities through attributed transactions using established responsible
identities. Adopting an existing unattributed database needs an explicit
project-owned operational policy; provisioning intentionally refuses to update it.

Integration tests create and remove isolated temporary storage; they never use
the CLI database or first-contact experiment. Initialization may partially complete
if an initial file fails: inspect and recover explicitly rather than retrying it.
