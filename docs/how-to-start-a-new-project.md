# How to integrate the connector into a new project

Start with the independent starter package. It already demonstrates explicit
configuration, project-owned schema, attributed writes, reads, and migrations.
You can replace the example note model as your application grows.

| The connector provides | Your application owns |
| --- | --- |
| Starting and communicating with Datomic Local | Database location and runtime configuration |
| Queries, pulls, transactions, and snapshots | Schema and application data types |
| Requiring actor/application attribution | The identities representing your actor and application |
| Typed errors and transaction reports | Migration files and recovery decisions |

## 1. Copy the starter

These commands assume the connector checkout is at
`/Users/bjoern/code/connector`, as in this workspace. Replace paths if needed.
Use a new, empty application directory. The shell examples work in Bash or Zsh.

```sh
mkdir -p /Users/bjoern/code/my-app
cd /Users/bjoern/code/my-app

cp -R /Users/bjoern/code/connector/starter/{app,src,db,migrations} .
cp /Users/bjoern/code/connector/starter/{connector-starter.cabal,LICENSE,README.md} .
```

This copies the example application without its build artifacts. It keeps the
package name `connector-starter` and executable name `starter` initially; you can
rename them later. The connector implementation stays in its own package.

## 2. Add the connector dependency

Create `/Users/bjoern/code/my-app/cabal.project` containing:

```cabal
packages:
  .
  /Users/bjoern/code/connector
```

The starter's Cabal file already declares `connector` in `build-depends`.
The project file tells Cabal where to obtain that dependency. A local checkout
works without requiring a published Hackage release. Keep that checkout available
for builds; your application depends on its library rather than copying its code.

Use the tested GHC 9.6.7 toolchain and Cabal. Dependency preparation may need
network access on your first build.

## 3. Prepare and select the Java runtime

Prepare the JVM dependency jars once, using a new destination directory:

```sh
cd /Users/bjoern/code/connector
scripts/package-runtime /Users/bjoern/code/datomic-runtime
```

The builder uses already resolved dependencies when available, or Clojure CLI
to resolve the pinned dependencies. On a fresh machine, install Java 21 and
Clojure CLI first; see the connector's [setup instructions](../README.md#setup-and-run-native-macos).
On macOS, `scripts/install-local` can install them into the connector checkout.

Configure the shell from which you will run your application:

```sh
export DATOMIC_CONNECTOR_JAVA=/Users/bjoern/code/connector/.tools/jdk/bin/java
export DATOMIC_CONNECTOR_CLASSPATH='/Users/bjoern/code/datomic-runtime/lib/*'
```

The Java path above uses the workspace-local installation. Replace it with another
Java 21 executable if that is what you installed. Quote the classpath wildcard.
Repeat these exports in new shells, or put the configuration in your own launcher.

The prepared runtime directory supplies dependency jars. The connector library
locates its matching bridge source through Cabal's installed resources. Once
prepared, this Java-only configuration needs no Clojure CLI or dependency downloads
when starting the application.

## 4. Build, initialize, and run

```sh
cd /Users/bjoern/code/my-app
cabal build all

cabal run starter -- init /Users/bjoern/code/my-app/data my-app development
cabal run starter -- demo /Users/bjoern/code/my-app/data my-app development
```

The three configuration arguments are the storage directory, Datomic Local system
name, and database name. Keep them consistent between commands.

`init` creates a new database and installs the connector's attribution schema,
your project's schema, initial identities, and migration bookkeeping. Run it once
per database. It refuses an existing database. Initialization across files can
partially complete if a later file fails; inspect and recover explicitly instead
of blindly retrying initialization.

`demo` resolves the project's write context, writes a note, and reads it back.
The database remains on disk when the application exits.

## 5. Run the integration tests

```sh
cabal test starter-integration --test-show-details=direct
```

The tests create and remove their own temporary storage. They do not use the
`development` database from the commands above. They cover persistence,
attribution, and migration behavior, including concurrent runners.

## 6. Adapt the application

| File | What to change |
| --- | --- |
| `db/schema.edn` | Define your application's attributes instead of the example note model. |
| `db/identities.edn` | Choose the initial actor and application identities. |
| `src/Starter/Config.hs` | Connect runtime and database settings to your configuration loader. |
| `src/Starter/Persistence.hs` | Define your Haskell types and persistence functions. |
| `app/Main.hs` | Replace the demonstration flow with your application entry point. |
| `migrations/` | Add schema/data changes after initialization. |

If you rename the identity identifiers in `db/identities.edn`, update
`resolveContext` in the persistence module too. These names belong to your project;
the connector does not require the starter's identity names.

The starter's application flow uses these functions:

```haskell
import Datomic.Connector
import Starter.Config
import Starter.Persistence

saveExampleNote :: FilePath -> String -> String -> IO ()
saveExampleNote storage system database = do
  config <- configure storage system database
  withLocalConnector config $ \connector -> do
    context <- resolveContext connector
    _ <- saveNote connector context "hello" "My first saved note"
    findNote connector "hello" >>= print
```

`configure` produces a `LocalConfig`. `withLocalConnector` opens the connection
scope and owns the bridge JVM. `resolveContext` finds the numeric IDs of your
actor and application entities and creates a `WriteContext`. Every normal write
requires that context.

For a long-running application, open one connector at startup and share it among
your application threads. Finish those threads before leaving its scope. Use
`withSnapshot` when several reads need to observe the same database basis.

## 7. Apply changes after initialization

Add immutable migration files containing EDN transaction vectors, then run the
ordered list against your existing database:

```sh
cabal run starter -- migrate /Users/bjoern/code/my-app/data my-app development --migration 001-note-category migrations/001-note-category.edn
```

Repeat runs with the same migration IDs and checksums are harmless. Do not edit
an applied file; add another migration. The CLI uses the supplied file path as
its migration ID, so keep the path spelling stable. See the starter's
[migration documentation](../starter/README.md#attributed-migrations) for atomicity,
concurrency, and recovery details.

Do not automatically retry a cancelled write or `WriteOutcomeUnknown`: the write
may already have committed. Reconnect and reconcile its outcome first. Catch
`ConnectorError` for connector failures; an `IOException` handler alone will not
catch them.

For adoption into an existing application or database, see the
[starter guidance](../starter/README.md). Provisioning is a new-database operation;
updates to existing databases go through attributed transactions.
