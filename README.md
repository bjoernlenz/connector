# Haskell ↔ Datomic: first contact

A small Haskell library starts a Clojure/JVM subprocess, exchanges newline-delimited
EDN over stdin/stdout, and uses the synchronous Datomic Client API. Datomic Local
stores the database on disk. No server, Scotty dependency or application schema
exists in the connector.

Verified on Intel macOS on 2026-10-14: `scripts/acceptance` exited successfully.
The retained database is `data/first-contact/acceptance-20261014-183912-19764`;
transaction `13194139533321`, timestamp `2026-10-14T16:39:30.814-00:00`, actor
`79164837199951` and via `79164837199952` matched before and after process restart.

## Setup and run (native macOS)

Install GHC 9.6.7 and Cabal (for example with [GHCup](https://www.haskell.org/ghcup/)).
Then, from this checkout:

```sh
scripts/install-local          # workspace-local Java 21 and Clojure CLI
cabal build --build-summary=.cache/cabal-build.log all
scripts/acceptance
```

The installer supports Intel and Apple Silicon. Alternatively provide Java 21
and `clojure` on your PATH. `deps.edn` pins Clojure 1.12.4 and Datomic Local
1.0.301; the CLI downloads Datomic and its dependencies on first use. Python 3
and `rg` are used by the acceptance script. Acceptance uses `--offline` for the
Haskell build, so run the preceding build first on a fresh machine.

Each acceptance run creates a new durable database under
`data/first-contact/acceptance-*`. It installs the schema and identities, writes
and reads from Haskell, checks the transaction references and timestamp, exits
both processes, and starts another Haskell/JVM pair to verify the same data.
It also checks invalid/nonexistent identities, metadata overrides, protocol
writes without context, unchanged transaction count after refusal, and a Haskell
write without context that must fail to compile. Data is retained for inspection.

To use one named database manually:

```sh
scripts/bridge bootstrap "$PWD/data" first-contact experiment
cabal run --build-summary=.cache/cabal-build.log first-contact -- write "$PWD/data" first-contact experiment hello
cabal run --build-summary=.cache/cabal-build.log first-contact -- read  "$PWD/data" first-contact experiment hello
```

## Interface and attribution

The exposed library operations are:

```haskell
withConnector :: FilePath -> [String] -> (Connector -> IO a) -> IO a
withLocalConnector :: LocalConfig -> (Connector -> IO a) -> IO a
transact :: Connector -> WriteContext -> EDN -> IO EDN
query    :: Connector -> EDN -> [EDN] -> IO EDN
pull     :: Connector -> EDN -> EDN -> IO EDN

data WriteContext = WriteContext { actor :: EntityId, via :: EntityId }
```

For another project, add this package as a Cabal dependency and use the configured
entry point; it discovers its matching bridge through Cabal's installed data files:

```haskell
import Datomic.Connector
import Datomic.Runtime

-- The database must already have been provisioned.
withLocalConnector (defaultLocalConfig "/absolute/data" "my-system" "my-db") $ \c ->
  query c queryEDN inputs
```

The default runtime is `ClojureCLI "clojure"`. It uses bundled dependency pins,
ignores user Clojure configuration and runs in a writable cache directory, away
from the consuming project's `deps.edn`. Prepare dependencies before offline use.
For Java-only deployment set `bridgeRuntime = JavaClasspath "/path/to/java"
"/absolute/runtime/lib/*"`. The package's own bridge source is prepended to that
dependency classpath. Startup checks protocol and package versions, with a
configurable `startupTimeoutMicros` (60 seconds by default).

The `datomic-bridge` executable uses the same installed resources. Runtime selection
comes from `DATOMIC_CONNECTOR_CLASSPATH` (dependency jars), `DATOMIC_CONNECTOR_JAVA`
or `JAVA_HOME`, and `DATOMIC_CONNECTOR_CLOJURE`. The example executable also uses
these settings. Library callers choose explicitly, or call
`bridgeRuntimeFromEnvironment` to use that convention. `withConnector` remains the
lower-level entry point for a custom launcher.

Build a relocatable runtime directory before deployment:

```sh
scripts/package-runtime /tmp/connector-runtime
# Alternatively: scripts/package-runtime DEST --classpath '/resolved/jars/*'
# Move or copy the complete directory to the target machine, then:
/opt/connector-runtime/run serve /absolute/data my-system my-db
```

The directory contains the bridge, resolved jars, version, pinned dependency
declarations and SHA-256 checksums. It needs Java 21; it does not need a checkout,
Clojure CLI or downloads at startup. The builder uses an explicit classpath, the
environment, the development cache, or finally Clojure's dependency resolver.

See `experiment/Main.hs` for a complete caller. The EDN values are Datomic
transaction maps/vectors, Datalog queries, selectors and entity IDs/lookup refs.
`query` supplies the current database as the first input (`$`); additional inputs
are supplied by Haskell. Each call reads the current database, not a pinned
snapshot. Transaction reports contain `:t`, `:tx`, `:tempids` and datom vectors
`[e a v tx added]`; live JVM database objects stay inside the bridge.

Haskell requires context at the type level. The bridge also checks that both
positive numeric IDs exist before transacting, then adds `:write/actor` and
`:write/via` to `"datomic.tx"`. Both attributes have type `:db.type/ref`.
Datomic supplies `:db/txInstant`. The caller cannot supply these three attributes
or the transaction tempid. Context identifies responsibility; it authenticates
nobody and does not restrict actors to Björn.

Bootstrap is a separate process mode with two fixed experiment files. It creates
the attribution/test schema and the Björn/application entities. Bootstrap
transactions have no actor/via; there is no bootstrap operation or arbitrary
unattributed transaction function in the Haskell API or serving protocol.

## Observed limits

- One JVM owns a local database at a time. Calls on one Haskell connector are
  serialized. Closing its scope closes stdin, releases Datomic and waits for exit.
  Shutdown allows two seconds for graceful exit, then terminates the subprocess.
  Cancellation, EOF or malformed responses permanently close that connector;
  subsequent calls fail instead of consuming an abandoned response. A write whose
  response was lost may already have committed: reconnect and reconcile its outcome
  before retrying. A complete Datomic error response leaves the connector usable.
- EDN supports nil, booleans, signed 64-bit integers (`IntEDN`), arbitrary integers
  (`BigIntEDN`, `N` suffix), doubles (`FloatEDN`, including `##NaN`/`##Inf`/`##-Inf`),
  exact decimals (`DecimalEDN coefficient exponent`, value = coefficient × 10^exponent),
  strings, keywords, symbols, maps, lists, vectors, sets and tagged values.
  Decimal scale is preserved. Instants and UUIDs use `Tagged "inst"`/`Tagged "uuid"`
  with a `StringEDN`. URIs use `Tagged "connector/uri" (StringEDN uri)`;
  binary wire values use `Tagged "connector/bytes" (StringEDN base64)`. Datomic
  Local 1.0.301 does not provide a `:db.type/bytes` attribute type.
  Ratios, characters and namespaced-map syntax are deliberately unsupported;
  signed unsupported numbers fail parsing instead of becoming symbols. The JVM
  prints expanded maps. `encodeEDN` returns `Either String String` and validates
  outgoing values before any protocol bytes are written; `renderEDN` is the
  partial convenience function for known-valid literals. Invalid JVM tags return
  an ordinary bridge error. Unicode escape pairs decode to one character.
  Pulling an absent lookup reference returned `{:db/id nil}`, rather than an error.
- Ordinary transactions support entity maps and native add/retract/cas/retractEntity
  operations. User transaction functions are excluded in this experiment.
- This is a trusted local connector, not a security boundary. Someone directly
  accessing Datomic can bypass it. There are no request timeouts, retries or crash recovery;
  failed calls raise IO exceptions containing the bridge's EDN error response.
- In this managed workspace Java's Maven downloads were blocked. The demonstrated
  run used direct downloads of Datomic's flattened POM dependencies in
  `.cache/runtime` and `.cache/bridge-classpath` containing
  `.cache/runtime/*:bridge/src`. The launcher accepts a cached classpath; normal
  machines use the Clojure CLI with `deps.edn`. Caches, tools and databases are
  ignored by Git. Intel macOS was tested; Apple Silicon installation was not.

[Datomic Local setup and durability](https://docs.datomic.com/datomic-local.html),
[Client API](https://docs.datomic.com/client-api/datomic.client.api.html),
[transaction entity metadata](https://docs.datomic.com/reference/best.html),
[Clojure installation](https://clojure.org/guides/install_clojure).
