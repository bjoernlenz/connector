# Haskell ↔ Datomic Local connector

A small Haskell library starts a Clojure/JVM subprocess, exchanges newline-delimited
EDN over stdin/stdout, and uses the synchronous Datomic Client API. Datomic Local
stores the database on disk. No server, Scotty dependency or application schema
exists in the connector.

Validated on Intel macOS with GHC 9.6.7 and Java 21. `scripts/acceptance` checks
process restart, write attribution, scalar codecs, cancellation, runtime relocation,
generic provisioning and refusal paths. Each run prints its retained database name.

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
scripts/bridge provision "$PWD/data" first-contact experiment experiment/schema.edn experiment/identities.edn
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
are supplied by Haskell. Ordinary reads use the latest database; `withSnapshot` pins a basis (see below). Transaction reports contain `:t`, `:tx`, `:tempids` and datom vectors
`[e a v tx added]`; live JVM database objects stay inside the bridge.

Haskell requires context at the type level. The bridge also checks that both
positive numeric IDs exist before transacting, then adds `:write/actor` and
`:write/via` to `"datomic.tx"`. Both attributes have type `:db.type/ref`.
Datomic supplies `:db/txInstant`. The caller cannot supply these three attributes
or the transaction tempid. Context identifies responsibility; it authenticates
nobody and does not restrict actors to Björn.

Provisioning is a separate operational command, available through the installed
CLI or the relocatable runtime:

```sh
datomic-bridge provision /absolute/data my-system my-db db/schema.edn db/identities.edn
# Or install only the connector's attribution schema:
datomic-bridge provision /absolute/data my-system empty-db
```

The connector installs only `:write/actor` and `:write/via`. Schema, identity names
and seed data belong to the consuming project, supplied as explicit EDN transaction
vector files, applied in order. Relative file and storage paths resolve against
the caller's directory. For a writable project, include its initial actor and
application entities in these files, then resolve their IDs with `pull`; the
connector assumes no identity names or application-specific identity schema.
All files are parsed and checked as vectors before database
creation. A storage-directory file lock serializes connector provisioning commands.
Provisioning refuses existing databases; subsequent schema/data changes
use ordinary attributed transactions. Initial transactions are unattributed because
the context entities do not exist yet. There is no provisioning operation or
arbitrary unattributed transaction function in the serving protocol.

Provisioning across multiple files is not atomic. If Datomic rejects a later file,
earlier initial transactions remain in the newly created database; inspect that
database and explicitly recover it rather than retrying provisioning blindly.
The `experiment/` files are optional example inputs, absent from the bridge runtime.

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
  32-bit floats use `Tagged "connector/float" (StringEDN "0.1")`, retaining their
  JVM type for equality queries (ordinary EDN floating literals are doubles).
  The float tag also accepts `"NaN"`, `"Infinity"` and `"-Infinity"`.
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
  failed calls raise typed `ConnectorError` exceptions (see below).
- Initial managed-workspace runs could not download Maven dependencies and used
  direct downloads of Datomic's flattened POM dependencies in
  `.cache/runtime` and `.cache/bridge-classpath` containing
  `.cache/runtime/*:bridge/src`. The launcher accepts a cached classpath; normal
  machines use the Clojure CLI with `deps.edn`. Caches, tools and databases are
  ignored by Git. A later clean Maven preparation and CLI startup check succeeded
  with network permission. Intel macOS was tested; Apple Silicon was not.

[Datomic Local setup and durability](https://docs.datomic.com/datomic-local.html),
[Client API](https://docs.datomic.com/client-api/datomic.client.api.html),
[transaction entity metadata](https://docs.datomic.com/reference/best.html),
[Clojure installation](https://clojure.org/guides/install_clojure).

## Typed failures (caller compatibility)

Catch `ConnectorError` (also exported from `Datomic.Connector`): `LocalValidation`
is rejected before sending; `DatomicRejected EDN` retains the complete bridge
class, message and exception data, including Datomic anomaly keys. It leaves the
connector usable. `StartupFailure` covers launcher/startup failures and version
mismatch. `ProtocolFailure` and `ConnectorClosed` indicate unusable connections.
`WriteOutcomeUnknown SomeException` retains the cause of a failed write exchange;
the transaction may have committed. Reconnect and reconcile before any retry.

This deliberately changes the exception API: callers catching `IOException` for
connector failures must catch `ConnectorError` instead. Unrelated filesystem IO can
still throw `IOException`. Asynchronous exceptions are rethrown unchanged, including
during writes: cancellation remains recognizable and may also leave a committed
write. Cleanup preserves the original exception. There are no automatic retries.

## Transaction reports

`transactReport :: Connector -> WriteContext -> EDN -> IO TransactionReport`
returns `transactionBasis :: Basis`, `transactionEntity :: EntityId`,
`transactionTempids :: [(TempId, EntityId)]` and `transactionDatoms :: [Datom]`.
A datom has typed entity/attribute/transaction IDs, an added flag and a lossless
EDN value (including tagged float32, decimal, bigint and tuple values). Resolve a
named tempid with `lookup (StringTempId "item") (transactionTempids report)`.
`Datomic.Report` also exports the pure `decodeTransactionReport` validator.

The existing `transact :: ... -> IO EDN` is retained for callers using raw EDN;
it now validates the same report before returning the original EDN. Migrate by
switching to `transactReport` and the record accessors. Invalid success reports
close the connector and raise `WriteOutcomeUnknown`; do not retry automatically.

## Consistent read scopes

```haskell
withSnapshot connector $ \snapshot -> do
  rows <- querySnapshot snapshot queryEDN inputs
  entity <- pullSnapshot snapshot selectorEDN entityEDN
  pure (rows, entity, snapshotBasis snapshot)
```

Each snapshot retains one immutable Datomic database value inside its owning JVM.
It observes the basis captured at acquisition while ordinary `query`/`pull` and
writes keep advancing. Multiple snapshots may coexist. The handle carries its
connector, so it cannot accidentally be sent to another connector. Use it only
inside `withSnapshot`: scope exit releases it, including on exceptions; escaped
handles fail with `LocalValidation`. A closed connector raises `ConnectorClosed`.
Bridge shutdown releases all retained values; unknown/released protocol tokens
are rejected without changing the connector. There is no time-based expiration.
Keep scopes short to bound memory use. Concurrent reads and release are serialized.
As with connector scopes, await child threads before leaving a scope.

See [the independent starter](starter/README.md) for a project-owned typed
persistence module, explicit configuration, initialization and adoption guidance.

## Distribution metadata

The connector and starter use the BSD Zero Clause license (`0BSD`); see LICENSE.
Source information follows this repository's configured GitHub remote. GHC 9.6.7
is the tested compiler. Maintainer identity is intentionally unspecified; `cabal
check` reports only the corresponding `no-maintainer` warning. No other compiler
or platform result is implied by the CI matrix until that job actually passes.

## Continuous integration

`.github/workflows/ci.yml` uses GitHub Actions (the configured source host) with
GHC 9.6.7, Java 21 and pinned Clojure CLI on Ubuntu x64, Intel macOS and ARM macOS.
Runner labels describe scheduled coverage, not locally verified platforms.
Dependency preparation is separate from the subsequent checks:

- `test/clojure-startup.py prepare STATE` resolves the pinned graph into a fresh,
  isolated Maven repository; `verify STATE` starts the CLI and serves reads with
  HTTP(S) proxies blocked. STATE must be a new absolute directory for preparation.
- Haskell dependency preparation builds both projects with tests enabled.
  Acceptance and starter integration then run with Cabal `--offline`.
- Acceptance includes live scalar/report/snapshot checks, process restart,
  cancellation, provisioning/refusals, and Java-only runtime relocation/checksums.
- `test/distribution.py` builds both source archives, installs their executables,
  hides the unpacked sources, removes resource overrides and source classpath
  entries, and verifies installed provisioning plus the starter write/read and
  migration flow from an unrelated directory.
- `scripts/check-metadata` rejects Cabal diagnostics except `no-maintainer`, which
  is intentionally left unset. CI does not publish packages or push changes.

To run the distribution check locally, prepare Cabal dependencies first and set
`DATOMIC_CONNECTOR_CLASSPATH` to absolute dependency jars (or a jar wildcard).
The Java-only checks do not invoke a dependency resolver. Cabal's offline option
and the CLI proxy check are not an OS-level network isolation guarantee.
