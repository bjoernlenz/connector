"""Exercise generic provisioning through either installed CLI or bundled runtime."""
from pathlib import Path
import fcntl
import subprocess
import sys
import tempfile

bridge = str(Path(sys.argv[1]).resolve())
with tempfile.TemporaryDirectory(prefix="connector-provision-") as temporary:
    work = Path(temporary)
    storage = str(work / "data")

    def run(mode, database, files=(), request=None, success=True):
        result = subprocess.run([bridge, mode, storage, "project", database, *files],
                                input=request, text=True, capture_output=True, cwd=work, timeout=60)
        assert (result.returncode == 0) == success, result
        return result.stdout if success else result.stderr

    (work / "invalid.edn").write_text("{}")
    (work / "schema.edn").write_text('''[{:db/ident :app/name :db/valueType :db.type/string
                                       :db/cardinality :db.cardinality/one}]''')
    (work / "identities.edn").write_text('''[{:db/ident :identity/alice :app/name "Alice"}
                                           {:db/ident :identity/project :app/name "Project"}]''')
    # Parsing failure must precede database creation, so the same name remains usable.
    assert "transaction vectors" in run("provision", "custom", ["invalid.edn"], success=False)
    result = run("provision", "custom", ["schema.edn", "identities.edn"])
    assert ':initial-transactions 3' in result, result
    checks = '''{:op :query :query [:find ?name :where [_ :app/name ?name]]}
{:op :query :query [:find ?e :where [?e :db/ident :test/key]]}
{:op :query :query [:find ?e :where [?e :db/ident :identity/bjoern]]}
{:op :pull :selector [:db/ident :db/valueType] :eid :write/actor}
{:op :query :query [:find (count ?tx) :where [?tx :db/txInstant]]}
'''
    before = run("serve", "custom", request=checks).splitlines()
    assert all(line.startswith("{:ok true,") for line in before), before
    assert '"Alice"' in before[0] and '"Project"' in before[0], before
    assert before[1:3] == ['{:ok true, :result []}'] * 2, before
    assert ':db/ident :write/actor' in before[3] and ':db.type/ref' in before[3], before
    assert "already exists" in run("provision", "custom", ["schema.edn"], success=False)
    after = run("serve", "custom", request=checks).splitlines()
    assert after[-1] == before[-1], "Reprovisioning committed a transaction"
    with (Path(storage) / ".connector-provision.lock").open("a") as held:
        fcntl.lockf(held, fcntl.LOCK_EX | fcntl.LOCK_NB)
        assert "already in progress" in run("provision", "core-only", success=False)
    assert ':initial-transactions 1' in run("provision", "core-only")
    core = run("serve", "core-only", request='''{:op :pull :selector [:db/ident] :eid :write/via}
{:op :query :query [:find ?e :where [?e :db/ident :identity/alice]]}
''').splitlines()
    assert ':db/ident :write/via' in core[0] and core[1] == '{:ok true, :result []}', core
print("PASS: project-owned provisioning, core-only setup, preflight, locking and existing-database refusal")
